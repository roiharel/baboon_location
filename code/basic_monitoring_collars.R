## code is running some basic visualizations of MBRP data collected since Mar 2024
## the input is based on connection to movebank 
## install packages
{
  # Set a CRAN mirror
options(repos = c(CRAN = "https://cloud.r-project.org"))

# Define a user-specific library path
user_lib <- Sys.getenv("R_LIBS_USER")
dir.create(user_lib, recursive = TRUE, showWarnings = FALSE)

# Install packages if they are not already installed
install_if_missing <- function(pkg) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    install.packages(pkg, lib = user_lib)
  }
}

# List of required packages
packages <- c(
  "move2", "ggplot2", "lubridate", "dplyr",  "ggmap", "maps",  "sf", "mapview", "leaflet", "leaflet.minicharts", "htmlwidgets",  "RColorBrewer", "units", "magrittr", "purrr", "plotly","gridExtra", "png", "grid", "DT", "htmlwidgets","keyring","lwgeom","rmarkdown", "geosphere","dbscan")

# Apply the function to each package
invisible(lapply(packages, install_if_missing))

# Load the libraries
lapply(packages, library, character.only = TRUE, lib.loc = user_lib)
}
## parameters - fill in details 
{
#movebank_store_credentials("USER", "PASSWORD", force = TRUE)
#ggmap::register_google(key = "KEY")

setwd("C:\\Users\\meerkat\\Documents\\MBRP\\")

time_interval_high <- "1 mins"
time_interval_low <- "1 hours" #time interval for plots
days_window <- 4 # window in days - max distance 

date_start <- as.POSIXct("2024-07-01 00:00:00")
speed_threshold <- set_units(10, "m/s")  # Replace "m/s" with the appropriate unit if needed
mark_old_downloads <- 21 # 21 days
possible_mortality <- c(10368,15484,14550,14542,6898) # Replace with actual names

}
## functions
{
# load data and basic cleaning
download_data <- function(date_start) {
  baboon_data <- movebank_download_study(3445611111, sensor_type_id = c("gps"), 
                                         timestamp_start = date_start,
                                         # individual_id = c(3487912671, 3487912662, 3487912663, 3487844492, 3508338112, 3938663627),
                                         remove_movebank_outliers = TRUE) %>%
    filter(!st_is_empty(.))     # remove empty rows
  return(baboon_data)
}

arrange_data <- function(baboon_data, time_interval, speed_threshold) {
    
  # calc speed azimuth and clean speed outliers
  baboon_data %<>% mutate(azimuth = mt_azimuth(.), speed = mt_speed(.))
  baboon_data$speed <- set_units(baboon_data$speed, "m/s")
  baboon_data <- baboon_data %>%
    filter(speed <= speed_threshold | is.na(speed))
  
  # add fields from metadata
  metadata <- mt_track_data(baboon_data)
  
  baboon_data <- baboon_data %>%
    left_join(metadata %>% 
                dplyr::select(individual_local_identifier, tag_local_identifier, group_id, sex), by = c("individual_local_identifier" = "individual_local_identifier"))  %>%
    mt_filter_per_interval(unit = time_interval)
  
  baboon_data$location.long <- sf::st_coordinates(baboon_data)[,1]
  baboon_data$location.lat <- sf::st_coordinates(baboon_data)[,2]
  baboon_data$group_id <- baboon_data$group_id
  
  
  # Identify matching columns
  matching_columns <- Reduce(intersect, list(names(baboon_data)))
  
  
  # Join tibbles while keeping only matching columns
  cleaned_data <- bind_rows(
    dplyr::select(as.data.frame(baboon_data), matching_columns))
  
  
  # cleaned_data <- cleaned_data %>%
  #   mutate(plot_name = paste(group_id, year(timestamp), sep = "_")) %>%
  #   filter(tag_local_identifier != 6915)
  
  cleaned_data <- cleaned_data %>%
    mutate(plot_name = paste(group_id, sep = "_")) %>%
    filter(tag_local_identifier != 6915)
  
  # filter(plot_name != "Campsite_2020")
  
  # Return the clustered data
  return(cleaned_data)
  
  # cleaned_data <- cleaned_data %>%
  #   filter(group_id %in% c("Mlimafisi", "Clifford", "Leikiji"))
  
}
# Function to round to the nearest specified value
round_to_nearest <- function(x, values) {
  values[which.min(abs(values - x))]
}
# Functions for coloring text in tables
mark_status_change <- function(status, battery, tag, max_last_days) {
  style <- ""
  
  # Check if the tag is in the possible mortality list
  if (tag %in% possible_mortality) {
    style <- paste(style, "color: red; text-decoration: line-through; font-weight: bold;")  # Red color and strikethrough for mortality
  } else {
    # Check for max_last_days less than 0.5
    if (max_last_days < 0.5) {
      style <- paste(style, "color: red; font-weight: bold;")  # Red color for max_last_days < 0.5
    } else {
      # Check for 'rest' status and battery
      if (status == "Rest" && battery > set_units(3950, "mV")) {
        style <- paste(style, "color: blue; font-weight: bold;")  # Blue color for non-mortality
      }
      
      # Check for 'monitor' status and battery
      if ((status == "Monitor" || status == "High") && battery < set_units(3700, "mV")) {
        style <- paste(style, "color: blue; font-weight: bold;")  # Blue color for non-mortality
      }
    }
  }
  
  # Return HTML string with the style
  return(paste("<span style='", style, "'>", tag, "</span>", sep = ""))
}

}
## load data and basic cleaning
baboon_data <- download_data(date_start)
cleaned_data <- arrange_data(baboon_data, time_interval_high, speed_threshold)

# ## plot data dist
{
recent_data <- cleaned_data[cleaned_data$timestamp > date_start,]
recent_data$tag_local_identifier <- with(recent_data, reorder(tag_local_identifier, group_id))

ordered_levels <- recent_data %>%
  arrange(group_id) %>%
  pull(tag_local_identifier) %>%
  unique()

recent_data$tag_local_identifier <- factor(recent_data$tag_local_identifier, levels = ordered_levels)

records <- ggplot(recent_data, 
                  aes(x = timestamp, 
                      y = eobs_battery_voltage,
                      color = tag_local_identifier)) +
  geom_point() +
  labs(x = "timestamp", y = "tagID") 
interactive_plot <- ggplotly(records, tooltip = "text")
# save plots

saveWidget(interactive_plot, paste0('plots/htmls/','/baboon_data_batt_plot.html')
           , selfcontained = FALSE)

## calculate median time difference, add group_id, and round it

daily_summary <- recent_data %>%
  mutate(date = as.Date(timestamp),            # Extract date
         time_diff = as.numeric(difftime(timestamp, lag(timestamp), units = "secs"))) %>%
  group_by(tag_local_identifier, individual_local_identifier, group_id, date) %>%
  summarize(median_time_diff = median(time_diff, na.rm = TRUE), 
            min_battery = min(eobs_fix_battery_voltage, na.rm = TRUE),   # Calculate median battery level
            .groups = "drop") %>%
  mutate(rounded_time_diff = map_dbl(median_time_diff, round_to_nearest, values = c(60, 120, 7200))) %>%
  mutate(rounded_time_diff = recode(rounded_time_diff, 
                                    "60" = "High",
                                    "120" = "Monitor",
                                    "7200" = "Rest")) 

# Find the most recent `rounded_time_diff` for each tag
most_recent_rounded_time_diff <- daily_summary %>%
  group_by(tag_local_identifier) %>%
  filter(date == max(date)) %>%
  dplyr::select(tag_local_identifier, 
                last_rounded_time_diff = rounded_time_diff,  # Get the most recent rounded time diff
                last_batt_value = min_battery)               # Get the most recent battery value

# daily_summary <- st_as_sf(daily_summary)  
#most_recent_rounded_time_diff <- st_drop_geometry(most_recent_rounded_time_diff)

# Join this information back into the original `daily_summary` to keep all rows
daily_summary <- daily_summary %>%
  left_join(most_recent_rounded_time_diff, by = "tag_local_identifier") 

daily_summary <- daily_summary %>%
  mutate(y_axis_label = interaction(group_id, tag_local_identifier, last_rounded_time_diff, last_batt_value, sep = " - ")) %>%
  mutate(y_axis_label = factor(y_axis_label, 
                               levels = unique(y_axis_label[order(group_id, tag_local_identifier)]))) 

# Create the plot
daily_plot <- ggplot(daily_summary, 
                     aes(x = date, 
                         y = y_axis_label,
                         color = as.numeric(min_battery))) +
  geom_point(size = 3) +
  labs(x = "Date", y = "Tag ID") +
  theme(axis.text.y = element_text(angle = 0, hjust = 1)) +  # Adjust text if needed
  scale_y_discrete(drop = FALSE) +  # Keep all levels even if some are missing 
  scale_color_gradientn(colors = heat.colors(20), 
                        limits = c(3590, 4000)) +
  scale_x_date(date_breaks = "1 week", date_labels = "%Y-%m-%d") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
interactive_plot <- ggplotly(daily_plot, tooltip = "text")

# save plots
saveWidget(interactive_plot, paste0('plots/htmls/','/baboon_data_records.html'), selfcontained = TRUE)

}
## possible mortality check
{
max_distance_summary <- recent_data %>%
  arrange(tag_local_identifier, timestamp) %>%  # Ensure data is sorted by individual and time
  mutate(date = as.Date(timestamp)) %>%         # Extract date
  group_by(tag_local_identifier, date) %>%
  mutate(
    start_long = first(location.long),  # Get starting longitude for the day
    start_lat = first(location.lat),    # Get starting latitude for the day
    distance_from_start = distHaversine(
      cbind(start_long, start_lat), 
      cbind(location.long, location.lat)
    ) / 1000  # Convert meters to kilometers
  ) %>%
  summarize(
    max_distance_from_start = max(distance_from_start, na.rm = TRUE),  # Max distance from the start
    .groups = "drop"
  )

max_distance_summary <- max_distance_summary %>%
  arrange(tag_local_identifier, date) %>%  # Ensure the data is sorted
  group_by(tag_local_identifier) %>%       # Group by identifier
  mutate(
    max_last_days = map_dbl(date, function(current_date) {
      relevant_values <- max_distance_from_start[
        date < current_date & date >= current_date - days_window
      ]
      if (length(relevant_values) > 0) {
        max(relevant_values, na.rm = TRUE)
      } else {
        NA_real_  # Return NA if no relevant values
      }
    })
  ) %>%
  ungroup()  # Remove grouping


max_distance_summary <- max_distance_summary %>%
  arrange(tag_local_identifier, date) %>%  # Ensure the data is sorted
  group_by(tag_local_identifier) %>%       # Group by identifier
  mutate(
    max_last_days = map_dbl(date, function(current_date) {
      relevant_values <- max_distance_from_start[
        date < current_date & date >= current_date - days_window
      ]
      if (length(relevant_values) > 0) {
        max(relevant_values, na.rm = TRUE)
      } else {
        NA_real_  # Return NA if no relevant values
      }
    })
  ) %>%
  ungroup()  # Remove grouping

# Join the calculated max_last_days to daily_summary
daily_summary <- daily_summary %>%
  left_join(
    max_distance_summary %>% select(tag_local_identifier, date, max_last_days),
    by = c("tag_local_identifier", "date")
  )

# Prepare the data for plotting
plot_data <- daily_summary %>%
  filter(!is.na(max_last_days)) %>%  # Remove rows with missing distance values
  select(tag_local_identifier, group_id, date, max_last_days)

# Create the interactive plot
interactive_plot <- plot_ly(
  data = plot_data,
  x = ~date,
  y = ~max_last_days,
  color = ~tag_local_identifier,  # Different line per tag
  type = 'scatter',
  mode = 'lines+markers',
  line = list(width = 2)
) %>%
  layout(
    xaxis = list(title = "Date"),
    yaxis = list(title = "Last Days Max Distance (km)"),
    legend = list(title = list(text = "Tag"),
                  itemclick = "toggleothers"  # Clicking a line shows only that line
    )
  )

interactive_plot <- interactive_plot %>%
  add_trace(
    text = ~paste("Tag ID:", tag_local_identifier, "<br>Group ID:", group_id),
    hoverinfo = "text"  # Customize hover text
  )

# Show the plot
output_file <- paste0('plots/htmls/','/all_ind_distance_plot.html')
saveWidget(interactive_plot, file = output_file, selfcontained = TRUE)
}
## create a table of tags, group, last download date and batt level
{
last_rows_per_tag <- daily_summary %>%
  group_by(tag_local_identifier) %>%
  filter(date == max(date)) %>%
  dplyr::select(tag_local_identifier , individual_local_identifier, group_id, date, rounded_time_diff , last_batt_value, max_last_days  ) %>%  # Exclude specific columns
  ungroup()   %>%
  st_drop_geometry() %>%
  rename(status = rounded_time_diff)

last_rows_per_tag_html <- last_rows_per_tag
# Create HTML formatted columns
last_rows_per_tag_html$tag_local_identifier <- mapply(
  mark_status_change, 
  last_rows_per_tag$status, 
  last_rows_per_tag$last_batt_value,
  last_rows_per_tag$tag_local_identifier,
  last_rows_per_tag$max_last_days  # Assuming this column exists in your data
)
last_rows_per_tag_html <- last_rows_per_tag_html %>%
  select(-max_last_days)

#last_rows_per_tag_html$date <- mapply(mark_old_downloads, last_rows_per_tag$date)

interactive_table <- datatable(
  last_rows_per_tag_html,
  escape = FALSE,  # Allow HTML content to be rendered
  options = list(
    paging = TRUE,
    searching = TRUE,
    ordering = TRUE,
    pageLength = nrow(last_rows_per_tag_html),
    lengthMenu = c(10, 20, nrow(last_rows_per_tag_html)),
    autoWidth = TRUE
  )
)

# Save the table as an HTML file
saveWidget(interactive_table, paste0('plots/htmls/','/table_baboon_data_records.html'), selfcontained = TRUE)
}

## plot maps
cleaned_data <- arrange_data(baboon_data, time_interval_low, speed_threshold)
## plot basic maps prop sleep site
source("code/plot_leaflet_basic.R")
## Run prop sleep site - pie chart
source("code/sleep_site_mapbox.R")

# System commands to commit and push changes
system("git add plots/")  # Add changes only from the plots directory
commit_message <- paste("Automated update -", Sys.Date())  # Generate commit message with date
system(paste('git commit -m "', commit_message, '"', sep = ""))
system("git push origin main")  # Push to the main branch

