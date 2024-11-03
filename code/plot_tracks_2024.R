## code is running some basic visualizations of MBRP data collected since Mar 2024
## the input is based on connection to movebank 
## install packages
{
  #library(moveVis)
  library(move2)
  library(ggplot2)
  library(lubridate)
  library(dplyr)
  library(ggmap)
  library(maps)
  
  library(sf)
  library(mapview)
  library(webshot)
  library(leaflet)
  library(htmlwidgets)
  library(RColorBrewer)
  library(units)
  library(magrittr)
  library(purrr) 
  library(plotly)
  library(gridExtra)
  library(png)
  library(grid)    
  library(DT)
  library(htmlwidgets)
  
  
  
}
## parameters - fill in details 
{
#movebank_store_credentials("USER", "PASSWORD", force = TRUE)
#ggmap::register_google(key = "KEY")
time_interval <- "1 mins"
time_interval_low_res <- "10 mins" #time interval for plots
  
date_start <- as.POSIXct("2024-09-01 00:00:00")
speed_threshold <- set_units(10, "m/s")  # Replace "m/s" with the appropriate unit if needed
mark_old_downloads <- 21 # 21 days
}
## functions
{
# load data and basic cleaning
get_data <- function(date_start, time_interval, speed_threshold) {
  baboon_data_2024 <- movebank_download_study(3445611111, sensor_type_id = c("gps"), 
                                              timestamp_start = date_start,
                                             # individual_id = c(3487912671, 3487912662, 3487912663, 3487844492, 3508338112, 3938663627),
                                              remove_movebank_outliers = TRUE) %>%
    filter(!st_is_empty(.))     # remove empty rows
  
  # calc speed azimuth and clean speed outliers
  baboon_data_2024 %<>% mutate(azimuth = mt_azimuth(.), speed = mt_speed(.))
  baboon_data_2024$speed <- set_units(baboon_data_2024$speed, "m/s")
  baboon_data_2024 <- baboon_data_2024 %>%
    filter(speed <= speed_threshold | is.na(speed))
  
  # add fields from metadata
  metadata_2024 <- mt_track_data(baboon_data_2024)
  
  baboon_data_2024 <- baboon_data_2024 %>%
    left_join(metadata_2024 %>% 
                select(individual_local_identifier, tag_local_identifier, group_id, sex), by = c("individual_local_identifier" = "individual_local_identifier"))  %>%
    mt_filter_per_interval(unit = time_interval)
  
  baboon_data_2024$location.long <- sf::st_coordinates(baboon_data_2024)[,1]
  baboon_data_2024$location.lat <- sf::st_coordinates(baboon_data_2024)[,2]
  baboon_data_2024$group_id <- baboon_data_2024$group_id
  

  # Identify matching columns
  matching_columns <- Reduce(intersect, list(names(baboon_data_2024)))
  

  # Join tibbles while keeping only matching columns
  combined_data <- bind_rows(
    select(as.data.frame(baboon_data_2024), matching_columns)
  )

  combined_data <- combined_data %>%
    mutate(plot_name = paste(group_id, year(timestamp), sep = "_")) %>%
    filter(tag_local_identifier != 6915)
    # filter(plot_name != "Campsite_2020")
  
# Return the clustered data
return(combined_data)

# combined_data <- combined_data %>%
#   filter(group_id %in% c("Mlimafisi", "Clifford", "Leikiji"))

}
# Function to round to the nearest specified value
round_to_nearest <- function(x, values) {
  values[which.min(abs(values - x))]
}

# Functions for coloring text in tables
mark_status_change <- function(status, battery, tag) {
  style <- ""
  
  # Check for 'rest' status and battery
  if (status == "Rest" && battery > set_units(3950, "mV")) {
    style <- paste(style, "color: red; font-weight: bold;")  # Add red text color
  }
  
  # Check for 'monitor' status and battery
  if ((status == "Monitor" || status == "High") && battery < set_units(3700, "mV")) {
    style <- paste(style, "color: blue; font-weight: bold;")  # Add blue text color
  }
  
  # Return HTML string with the style
  return(paste("<span style='", style, "'>", tag, "</span>", sep = ""))
}
mark_old_downloads <- function(last_date) {
  style <- ""
  
  # Check for date older than 21 days
  if (last_date < Sys.Date() - mark_old_downloads) {
    style <- paste(style, "color: orange; font-weight: bold;")  # Add blue text color
  }
  # Return HTML string with the style
  return(paste("<span style='", style, "'>", last_date, "</span>", sep = ""))
}
}
## load data and basic cleaning
combined_data <- get_data(date_start, time_interval, speed_threshold)

## plot data dist
{
recent_data <- combined_data[combined_data$timestamp > date_start,]
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
saveWidget(interactive_plot, paste(as.Date(Sys.Date(), format = "%Y%m%d"),'_baboon_data_batt_plot.html'), selfcontained = TRUE)
}
## Calculate median time difference, add group_id, and round it
{
  daily_summary <- recent_data %>%
    mutate(date = as.Date(timestamp),            # Extract date
           time_diff = as.numeric(difftime(timestamp, lag(timestamp), units = "secs"))) %>%
    group_by(tag_local_identifier, group_id, date) %>%
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
    select(tag_local_identifier, 
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
                          limits = c(3590, 4000))  
  interactive_plot <- ggplotly(daily_plot, tooltip = "text")
  
  # save plots
  saveWidget(interactive_plot, paste(as.Date(Sys.Date(), format = "%Y%m%d"),'_baboon_data_records.html'), selfcontained = TRUE)
  webshot(paste(as.Date(Sys.Date(), format = "%Y%m%d"),'_baboon_data_records.html'), file = paste(as.Date(Sys.Date(), format = "%Y%m%d"),'_baboon_data_records.png'), vwidth = 800, vheight = 600)
}
## create a table of tags, group, last download date and batt level
{
  last_rows_per_tag <- daily_summary %>%
    group_by(tag_local_identifier) %>%
    filter(date == max(date)) %>%
    select(tag_local_identifier , group_id, date, rounded_time_diff , last_batt_value  ) %>%  # Exclude specific columns
    ungroup()   %>%
    st_drop_geometry() %>%
    rename(status = rounded_time_diff)
  
  last_rows_per_tag_html <- last_rows_per_tag
  # Create HTML formatted columns
  last_rows_per_tag_html$tag_local_identifier <- mapply(mark_status_change, 
                                     last_rows_per_tag$status, 
                                     last_rows_per_tag$last_batt_value,
                                     last_rows_per_tag$tag_local_identifier)
  
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
  saveWidget(interactive_table, paste(as.Date(Sys.Date(), format = "%Y%m%d"),'table_baboon_data_records.html'), selfcontained = TRUE)
  webshot(paste(as.Date(Sys.Date(), format = "%Y%m%d"),'table_baboon_data_records.html'), file = paste(as.Date(Sys.Date(), format = "%Y%m%d"),'table_baboon_data_records.png'), vwidth = 800, vheight = 1600)
}
## plot maps
{
combined_data <- get_data(date_start, time_interval_low_res, speed_threshold)
## plot basic maps prop sleep site
source("plot_leaflet_basic.R")

## Run prop sleep site - pie chart
source("sleep_site_mapbox.R")
}