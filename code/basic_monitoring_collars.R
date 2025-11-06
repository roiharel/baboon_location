## code is running some basic visualizations of MBRP data collected since Mar 2024
## the input is based on connection to movebank
## install packages
{
  # Silent installer / loader for a vector of packages
  silent_install_and_load <- function(pkgs, lib = Sys.getenv("R_LIBS_USER")) {
    # Helper to suppress everything (messages, warnings, output)
    quiet <- function(expr) {
      sink(tempfile())         # divert stdout
      on.exit({
        sink()                 # restore stdout
      }, add = TRUE)
      suppressMessages(suppressWarnings(invisible(capture.output(force(expr), file = NULL))))
    }
    # Ensure user lib exists quietly
    quiet(dir.create(lib, recursive = TRUE, showWarnings = FALSE))
    # Set CRAN mirror quietly
    quiet(options(repos = c(CRAN = "https://cloud.r-project.org")))
    # Normalize package names and remove duplicates
    pkgs <- unique(as.character(pkgs))
    pkgs <- pkgs[pkgs != ""]  # drop empty
    # Install missing packages quietly
    for (p in pkgs) {
      quiet({
        if (!requireNamespace(p, quietly = TRUE)) {
          install.packages(p, lib = lib, repos = getOption("repos"),
                           dependencies = TRUE, quiet = TRUE)
        }
      })
    }
    # Load packages quietly; prefer suppressPackageStartupMessages + library()
    for (p in pkgs) {
      quiet({
        if (requireNamespace(p, quietly = TRUE)) {
          suppressPackageStartupMessages(
            library(p, character.only = TRUE, quietly = TRUE, logical.return = TRUE, lib.loc = lib)
          )
        }
      })
    }
    # Return invisibly a named logical of availability (no print)
    invisible(sapply(pkgs, function(p) requireNamespace(p, quietly = TRUE)))
  }
  # Example usage (replace with your package vector)
  packages <- c(
    "move2","ggplot2","lubridate","dplyr","ggmap","maps","sf","mapview",
    "leaflet","leaflet.minicharts","htmlwidgets","RColorBrewer","units",
    "magrittr","purrr","plotly","gridExtra","png","grid","DT","keyring",
    "lwgeom","rmarkdown","geosphere","dbscan","xml2","tidyverse","arrow",
    "data.table"
  )
  # Run silently; nothing will print to console
  silent_install_and_load(packages)
}
## parameters - fill in details
{
  #movebank_store_credentials("USER", "PASSWORD", force = TRUE)
  #ggmap::register_google(key = "KEY")
  study_id <- 3445611111
  time_interval_high <- "1 mins"
  time_interval_low <- "1 hours" #time interval for plots
  days_window <- 4 # window in days - max distance
  date_start <- as.POSIXct("2024-02-28 03:00:00")
  date_end <- now(tz = "CET" )
  speed_threshold <- set_units(5, "m/s")  # Replace "m/s" with the appropriate unit if needed
  mark_old_downloads <- 21 # 21 days
  possible_mortality <- c(059292, 059293 , 059294, 059295,  059296, 10368, 15484 ,14550 ,14542 ,6898 , 15518) # Replace with actual names
  # Your color mapping
  color_mapping = c(
    Copper = "#B87333",        # 
    Bronze = "#CD7F32",        # 
    Chartreuse = "#7FFF00",    # 
    Emerald = "#50C878",       # 
    Lilac = "#C8A2C8",         # 
    Periwinkle = "#CCCCFF",    # 
    Lapis = "#26619C",         # 
    Maroon = "#800000",        # 
    Magenta = "#FF00FF",       # 
    LapisSplinter = "#87CEFA", # 
    RubyRunners = "#E0115F",   # 
    PhantomWest = "#FF0000",   # 
    SneakySilver = "#C0C0C0",  # 
    TrickyTeal = "#008080",    # 
    Purple = "#800080",        #
    Green = "#008000",         # 
    Jade = "#00A36C"           # 
  )
  saveRDS(color_mapping, "C:\\Users\\meerkat\\Documents\\MBRP\\data\\group_colors.RDS")
  setwd("C:\\Users\\meerkat\\Documents\\MBRP")
}
## functions
source("code\\functions\\prep_gps_movebank.R")
source("code\\functions\\create_interactive_table.R")
source("code\\functions\\plot_interactive_map.R")
source("code\\functions\\plot_max_distance.R")
source("code\\functions\\plot_data_records.R")
source("code\\functions\\cluster_groups.R")

## plot data
result <- plot_data_records(cleaned_data_high)

# Access the returned data
cleaned_data <- result$cleaned_data
daily_summary <- result$daily_summary

## possible mortality check
daily_summary <- plot_max_distance(cleaned_data, daily_summary, days_window, color_mapping)

## make a monitoring table
create_interactive_table(daily_summary)

# Identify missing dates and filter for NA gps_fix_count
missing_gps_data <- daily_summary %>%
  group_by(individual_local_identifier) %>%
  complete(date = seq(min(date), max(date), by = "day")) %>%
  filter(is.na(gps_fix_count)) %>%
  ungroup()

# Plot the data
missing_gps_plot <- ggplotly(ggplot(missing_gps_data, aes(x = date, y = individual_local_identifier)) +
                               geom_point() +
                               labs(title = "Missing GPS Fix Count Data Points", x = "Date", y = "Individual ID") +
                               theme_minimal())
saveWidget(missing_gps_plot, 'plots/htmls/missing_gps_plot.html', selfcontained = TRUE)

# plot daytime locations
plot_interactive_map(cleaned_data_low, 'plots/htmls/baboon_interactive_map.html', color_mapping)

# Nighttime locations (last fix of the day after 15:50)
data_filtered_night <- cleaned_data %>%
  mutate(date = date(timestamp)) %>%
  group_by(individual_local_identifier, date) %>%
  slice_tail(n = 1) %>%
  ungroup() %>%
  filter(format(timestamp, "%H:%M") >= "15:50")

plot_interactive_map(data_filtered_night, 'plots/htmls/baboon_night_interactive_map.html', color_mapping)

saveRDS(data_filtered_night, "data/night_locations.RDS")

system("python code/plot_kmls.py", wait = TRUE)

# find nighttime clusters
dt <- readRDS("data/night_locations.RDS")
results <- cluster_groups(dt, eps_thres = 0.0001, united_eps_thres = 0.001, plot_map = TRUE)
# Save outputs
saveRDS(results$individual_night_locations, "data/night_locations_clust.RDS")
saveWidget(results$map, file = "plots/htmls/clustered_map_satellite.html", selfcontained = TRUE)
write.csv(results$cluster_summary, "cluster_summary.csv", row.names = FALSE)
write.csv(results$individual_night_locations, "individual_night_locations.csv", row.names = FALSE)

# find midday clusters
data_filtered_midday <- cleaned_data %>%
  mutate(date = date(timestamp)) %>%
  filter(format(timestamp, "%H:%M") >= "10:00") %>%
  group_by(individual_local_identifier, date) %>%
  slice_min(timestamp, with_ties = FALSE) %>%
  ungroup()

results <- cluster_groups(data_filtered_midday, eps_thres = 0.0001, united_eps_thres = 0.001,plot_map = TRUE)

# Save outputs
saveRDS(results$individual_night_locations, "data/day_locations_clust.RDS")
saveWidget(results$map, file = "plots/htmls/day_clustered_map_satellite.html", selfcontained = TRUE)
write.csv(results$cluster_summary, "day_cluster_summary.csv", row.names = FALSE)
write.csv(results$individual_night_locations, "day_individual_night_locations.csv", row.names = FALSE)

source("code\\functions\\find_sleeping_site_clusters.R")
source("code\\functions\\find_sleeping_site_transitions.R")

source("code\\functions\\prep_location_mat.R")

prep_location_mat(
  input_rds_path = "data/gps_v1.RDS",
  output_dir = "data",
  utm_zone = 37,
  hemisphere = "north")

# System commands to commit and push changes
system("git add plots/")  # Add changes only from the plots directory
commit_message <- paste("Automated update -", Sys.Date())  # Generate commit message with date
system(paste('git commit -m "', commit_message, '"', sep = ""))
system("git push origin main")  # Push to the main branch
  