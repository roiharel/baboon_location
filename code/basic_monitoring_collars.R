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
  speed_threshold <- set_units(10, "m/s")  # Replace "m/s" with the appropriate unit if needed
  mark_old_downloads <- 21 # 21 days
  possible_mortality <- c(059292, 059293 , 059294, 059295,  059296, 10368, 15484 ,14550 ,14542 ,6898 , 15518) # Replace with actual names
  group_col = c(
    "#800000",   # Maroon - Mlimafisi
    "#7FFF00",   # Chartreuse - Campsite
    "#CD7F32",   # Bronze - BaboonCliffs
    "#50C878",   # Emerald - EagleScout
    "#C8A2C8",   # Lilac - Leikiji
    "#B87333",   # Copper - Clifford
    "#FF00FF",   # Magenta - WestMukenya
    "#87CEFA",   # LapisSplinter - LizardRock2
    "#26619C",   # Lapis - LizardRock
    "#CCCCFF",   # Periwinkle - Pylon
    "#FF0000",   # Red - PhantomWest
    "#008080",   # Teal - Teal
    "#C0C0C0",   # Silver - sneakySilver
    "#800080",   # Purple - Purple
    "#E0115F",   # Ruby - RubyRunners
    "#008000",   # Green - Ivan's
    "#00A36C"    # Jade - Ol Jogi School
  )
  saveRDS(group_col, "C:\\Users\\meerkat\\Documents\\MBRP\\data\\group_colors.RDS")
  setwd("C:\\Users\\meerkat\\Documents\\MBRP")
}
## functions
source("code\\functions\\prep_gps_movebank.R")
source("code\\functions\\create_interactive_table.R")
source("code\\functions\\plot_interactive_map.R")
source("code\\functions\\plot_max_distance.R")
source("code\\functions\\plot_data_records.R")

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

# find and plot nighttime locations - last 10 minutes in the day
data_filtered_night <- cleaned_data %>%
  dplyr::group_by(individual_local_identifier, date(timestamp)) %>%
  slice(n()) %>%
  ungroup() %>%
  filter(format(timestamp, "%H:%M") >= "15:50")

plot_interactive_map(data_filtered_night, 'plots/htmls/baboon_night_interactive_map.html', color_mapping)

saveRDS(data_filtered_night, "data/night_locations.RDS")

system("python code/plot_kmls.py", wait = TRUE)

source("code\\functions\\find_sleeping_site_clusters.R")
source("code\\functions\\find_sleeping_site_transitions.R")

# System commands to commit and push changes
system("git add plots/")  # Add changes only from the plots directory
commit_message <- paste("Automated update -", Sys.Date())  # Generate commit message with date
system(paste('git commit -m "', commit_message, '"', sep = ""))
system("git push origin main")  # Push to the main branch