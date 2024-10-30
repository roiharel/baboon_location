# Load required libraries
library(move2)
library(ggplot2)
library(lubridate)
library(dplyr)
library(sf)
library(RColorBrewer)

# Set parameters
time_interval <- "10 mins"
date_start <- as.POSIXct("2024-03-01 00:00:00")

# Download and process baboon data
baboon_data_2024 <- movebank_download_study(
    study_id = 3445611111,
    sensor_type_id = "gps",
    timestamp_start = date_start,
    remove_movebank_outliers = TRUE
)

# Retrieve metadata and merge it with main data
metadata_2024 <- mt_track_data(baboon_data_2024)
baboon_data_2024 <- baboon_data_2024 %>%
    left_join(metadata_2024 %>% select(individual_local_identifier, tag_local_identifier, group_id, sex),
        by = "individual_local_identifier"
    ) %>%
    mt_filter_per_interval(unit = time_interval)

# Extract longitude and latitude coordinates
coords <- st_coordinates(baboon_data_2024)
baboon_data_2024$location.long <- coords[, 1]
baboon_data_2024$location.lat <- coords[, 2]

# Step 1: Summarize data to count individuals per group
individual_count_per_group <- baboon_data_2024 %>%
    group_by(group_id) %>%
    summarise(individual_count = n_distinct(individual_local_identifier))

# Step 2: Plot number of individuals per group
ggplot(individual_count_per_group, aes(x = group_id, y = individual_count, fill = group_id)) +
    geom_bar(stat = "identity") +
    labs(title = "Number of Individuals in Each Group", x = "Group ID", y = "Number of Individuals") +
    theme_minimal() +
    theme(legend.position = "none") +
    scale_fill_brewer(palette = "Set2")

# Step 3: Add 'month' column and summarize data by group and month
baboon_data_2024 <- baboon_data_2024 %>%
    mutate(month = floor_date(timestamp, "month")) %>%
    filter(!is.na(location.lat) & !is.na(location.long))

group_distribution_over_time <- baboon_data_2024 %>%
    group_by(group_id, month) %>%
    summarise(individual_count = n_distinct(individual_local_identifier), .groups = "drop")

# Step 4: Plot group distribution over time
ggplot(group_distribution_over_time, aes(x = month, y = individual_count, color = group_id)) +
    geom_line(linewidth = 1) +
    geom_point(size = 2) +
    labs(title = "Baboon Group Distribution Over Time", x = "Month", y = "Number of Individuals", color = "Group ID") +
    scale_color_brewer(palette = "Set1") +
    theme_minimal()

# Step 5: Facet each group in its own panel by month
ggplot(group_distribution_over_time, aes(x = month, y = individual_count)) +
    geom_line(linewidth = 1) +
    geom_point(size = 2) +
    facet_wrap(~group_id, scales = "free_y") +
    labs(title = "Baboon Group Distribution Over Time (Each Group)", x = "Month", y = "Number of Individuals") +
    theme_minimal()

# Step 6: Plot spatial distribution of each group with monthly facets
group_colors <- RColorBrewer::brewer.pal(n = length(unique(baboon_data_2024$group_id)), "Set3")
ggplot(baboon_data_2024, aes(x = location.long, y = location.lat, color = factor(group_id))) +
    geom_point(alpha = 0.7) +
    scale_color_manual(values = group_colors) +
    labs(title = "Baboon Group Movement by Month", x = "Longitude", y = "Latitude", color = "Group ID") +
    theme_minimal() +
    facet_wrap(~month, ncol = 3) +
    theme(legend.position = "right")
