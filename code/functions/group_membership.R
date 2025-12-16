# --------------------------------------------------------------
# Identify day-by-day group membership
# --------------------------------------------------------------

suppressPackageStartupMessages({
  library(dplyr)
  library(ggplot2)
  library(dbscan)
  library(geosphere)
  library(lubridate)
  library(scales)
  library(sf)
  library(htmlwidgets)
  library(plotly)
})

# Read and Filter Data (Replace paths or sources as needed)
# "gps_v1_1hour.RDS" should exist and contain columns including:
# individual_local_identifier
# group_id
# location.lat, location.long
# timestamp
# Assumes 'group_id' is a factor or character. If not, convert below.

cleaned_data <- readRDS("data/gps_v1_1hour.RDS")
filtered_data <- cleaned_data %>%
  st_drop_geometry() %>%  # Drop spatial column
  mutate(timestamp_dt = as.POSIXct(timestamp)) %>%
  #filter(year(timestamp_dt) == 2025) %>%  # Filter specific month
  #filter(month(timestamp_dt) == 8) %>%  # Filter specific month
  filter(hour(timestamp_dt) > 11) %>%   # Filter for hour > 14
  mutate(interval = floor_date(timestamp_dt, "day")) %>%
  group_by(individual_local_identifier, interval) %>%
  slice_head(n = 1) %>%
  ungroup()

group_col <- readRDS("data/group_colors.RDS")

# Example filter: March only, hour > 14, then daily (first occurrence).

# cleaned_data <- readRDS("data/night_locations.RDS")
# filtered_data <- cleaned_data %>%
#   st_drop_geometry() %>%  # Drop spatial column
#   mutate(timestamp_dt = as.POSIXct(date)) %>%
#   filter(year(timestamp_dt) == 2025) %>%  # Filter specific month
#   filter(month(timestamp_dt) == 8) 

# DBSCAN Clustering per Interval
# Adjust eps/minPts as needed for your data scale.
cluster_results <- filtered_data %>%
  group_by(interval) %>%
  group_modify(~ {
    coords <- .x %>% select(location.long, location.lat)
    db_result <- dbscan(coords, eps = 0.005, minPts = 1)
    .x %>% mutate(cluster_id = db_result$cluster)
  }) %>%
  ungroup()

# Calculate Cluster Centroids
cluster_centroids <- cluster_results %>%
  filter(cluster_id > 0) %>%  # Exclude noise points (cluster_id=0)
  group_by(interval, cluster_id) %>%
  summarize(
    centroid_lat = median(location.lat, na.rm = TRUE),
    centroid_lon = median(location.long, na.rm = TRUE),
    .groups = "drop"
  )

# Distance to Centroid, Belonging Strength
cluster_membership_data <- cluster_results %>%
  filter(cluster_id > 0) %>%
  left_join(cluster_centroids, by = c("interval", "cluster_id")) %>%
  rowwise() %>%
  mutate(distance_to_centroid = distHaversine(
    c(location.long, location.lat),
    c(centroid_lon, centroid_lat)
  )) %>%
  ungroup() %>%
  group_by(interval, cluster_id) %>%
  mutate(
    max_dist_in_cluster = max(distance_to_centroid, na.rm = TRUE),
    cluster_belonging = ifelse(
      max_dist_in_cluster == 0, 1,
      1 - (distance_to_centroid / max_dist_in_cluster)
    )
  ) %>%
  ungroup()

# Build Summary Data
summary_df <- cluster_membership_data %>%
  select(
    individual_id = individual_local_identifier,
    group_id,
    interval,
    cluster = cluster_id,
    centroid_lat,
    centroid_lon,
    cluster_belonging,
    distance_to_centroid
  ) %>%
  arrange(interval, individual_id, cluster)

# Calculate Cluster Distances for Ordering
reference_loc <- summary_df %>%
  slice(1) %>%
  summarize(
    lat = mean(centroid_lat, na.rm = TRUE),
    lon = mean(centroid_lon, na.rm = TRUE)
  )

summary_df_with_distance <- summary_df %>%
  group_by(cluster, interval) %>%
  slice_head(n = 1) %>%  # one record per cluster-day
  ungroup() %>%
  rowwise() %>%
  mutate(
    distance_from_ref = distHaversine(
      c(centroid_lon, centroid_lat),
      c(reference_loc$lon, reference_loc$lat)
    )
  ) %>%
  ungroup()

# Summarize average distance per cluster
cluster_distances_df <- summary_df_with_distance %>%
  group_by(cluster) %>%
  summarize(avg_distance_km = round(mean(distance_from_ref, na.rm = TRUE) / 1000, 2)) %>%
  arrange(avg_distance_km)

# Build the "alluvial_data" for the Timeline Plot
# The first snippet references "alluvial_data" with columns:
# individual_id (factor), day (factor), cluster (factor), group_id (factor).
alluvial_data <- summary_df %>%
  mutate(
    day = factor(as.Date(interval)),  # factor of date
    group_id = factor(group_id)       # ensure factor
  ) %>%
  select(individual_id, day, cluster, group_id)

# We'll define 'cluster_distances_user_defined' based on the above distances
# so the first snippet's code can use it.
cluster_distances_user_defined <- cluster_distances_df %>%
  dplyr::rename(avg_distance_km = avg_distance_km) %>%  # rename if needed
  mutate(cluster = factor(cluster))  # ensure it's a factor

# Create a 'distinct_colors' palette for groups
unique_groups <- sort(unique(alluvial_data$group_id))


# The Timeline Plot (from the first snippet), adapted to use 'cluster_distances_user_defined'.
# We assume we have "alluvial_data", "cluster_distances_user_defined", and "distinct_colors".
# -- Plotting Parameters ---
intra_cluster_step_height <- 1
inter_cluster_gap_factor_per_individual <- 0.5
rect_x_width <- 0.4
rect_y_padding <- intra_cluster_step_height * 0.25

# Flag to let snippet know we have user-defined cluster distances
cluster_distances_user_defined <- cluster_distances_user_defined

# Compute cluster order from user-defined distances
clusters_in_data <- levels(factor(alluvial_data$cluster))
default_cluster_order_df <- cluster_distances_user_defined

# Ensure missing clusters in data are added if not found in user-defined table
missing_clusters_for_order <- setdiff(clusters_in_data, default_cluster_order_df$cluster)
if(length(missing_clusters_for_order) > 0) {
  max_existing_dist <- if(nrow(default_cluster_order_df) > 0) {
    max(default_cluster_order_df$avg_distance_km, na.rm=TRUE)
  } else 0
  default_cluster_order_df <- bind_rows(
    default_cluster_order_df,
    data.frame(cluster = missing_clusters_for_order,
               avg_distance_km = max_existing_dist + seq_along(missing_clusters_for_order))
  )
  warning("Some clusters from data were not in cluster_distances_user_defined; added them.")
}

cluster_order_df <- default_cluster_order_df %>%
  mutate(avg_distance_m = avg_distance_km * 1000) %>%
  arrange(avg_distance_m) %>%
  mutate(cluster = factor(cluster, levels = .$cluster))
ordered_cluster_levels <- levels(cluster_order_df$cluster)
alluvial_data$cluster <- factor(alluvial_data$cluster, levels = ordered_cluster_levels)

# Convert day to numeric for plotting
day_levels <- levels(alluvial_data$day)
alluvial_data <- alluvial_data %>%
  mutate(day_numeric = as.numeric(factor(day, levels = day_levels)))

# Compute n_individuals_per_cluster_day
n_individuals_per_cluster_day <- alluvial_data %>%
  group_by(day_numeric, cluster) %>%
  summarise(n_in_cluster = n_distinct(individual_id), .groups = 'drop')

# Compute y_offsets
y_offsets <- expand.grid(
  day_numeric = unique(alluvial_data$day_numeric),
  cluster = ordered_cluster_levels
) %>%
  left_join(n_individuals_per_cluster_day, by = c("day_numeric", "cluster")) %>%
  mutate(n_in_cluster = ifelse(is.na(n_in_cluster), 0, n_in_cluster)) %>%
  arrange(day_numeric, cluster) %>%
  group_by(day_numeric) %>%
  mutate(
    height_of_cluster_itself = n_in_cluster * intra_cluster_step_height,
    gap_above_this_cluster = n_in_cluster * inter_cluster_gap_factor_per_individual,
    total_block_height = height_of_cluster_itself + gap_above_this_cluster,
    cumulative_y_offset_before_this_cluster = lag(cumsum(total_block_height), default = 0)
  ) %>%
  ungroup() %>%
  select(day_numeric, cluster, cluster_base_y = cumulative_y_offset_before_this_cluster,
         height_of_cluster_itself)

# Join offsets & finalize positions
plot_data <- alluvial_data %>%
  left_join(y_offsets, by = c("day_numeric", "cluster")) %>%
  group_by(day_numeric, cluster) %>%
  mutate(rank_in_cluster_time = rank(as.character(individual_id), ties.method = "first")) %>%
  ungroup() %>%
  mutate(
    y_position = cluster_base_y + (rank_in_cluster_time - 1) * intra_cluster_step_height
  )

# Prepare rectangles
rect_data <- plot_data %>%
  group_by(day_numeric, cluster, cluster_base_y, height_of_cluster_itself) %>%
  filter(height_of_cluster_itself > 0) %>%
  summarise(.groups = 'drop') %>%
  mutate(
    xmin = day_numeric - rect_x_width,
    xmax = day_numeric + rect_x_width,
    ymin = cluster_base_y - rect_y_padding,
    ymax = cluster_base_y + height_of_cluster_itself - intra_cluster_step_height + rect_y_padding
  )

# Build the timeline plot
timeline_plot <- ggplot() +
  geom_rect(
    data = rect_data,
    aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
    fill = NA,
    color = "darkgray",
    linewidth = 0.5
  ) +
  geom_line(
    data = plot_data,
    aes(x = day_numeric, y = y_position, group = individual_id, color = group_id),
    linewidth = 0.7
  ) +
  geom_point(
    data = plot_data,
    aes(x = day_numeric, y = y_position, color = group_id, 
        text = paste0("Animal ID: ", individual_id, 
                      "\nGroup: ", group_id,
                      "\nCluster: ", cluster,
                      "\nDay: ", day)),
    size = 2,
    shape = 16
  ) +
  scale_color_manual(values = group_col, name = "Group") +
  scale_x_continuous(
    breaks = 1:length(day_levels),
    labels = day_levels
  ) +
  labs(
    x = "Time (Filtered Days)",
    y = "Relative Position (Proportionally Spaced Clusters)",
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_line(color = "gray90"),
    panel.grid.major.y = element_blank()
  )

# Print or return the final plot
saveWidget(ggplotly(timeline_plot), file = "plots/htmls/group_membership.html", selfcontained = TRUE)
saveRDS(summary_df, "data/group_membership.RDS")