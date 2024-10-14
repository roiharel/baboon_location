## Code is using output from the sleep site estimation of plot_tracks_2024 and out csv ti use in mapbox and plots pie charts of sleep site per site per group

# Load necessary libraries
library(dbscan)
library(geosphere)
library(dplyr)
library(tidyr)
library(leaflet)
library(leaflet.minicharts)
library(RColorBrewer)


pie_size <- 10 # set size of pie charts
eps_thres <- 500 # DBscan parameter
pnts_num <- 3 # DBscan parameter

# Row per event
cluster_positions <- function(data_filtered_night) {
  # Prepare the data and clean NA values
  coords <- as.matrix(data_filtered_night[, c("location.lat", "location.long")])
  coords_clean <- na.omit(coords)  # Remove rows with NAs
  
  # Also keep group_id for each point, remove corresponding rows with NAs
  group_ids <- data_filtered_night$group_id[complete.cases(data_filtered_night[, c("location.lat", "location.long")])]
  
  # Calculate the distance matrix using the Haversine formula
  distance_matrix <- distm(coords_clean, fun = distHaversine)
  
  # Apply DBSCAN clustering
  dbscan_result <- dbscan(distance_matrix, eps = eps_thres, minPts = pnts_num, borderPoints = TRUE)
  
  # Create a dataframe with group_id, lat, lon, and cluster assignments
  clustered_data <- data.frame(
    group_id = group_ids,
    lat = coords_clean[, 1],
    lon = coords_clean[, 2],
    cluster = dbscan_result$cluster
  )
  
  clustered_data <- clustered_data %>% filter(cluster != 0)
  
  # Return the clustered data
  return(clustered_data)
}

# Row per cluster
simplify_cluster_table <- function(clustered_data) {
  
  # Group by cluster, calculate centroid, and summarize group_ids
  clustered_data_clean <- clustered_data %>%
    group_by(cluster) %>%
    summarise(
      group_ids_combined = paste(unique(group_id), collapse = ", "),  # Concatenate unique group_ids
      centroid_lat = mean(lat),  # Calculate mean latitude (centroid)
      centroid_lon = mean(lon)   # Calculate mean longitude (centroid)
    )
  
  # Split the rows where there are multiple group_ids
  clustered_data_clean <- clustered_data_clean %>%
    separate_rows(group_ids_combined, sep = ", ")  # Split by comma and space
  
  clustered_data_clean <- clustered_data_clean %>%
    rename(
      group_id = group_ids_combined,  # Rename the concatenated group IDs to 'group_id'
      lat = centroid_lat,          # Rename latitude column to 'lat'
      lon = centroid_lon           # Rename longitude column to 'lon'
    )
  
  # Add a numeric serial ID for group_id
  clustered_data_clean$group_id_serial <- as.numeric(as.factor(clustered_data_clean$group_id))
  
  
  # Add a new column concatenating group IDs in the same cluster
  clustered_data_clean <- clustered_data_clean %>%
    group_by(cluster) %>%
    mutate(group_ids_in_cluster = paste(unique(group_id), collapse = ", ")) %>%
    ungroup()
  
  # Return the cleaned and processed data
  return(clustered_data_clean)
}

## plot pie plot
create_leaflet_map_with_proportions <- function(clustered_data) {
  # Group by cluster and group_id, then calculate the proportions
  cluster_by_group <- clustered_data %>%
    group_by(cluster) %>%
    summarise(
      lat = first(lat),  # Get first latitude in the cluster
      lon = first(lon),  # Get first longitude in the cluster
      group_counts = list(table(group_id))  # Count occurrences of each group_id in the cluster
    ) %>%
    unnest_wider(group_counts) %>%
    rowwise() %>%
    # Create a new column that sums the values of the row, excluding 'cluster', 'lat', and 'lon'
    mutate(row_sum = sum(c_across(-c(cluster, lat, lon)), na.rm = TRUE)) %>%
    ungroup()
  
  # Prepare data for leaflet
  group_col <- colnames(cluster_by_group)[!colnames(cluster_by_group) %in% c("cluster", "lat", "lon", "row_sum")]
  
  # Define a color palette for pie charts
  color_palette <- colorRampPalette(brewer.pal(8, "Set1"))(length(group_col))
  
  # Create a leaflet map
  leaflet_map <- leaflet(cluster_by_group) %>%
    addTiles(group = "OSM") %>%
    addProviderTiles(providers$Esri.WorldTopoMap, group = "Topo") %>%
    addProviderTiles(providers$Esri.WorldImagery, group = "Terrain", options = providerTileOptions(noWrap = TRUE))    

  # Add minicharts (pie charts) for group proportions
  leaflet_map <- leaflet_map %>%
    addMinicharts(
      lng = cluster_by_group$lon,  # Longitude of the cluster
      lat = cluster_by_group$lat,  # Latitude of the cluster
      type = "pie",  # Specify pie charts
      chartdata = cluster_by_group %>% 
        select(all_of(group_col)),  # Data for pie charts
      width = pie_size * sqrt(cluster_by_group$row_sum / sqrt(max(cluster_by_group$row_sum))),  # Scale pie chart size
      transitionTime = 0,  # Disable animation
      colorPalette = color_palette  # Apply the color palette to the pie charts
    ) %>%
    addLayersControl(
      baseGroups = c("OSM", "Topo", "Terrain")
    )
  # Return the leaflet map
  return(leaflet_map)
}


clustered_data <- cluster_positions(data_filtered_night)

clustered_data_clean <- simplify_cluster_table(clustered_data)

# Save the result to a CSV file (optional, if required)
# Write to CSV if needed, comment this out if not needed
write.csv(clustered_data_clean, "clustered_data_clean.csv", row.names = FALSE)

# Display the map
leaflet_map <- create_leaflet_map_with_proportions(clustered_data)
leaflet_map  # 

saveWidget(leaflet_map, paste(as.Date(Sys.Date(), format = "%Y%m%d"),'_prop_sleep_site_map_2024.html'), selfcontained = TRUE)
