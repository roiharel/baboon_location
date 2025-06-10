library(dbscan)
library(geosphere)
library(dplyr)
library(tidyr)
library(lubridate)
library(htmlwidgets)
library(leaflet)
library(sf)
library(move2)

cleaned_data <- readRDS("data/combined_data.RDS")

# Function to cluster sleeping sites and return two summary tables
identify_sleep_clusters <- function(combined_data, eps_thres = 1500, pnts_num = 3) {
  
  # STEP 1: Filter to last point per individual per night (afternoon time)
  data_filtered_night <- combined_data %>%
    mutate(date_val = as.Date(timestamp)) %>%
    filter(date_val > ymd(date_start)) %>%
    group_by(individual_local_identifier, night = date(timestamp)) %>%
    filter(format(timestamp, "%H:%M") >= "15:45") %>%
    slice(n()) %>%
    ungroup()
  
  # STEP 2: Extract coordinates and filter out NAs
  coords <- as.matrix(data_filtered_night[, c("location.lat", "location.long")])
  coords_clean <- na.omit(coords)
  valid_rows <- complete.cases(data_filtered_night[, c("location.lat", "location.long")])
  
  # Keep associated metadata
  ind_ids <- data_filtered_night$individual_local_identifier[valid_rows]
  group_ids <- data_filtered_night$group_id[valid_rows]
  dates <- data_filtered_night$night[valid_rows]
  
  # STEP 3: Run DBSCAN
  distance_matrix <- distm(coords_clean, fun = distHaversine)
  dbscan_result <- dbscan(distance_matrix, eps = eps_thres, minPts = pnts_num, borderPoints = TRUE)

  clustered_data <- data.frame(
    individual_id = ind_ids,
    group_id = group_ids,
    date = dates,
    lat = coords_clean[, 1],
    lon = coords_clean[, 2],
    cluster = dbscan_result$cluster
  )
  
  # STEP 4: Clean cluster data
  clustered_data <- clustered_data %>% filter(cluster != 0)
  
  # Table 1: Cluster summary with centroids
  cluster_summary <- clustered_data %>%
    dplyr::group_by(cluster) %>%
    dplyr::summarise(
      lat = mean(lat),
      lon = mean(lon),
      count = n()
    )
  
  # Table 2: Where each individual slept each night
  individual_night_locations <- clustered_data %>%
    select(date, cluster, lat, lon, individual_id, group_id)
  
  # Return as list of two tables
  return(list(
    cluster_summary = cluster_summary,
    individual_night_locations = individual_night_locations
  ))
}

# Example usage
results <- identify_sleep_clusters(cleaned_data)

# Access both tables
cluster_summary <- results$cluster_summary
individual_night_locations <- results$individual_night_locations

# Optionally save to CSV
write.csv(cluster_summary, "cluster_summary.csv", row.names = FALSE)
write.csv(individual_night_locations, "individual_night_locations.csv", row.names = FALSE)

# Ensure cluster column exists and is numeric or factor
cluster_summary$cluster <- as.factor(cluster_summary$cluster)

dist_matrix <- distm(cluster_summary[, c("lon", "lat")], fun = distHaversine)
db <- dbscan::dbscan(dist_matrix, eps = 1500, minPts = 1, borderPoints = TRUE)

# Step 4: Add cluster_united back to the original data
cluster_summary$cluster_united <- as.factor(db$cluster)

# Step 5: Create final dataframe summarizing cluster_united groups
cluster_merged_summary <- cluster_summary %>%
  dplyr::group_by(cluster_united) %>%
  dplyr::summarise(
    lat = mean(lat),
    lon = mean(lon),
    count = sum(count),
    .groups = "drop"
  )

# Ensure cluster is a factor or character for a smooth join
individual_night_locations <- individual_night_locations %>%
  dplyr::mutate(cluster = as.character(cluster))

cluster_summary <- cluster_summary %>%
  dplyr::mutate(cluster = as.character(cluster))

# Perform the join
individual_night_locations <- individual_night_locations %>%
  dplyr::left_join(
    cluster_summary %>% dplyr::select(cluster, cluster_united),
    by = "cluster"
  )

# Optional: Keep mapping of original cluster to unified cluster
#cluster_mapping <- cluster_summary %>%
#  select(cluster, cluster_united, lat, lon, count)

# Create Leaflet map
map <- leaflet(data = cluster_merged_summary) %>%
  addProviderTiles("Esri.WorldImagery") %>%  # Satellite-style background
  addCircleMarkers(
    ~lon, ~lat,
    radius = 6,
    color = "blue",
    fillColor = "lightblue",
    fillOpacity = 0.8,
    stroke = TRUE,
    label = ~paste("Cluster", cluster_united),
    popup = ~paste0(
      "<strong>Cluster: </strong>", cluster_united, "<br>",
      "<strong>Count: </strong>", count, "<br>",
      "<strong>Latitude: </strong>", round(lat, 5), "<br>",
      "<strong>Longitude: </strong>", round(lon, 5)
    )
  )

saveWidget(map, file = "clustered_map_satellite.html", selfcontained = TRUE)
