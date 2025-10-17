library(dbscan)
library(geosphere)
library(dplyr)
library(tidyr)
library(lubridate)
library(htmlwidgets)
library(leaflet)
library(sf)
library(move2)
library(data.table)

# parameters
cluster_dist <- 0.0001
united_cluster_dist <- 0.001  

dt <- readRDS("data/night_locations.RDS")

dt <- as.data.table(dt)
dt <- dt[, .(individual_local_identifier, group_id, location.lat, location.long, timestamp)]

# Function to cluster sleeping sites and return two summary tables
identify_sleep_clusters <- function(dt, eps_thres = 0.005, pnts_num = 3) {

  # Extract coordinates and filter out NAs
  coords <- as.matrix(dt[, c("location.lat", "location.long")])
  coords_clean <- na.omit(coords)
  valid_rows <- complete.cases(dt[, c("location.lat", "location.long")])
  
  # Keep associated metadata
  ind_ids <- dt$individual_local_identifier[valid_rows]
  group_ids <- dt$group_id[valid_rows]
  dates <- as.Date(dt$timestamp[valid_rows])
  
  # Run DBSCAN
  distance_matrix <- distm(coords_clean, fun = distHaversine)
  dbscan_result <- dbscan(coords_clean, eps = cluster_dist, minPts = 2, borderPoints = TRUE)

  clustered_data <- data.frame(
    individual_id = ind_ids,
    group_id = group_ids,
    date = dates,
    lat = coords_clean[, 1],
    lon = coords_clean[, 2],
    cluster = dbscan_result$cluster
  )
  
  # Clean cluster data
  clustered_data <- clustered_data %>% filter(cluster != 0)
  
  # Table 1: Cluster summary with centroids
  cluster_summary <- clustered_data %>%
    dplyr::group_by(cluster) %>%
    dplyr::summarise(
      lat = mean(lat),
      lon = mean(lon),
      count = n()
    )
  
  cluster_summary$cluster <- as.factor(cluster_summary$cluster)
  
  # Table 2: Where each individual slept each night
  individual_night_locations <- clustered_data %>%
    select(date, cluster, lat, lon, individual_id, group_id)
  
  # Return as list of two tables
  return(list(
    cluster_summary = cluster_summary,
    individual_night_locations = individual_night_locations
  ))
}

# first level - sleeping within-site level
results <- identify_sleep_clusters(dt, eps_thres = cluster_dist)

# Access both tables
cluster_summary <- results$cluster_summary
individual_night_locations <- results$individual_night_locations

# second level - cluster united - sleeping site level
db <- dbscan::dbscan(cluster_summary[, c("lon", "lat")], eps = united_cluster_dist , minPts = 1, borderPoints = TRUE)

# Add cluster_united back to the original data
cluster_summary$cluster_united <- as.factor(db$cluster)

# Create final dataframe summarizing cluster_united groups
cluster_merged_summary <- cluster_summary %>%
  group_by(cluster_united) %>%
  summarise(
    lat = weighted.mean(lat, count),
    lon = weighted.mean(lon, count),
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

# Create Leaflet map

# Base map
map <- leaflet() %>%
  addProviderTiles("Esri.WorldImagery") %>%
  setView(lng = mean(cluster_merged_summary$lon),
          lat = mean(cluster_merged_summary$lat),
          zoom = 12)

# Add blue markers for cluster_merged_summary
map <- map %>%
  addCircleMarkers(data = cluster_merged_summary,
                   lng = ~lon, lat = ~lat,
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
                   ),
                   group = "Merged Clusters")

# Add grey markers for cluster_summary
map <- map %>%
  addCircleMarkers(data = cluster_summary,
                   lng = ~lon, lat = ~lat,
                   radius = 4,
                   color = "black",
                   fillColor = "black",
                   fillOpacity = 0.6,
                   stroke = TRUE,
                   label = ~paste("Cluster - raw", cluster, "Cluster - united", cluster_united),
                   popup = ~paste0(
                     "<strong>Cluster: </strong>", cluster_united, "<br>",
                     "<strong>Count: </strong>", count, "<br>",
                     "<strong>Latitude: </strong>", round(lat, 5), "<br>",
                     "<strong>Longitude: </strong>", round(lon, 5)
                   ),
                   group = "Raw Clusters")

# Add small white dots for individual_night_locations
map <- map %>%
  addCircleMarkers(data = individual_night_locations,
                   lng = ~lon, lat = ~lat,
                   radius = 2,
                   color = "white",
                   fillColor = "white",
                   fillOpacity = 0.9,
                   stroke = FALSE,
                   label = ~paste("ID:", individual_id, "date:", date, "group:", group_id),
                   group = "Individual Nights")

# Add layer control
map <- map %>%
  addLayersControl(
    overlayGroups = c("Merged Clusters", "Raw Clusters", "Connections", "Individual Nights"),
    options = layersControlOptions(collapsed = FALSE)
  )


saveWidget(map, file = "plots/htmls/clustered_map_satellite.html", selfcontained = TRUE)

saveRDS(individual_night_locations, "data/night_locations_clust.RDS")
# Optionally save to CSV
write.csv(cluster_summary, "cluster_summary.csv", row.names = FALSE)
write.csv(individual_night_locations, "individual_night_locations.csv", row.names = FALSE)


