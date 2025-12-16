library(dplyr)
library(purrr)

# Filter to first fix after 10:00 per individual per day
data_filtered_midday <- cleaned_data %>%
  mutate(date = as.Date(timestamp)) %>%
  filter(format(timestamp, "%H:%M") >= "10:00") %>%
  group_by(individual_local_identifier, date) %>%
  slice_min(timestamp, with_ties = FALSE) %>%
  ungroup()

# Run DBSCAN per date using Haversine distance
eps_thres <- 1500  # in meters
minPts <- 0

results_by_date <- data_filtered_midday %>%
  group_split(date) %>%
  map(~ {
    coords_clean <- select(.x, location.long, location.lat) %>% as.matrix()
    dist_matrix <- distm(coords_clean, fun = distHaversine)
    dbscan_result <- dbscan(dist_matrix, eps = eps_thres, minPts = minPts, borderPoints = TRUE)
    .x$cluster <- dbscan_result$cluster
    .x
  })

results_combined <- bind_rows(results_by_date)


library(geosphere)
library(dbscan)

# Choose a specific date
test_day <- as.Date("2025-08-18")  # replace with your actual date

data_test <- data_filtered_midday %>%
  filter(date == test_day)

coords_clean <- select(data_test, location.long, location.lat) %>% as.matrix()

dist_matrix <- distm(coords_clean, fun = distHaversine)

# Try a more permissive eps threshold (e.g., 50-100 meters)
eps_thres <- 1500  # meters
minPts <- 0

dbscan_result <- dbscan(dist_matrix, eps = eps_thres, minPts = minPts, borderPoints = TRUE)

data_test$cluster <- dbscan_result$cluster
table(data_test$cluster)


# Compute cluster centroids (excluding noise)
cluster_summary <- data_test %>%
  filter(cluster != 0) %>%
  group_by(cluster) %>%
  summarise(
    lon = mean(location.long),
    lat = mean(location.lat),
    count = n(),
    .groups = "drop"
  )

# Define a color palette by cluster ID
cluster_ids <- sort(unique(data_test$cluster))
pal <- colorFactor(palette = "Set1", domain = cluster_ids)

# Build the map
leaflet() %>%
  addProviderTiles("Esri.WorldImagery") %>%
  setView(lng = mean(data_test$location.long), lat = mean(data_test$location.lat), zoom = 12) %>%
  # Individual locations (white fill, colored by cluster)
  addCircleMarkers(data = data_test,
                   lng = ~location.long, lat = ~location.lat,
                   radius = 4,
                   color = ~pal(cluster),
                   fillColor = "white",
                   fillOpacity = 0.9, stroke = TRUE,
                   label = ~paste("ID:", individual_local_identifier, "<br>Cluster:", cluster),
                   popup = ~paste0("<strong>ID: </strong>", individual_local_identifier,
                                   "<br><strong>Cluster: </strong>", cluster,
                                   "<br><strong>Latitude: </strong>", round(location.lat, 5),
                                   "<br><strong>Longitude: </strong>", round(location.long, 5)),
                   group = "Individual Locations") %>%
  
  addLayersControl(
    overlayGroups = c("Cluster Centroids", "Individual Locations"),
    options = layersControlOptions(collapsed = FALSE)
  )
