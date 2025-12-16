cluster_groups <- function(dt, eps_thres = 0.0001, united_eps_thres = 0.001, minPts = 5, plot_map = TRUE) {
  library(data.table)
  library(dbscan)
  library(geosphere)
  library(dplyr)
  library(leaflet)
  library(htmlwidgets)
  
  setDT(dt)
  dt <- dt[, .(animal_id, group_id, location.lat, location.long, timestamp)]
  
  coords <- as.matrix(dt[, c("location.lat", "location.long")])
  coords_clean <- na.omit(coords)
  valid_rows <- complete.cases(dt[, c("location.lat", "location.long")])
  
  ind_ids <- dt$animal_id[valid_rows]
  group_ids <- dt$group_id[valid_rows]
  dates <- as.Date(dt$timestamp[valid_rows])
  
  dist_matrix <- distm(coords_clean, fun = distHaversine)
  dbscan_result <- dbscan(coords_clean, eps = eps_thres, minPts = minPts, borderPoints = TRUE)
  
  clustered_data <- data.frame(
    individual_id = ind_ids,
    group_id = group_ids,
    date = dates,
    lat = coords_clean[, 1],
    lon = coords_clean[, 2],
    cluster = dbscan_result$cluster
  ) %>% filter(cluster != 0)
  
  cluster_summary <- clustered_data %>%
    group_by(cluster) %>%
    summarise(
      lat = mean(lat),
      lon = mean(lon),
      count = n(),
      .groups = "drop"
    ) %>%
    mutate(cluster = as.character(cluster))
  
  individual_night_locations <- clustered_data %>%
    dplyr::select(date, cluster, lat, lon, individual_id, group_id) %>%
    mutate(cluster = as.character(cluster))
  
  
  db_united <- dbscan(cluster_summary[, c("lon", "lat")], eps = united_eps_thres, minPts = 1, borderPoints = TRUE)
  cluster_summary$cluster_united <- as.factor(db_united$cluster)
  
  cluster_merged_summary <- cluster_summary %>%
    group_by(cluster_united) %>%
    summarise(
      lat = weighted.mean(lat, log(count)),
      lon = weighted.mean(lon, log(count)),
      count = sum(count),
      .groups = "drop"
    )
  
  individual_night_locations <- individual_night_locations %>%
    left_join(cluster_summary %>% 
                dplyr::select(cluster, cluster_united), by = "cluster")
  
  # Optional map generation
  if (plot_map) {
    map <- leaflet() %>%
      addProviderTiles("Esri.WorldImagery") %>%
      setView(lng = mean(cluster_merged_summary$lon), lat = mean(cluster_merged_summary$lat), zoom = 12) %>%
      addCircleMarkers(data = cluster_merged_summary,
                       lng = ~lon, lat = ~lat,
                       radius = 6, color = "blue", fillColor = "lightblue",
                       fillOpacity = 0.8, stroke = TRUE,
                       label = ~paste("Cluster", cluster_united),
                       popup = ~paste0("<strong>Cluster: </strong>", cluster_united,
                                       "<br><strong>Count: </strong>", count,
                                       "<br><strong>Latitude: </strong>", round(lat, 5),
                                       "<br><strong>Longitude: </strong>", round(lon, 5)),
                       group = "Merged Clusters") %>%
      addCircleMarkers(data = cluster_summary,
                       lng = ~lon, lat = ~lat,
                       radius = 4, color = "black", fillColor = "black",
                       fillOpacity = 0.6, stroke = TRUE,
                       label = ~paste("Cluster - raw", cluster, "Cluster - united", cluster_united),
                       popup = ~paste0("<strong>Cluster: </strong>", cluster_united,
                                       "<br><strong>Count: </strong>", count,
                                       "<br><strong>Latitude: </strong>", round(lat, 5),
                                       "<br><strong>Longitude: </strong>", round(lon, 5)),
                       group = "Raw Clusters") %>%
      addCircleMarkers(data = individual_night_locations,
                       lng = ~lon, lat = ~lat,
                       radius = 2, color = "white", fillColor = "white",
                       fillOpacity = 0.9, stroke = FALSE,
                       label = ~paste("ID:", individual_id, "date:", date, "group:", group_id),
                       group = "Individual Nights") %>%
      addLayersControl(
        overlayGroups = c("Merged Clusters", "Raw Clusters", "Individual Nights"),
        options = layersControlOptions(collapsed = FALSE)
      )
  } else {
    map <- NULL
  }
  
  return(list(
    cluster_summary = cluster_summary,
    individual_night_locations = individual_night_locations,
    cluster_merged_summary = cluster_merged_summary,
    map = map
  ))
}