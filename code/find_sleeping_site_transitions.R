library(dplyr)
library(leaflet)
library(tidyr)
library(RColorBrewer)
library(dbscan)
library(geosphere)
library(lubridate)
library(htmlwidgets)
library(scales)


# Example input: clustered_data
# Make sure 'date' is Date type
clustered_data <- individual_night_locations %>%
  dplyr::mutate(date = as.Date(date))

# Sort and calculate transitions
transitions <- clustered_data %>%
  dplyr::arrange(group_id, individual_id, date) %>%
  dplyr::group_by(individual_id) %>%
  dplyr::mutate(
    next_cluster = lead(cluster_united),
    next_lat = lead(lat),
    next_lon = lead(lon),
    next_date = lead(date)
  ) %>%
  dplyr::filter(!is.na(next_cluster)) %>%
  ungroup()

# Get unique clusters and their median locations
clusters <- clustered_data %>%
  dplyr::group_by(cluster_united) %>%
  dplyr::summarise(lat = median(lat), 
                   lon = median(lon),
                   count = n()) %>%
  dplyr::mutate(
    prop = count / sum(count),  # Optional: raw proportion
    radius = rescale(count, to = c(3, 10))  # Scaled for visualization
  )

# Step 2: Compute proportion of each transition per individual
transition_counts <- transitions %>%
  dplyr::group_by(group_id, individual_id, from = cluster_united, to = next_cluster) %>%
  dplyr::summarise(
    count = dplyr::n(),
    from_lon = dplyr::first(lon),
    from_lat = dplyr::first(lat),
    to_lon = dplyr::first(next_lon),
    to_lat = dplyr::first(next_lat),
    distance_m = geosphere::distHaversine(
      c(first(lon), first(lat)),
      c(first(next_lon), first(next_lat))
    ),
    .groups = "drop"
  )

transition_props <- transition_counts %>%
  dplyr::group_by(group_id, individual_id) %>%
  dplyr::mutate(
    prop = count / sum(count),
    weight = 2 + 8 * prop  # Line thickness from 2 to 10
  ) %>%
  dplyr::ungroup()

# Assign color per individual
group_ids <- unique(clustered_data$group_id)
group_base_colors <- RColorBrewer::brewer.pal(min(8, length(group_ids)), "Dark2")
group_palettes <- setNames(group_base_colors[1:length(group_ids)], group_ids)

# Step 2: Assign individuals shades within each group
individual_colors <- clustered_data %>%
  distinct(group_id, individual_id) %>%
  dplyr::group_by(group_id) %>%
  dplyr::mutate(
    # Create a gradient palette for individuals in the group
    color = colorRampPalette(c("white", group_palettes[group_id[1]]))(n())[row_number()]
  ) %>%
  ungroup() %>%
  { setNames(.$color, .$individual_id) }

# Step 3: Create leaflet map
m <- leaflet() %>%
  addTiles() %>%
  setView(lng = mean(clustered_data$lon), lat = mean(clustered_data$lat), zoom = 12)

m <- m %>%
  addCircleMarkers(
    data = clusters,
    lng = ~lon,
    lat = ~lat,
    radius = ~radius,
    color = "black",
    fillColor = "black",
    fillOpacity = 1,
    stroke = FALSE, 
    label = ~paste("Cluster", cluster_united)
  )

# Step 4: Add polylines by group layer
layer_names <- c()

for (grp in unique(transition_props$group_id)) {
  layer_name <- paste(grp)
  layer_names <- c(layer_names, layer_name)
  
  group_trans <- transition_props %>% filter(group_id == grp)
  
  for (i in seq_len(nrow(group_trans))) {
    row <- group_trans[i, ]
    
    m <- addPolylines(
      m,
      lng = c(row$from_lon, row$to_lon),
      lat = c(row$from_lat, row$to_lat),
      color = individual_colors[[as.character(row$individual_id)]],
      weight = row$weight,
      opacity = 0.8,
      label = paste0(
        row$individual_id, ",", row$group_id, ": ", row$from, " → ", row$to,
        " (", round(row$prop * 100, 1), "%)"
      ),
      group = layer_name
    )
  }
}

# Step 5: Add layer control to toggle groups
m <- m %>%
  addLayersControl(
    overlayGroups = layer_names,
    options = layersControlOptions(collapsed = FALSE)
  )

# Show map
saveWidget(m, file = "transitions_sleeping_sites_map_satellite.html", selfcontained = TRUE)

#################  GROUP LEVEL - CURVED LINES TRANSITIONS 


group_names <- unique(group_transitions$group_id)

group_transitions <- transitions %>%
  dplyr::group_by(group_id, from = cluster, to = next_cluster) %>%
  dplyr::summarise(
    count = dplyr::n(),
    from_lat = dplyr::first(lat),
    from_lon = dplyr::first(lon),
    to_lat = dplyr::first(next_lat),
    to_lon = dplyr::first(next_lon),
    .groups = "drop"
  ) %>%
  dplyr::group_by(group_id) %>%
  dplyr::mutate(
    # All transitions (including self)
    prop = count / sum(count),
    weight = 3 + 12 * prop,
    
    # Non-self transitions only
    total_no_self = sum(count[from != to]),
    prop_no_self = dplyr::if_else(from != to, count / total_no_self, 0),
    weight_no_self = dplyr::if_else(from != to, 1 + 4 * log1p(count), 0)
  ) %>%
  dplyr::ungroup()

# Function to compute a curved line between two points
make_curve <- function(from_lon, from_lat, to_lon, to_lat, n = 50, curve_height = 0.1) {
  # Midpoint
  mid_lat <- (from_lat + to_lat) / 2
  mid_lon <- (from_lon + to_lon) / 2
  
  # Add curvature (shift midpoint)
  dx <- to_lon - from_lon
  dy <- to_lat - from_lat
  curve_lon <- mid_lon - dy * curve_height
  curve_lat <- mid_lat + dx * curve_height
  
  # Bezier interpolation
  t_vals <- seq(0, 1, length.out = n)
  lats <- (1 - t_vals)^2 * from_lat + 2 * (1 - t_vals) * t_vals * curve_lat + t_vals^2 * to_lat
  lons <- (1 - t_vals)^2 * from_lon + 2 * (1 - t_vals) * t_vals * curve_lon + t_vals^2 * to_lon
  
  data.frame(lat = lats, lon = lons)
}

m <- leaflet() %>%
  addTiles(group = "OSM") %>%
  addProviderTiles(providers$Esri.WorldTopoMap, group = "Topo") %>%
  addProviderTiles(providers$Esri.WorldImagery, group = "Terrain", options = providerTileOptions(noWrap = TRUE)) %>% 
  setView(lng = mean(clustered_data$lon), lat = mean(clustered_data$lat), zoom = 12)

# Plot curved transitions by group
group_colors <- colorFactor("Dark2", domain = group_names)

for (i in seq_len(nrow(group_transitions))) {
  row <- group_transitions[i, ]
  
  curve_coords <- make_curve(
    from_lon = row$from_lon,
    from_lat = row$from_lat,
    to_lon = row$to_lon,
    to_lat = row$to_lat
  )
  
  m <- addPolylines(
    m,
    lng = curve_coords$lon,
    lat = curve_coords$lat,
    weight = row$weight_no_self,
    color = group_colors(row$group_id),
    opacity = 0.8,
    label = paste0(
      row$group_id, ": ",
      row$from, " → ", row$to,
      " (", round(row$prop_no_self * 100, 1), "%)"
    ),
    group = row$group_id
  )
}

m <- m %>%
  addCircleMarkers(
    data = clusters,
    lng = ~lon,
    lat = ~lat,
    radius = ~radius,
    color = "black",
    fillColor = "black",
    fillOpacity = 1,
    label = ~paste("Cluster", cluster_united)
  )

m <- m %>%
  addLayersControl(
    baseGroups = c("OSM", "Topo", "Terrain"),
    overlayGroups = as.character(group_names),
    options = layersControlOptions(collapsed = FALSE)
  )

saveWidget(m, file = "transitions_group_sleeping_sites_map_satellite.html", selfcontained = TRUE)

