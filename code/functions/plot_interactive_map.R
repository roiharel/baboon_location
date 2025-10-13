# Main function to plot interactive map
plot_interactive_map <- function(cleaned_data, output_file, color_mapping) {
  cleaned_data <- cleaned_data %>%
    filter(!is.na(group_id))
  
  names_plot <- unique(sort(cleaned_data$group_id))
  
  m <- leaflet() %>%
    addTiles(group = "OSM") %>%
    addProviderTiles(providers$Esri.WorldTopoMap, group = "Topo") %>%
    addProviderTiles(providers$Esri.WorldImagery, group = "Terrain", options = providerTileOptions(noWrap = TRUE))
  
  for(id in group_ids) {
    data_subset <- subset(cleaned_data, group_id == id)
    m <- m %>%
      addCircleMarkers(data = data_subset, ~location.long, ~location.lat, 
                       color = unname(color_mapping[data_subset$group_id]), 
                       opacity = .4, fillOpacity = .4,
                       radius = .5, 
                       group = as.character(id))
  }
  
  m <- m %>% onRender("
    function(el, x) {
      var map = this;
      map.on('click', function(e) {
        var lat = e.latlng.lat.toFixed(5);
        var lon = e.latlng.lng.toFixed(5);
        var popup = L.popup()
          .setLatLng(e.latlng)
          .setContent(lat + ', ' + lon)
          .openOn(map);
      });
    }
  ")
  m <- m %>%
    addLayersControl(
      baseGroups = c("OSM", "Topo", "Terrain"),
      overlayGroups = group_ids,
      options = layersControlOptions(collapsed = FALSE)
    )
  
  saveWidget(m, output_file, selfcontained = TRUE)
}
