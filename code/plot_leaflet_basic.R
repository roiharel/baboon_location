## plot map interactive
{
  # Generate a list of unique identifiers
  #unique_ids <- unique(combined_data$individual_local_identifier)
  #combined_data <- combined_data[combined_data$timestamp > as.Date("2024-03-01 00:00:00 CET"),]
  
  # combined_data <- combined_data %>%
  #    filter(group_id %in% c("Mlimafisi", "Leikiji"))
  # names for legend
  
  combined_data <- combined_data %>%
    filter(!is.na(group_id))
  
  names_plot <- unique(sort(combined_data$plot_name))
  # Create a color palette
  pallete <- colorFactor("Set1", domain = names_plot)
  
  
  # Loop through each unique identifier to create a layer for each
  # Create the basic Leaflet map
  m <- leaflet() %>%
    addTiles(group = "OSM") %>%
    addProviderTiles(providers$Esri.WorldTopoMap, group = "Topo") %>%
    addProviderTiles(providers$Esri.WorldImagery, group = "Terrain", options = providerTileOptions(noWrap = TRUE))    
  
  
  
  for(id in names_plot) {
    data_subset <- subset(combined_data, plot_name == id) 
    
    m <- m %>%
      addCircleMarkers(data = data_subset, ~location.long, ~location.lat, 
                       color = ~pallete(id), 
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
  
  # Add layer control
  m <- m %>%
    addLayersControl(
      baseGroups = c("OSM", "Topo", "Terrain"),
      overlayGroups = as.character(names_plot),
      options = layersControlOptions(collapsed = FALSE)
    )
  
  
  # Print the map
  m
  # Save the map as an HTML file
  saveWidget(m, paste0('plots/',as.Date(Sys.Date(), format = "%Y%m%d"),'/baboon_interactive_map_2024.html'), selfcontained = TRUE)
  
}
  ## plot night site interactive by individual
  {
    # Assuming `data_rm_rest_group_members` has a column `timestamp` of class POSIXct
    
    data_filtered_night <- combined_data %>%
      mutate(date_val = as.Date(timestamp)) %>%
      filter(date_val > ymd(date_start)) %>%
      group_by(individual_local_identifier, date(timestamp)) %>%
      slice(n()) %>%
      ungroup()
    
    data_filtered_night<- data_filtered_night  %>%
      filter(format(timestamp, "%H:%M") >= "15:50")
    
    most_recent_timestamp <- max(data_filtered_night$timestamp, na.rm = TRUE)
    data_filtered_night <- data_filtered_night %>%
      mutate(days_ago = as.numeric(difftime(most_recent_timestamp, timestamp, units = "days")))
    
    # Normalize 'days ago' to an opacity value between 0.3 and 1
    # The oldest data (max days ago) will have opacity = 0.3, and the most recent data (0 days ago) will have opacity = 1
    # max_days_ago <- max(data_filtered_night$days_ago, na.rm = TRUE)
    # data_filtered_night <- data_filtered_night %>%
    #   mutate(opacity = 1 - (days_ago / max_days_ago * 0.9),
    #          opacity = ifelse(opacity < 0.5, 0.5, opacity)) # Ensure opacity does not go below 0.3
    
    
    # Create a color palette
    palette <- colorFactor("Set1", domain = names_plot)
    
    
    # Create the basic Leaflet map
    m <- leaflet() %>%
      addTiles(group = "OSM") %>%
      addProviderTiles(providers$Esri.WorldTopoMap, group = "Topo") %>%
      addProviderTiles(providers$Esri.WorldImagery, group = "Terrain", options = providerTileOptions(noWrap = TRUE))    
    
    #     addTiles(options = providerTileOptions(opacity = 1))
    
    for(id in names_plot) {
      data_subset <- data_filtered_night %>%
        filter(plot_name == id) %>%
        group_by(individual_local_identifier, day = date(timestamp)) %>%
        summarise(location.lat = first(location.lat), 
                  location.long = first(location.long),
                  date_label = first(format(timestamp, "%Y-%m-%d")),
                  #          opacity = opacity,# Format the date as desired
                  .groups = 'drop')
      
      if (nrow(data_subset) > 0) {
        m <- m %>%
          addCircleMarkers(data = data_subset, ~location.long, ~location.lat, 
                           color = ~palette(id), 
                           #                 opacity = 0, fillOpacity = ~opacity,
                           radius = 6, 
                           group = as.character(id), 
                           label = ~date_label)
      }
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
    
    # Add layer control
    m <- m %>%
      addLayersControl(
        baseGroups = c("OSM", "Topo", "Terrain"),
        overlayGroups = as.character(names_plot),
        options = layersControlOptions(collapsed = FALSE)
      )
    
    # Print the map
    m
    
    # Save the map as an HTML file
    saveWidget(m, paste0('plots/',as.Date(Sys.Date(), format = "%Y%m%d"),'/baboon_night_interactive_map.html'), selfcontained = TRUE)
  }
  ## plot night site interactive by group
  {
    # Assuming `data_rm_rest_group_members` has a column `timestamp` of class POSIXct
    
    data_filtered_night <- combined_data %>%
      mutate(date_val = as.Date(timestamp)) %>%
      filter(date_val > ymd(date_start)) %>%
      group_by(individual_local_identifier, date(timestamp)) %>%
      slice(n()) %>%
      ungroup()
    
    data_filtered_night<- data_filtered_night  %>%
      filter(format(timestamp, "%H:%M") >= "15:50")
    
    most_recent_timestamp <- max(data_filtered_night$timestamp, na.rm = TRUE)
    data_filtered_night <- data_filtered_night %>%
      mutate(days_ago = as.numeric(difftime(most_recent_timestamp, timestamp, units = "days")))  
    
    data_filtered_night <- data_filtered_night %>%
      group_by(group_id, date_val) %>%  # Group by group_id (individual_local_identifier) and date_val
      slice(1) %>%                               # Keep only the first row per group_id per date_val
      ungroup() 
    
    # Normalize 'days ago' to an opacity value between 0.3 and 1
    # The oldest data (max days ago) will have opacity = 0.3, and the most recent data (0 days ago) will have opacity = 1
    # max_days_ago <- max(data_filtered_night$days_ago, na.rm = TRUE)
    # data_filtered_night <- data_filtered_night %>%
    #   mutate(opacity = 1 - (days_ago / max_days_ago * 0.9),
    #          opacity = ifelse(opacity < 0.5, 0.5, opacity)) # Ensure opacity does not go below 0.3
    
    
    # Create a color palette
    palette <- colorFactor("Set1", domain = names_plot)
    
    
    # Create the basic Leaflet map
    m <- leaflet() %>%
      addTiles(group = "OSM") %>%
      addProviderTiles(providers$Esri.WorldTopoMap, group = "Topo") %>%
      addProviderTiles(providers$Esri.WorldImagery, group = "Terrain", options = providerTileOptions(noWrap = TRUE))    
    
    #     addTiles(options = providerTileOptions(opacity = 1))
    
    for(id in names_plot) {
      data_subset <- data_filtered_night %>%
        filter(plot_name == id) %>%
        group_by(individual_local_identifier, day = date(timestamp)) %>%
        summarise(location.lat = first(location.lat), 
                  location.long = first(location.long),
                  date_label = first(format(timestamp, "%Y-%m-%d")),
                  #          opacity = opacity,# Format the date as desired
                  .groups = 'drop')
      
      if (nrow(data_subset) > 0) {
        m <- m %>%
          addCircleMarkers(data = data_subset, ~location.long, ~location.lat, 
                           color = ~palette(id), 
                           #                 opacity = 0, fillOpacity = ~opacity,
                           radius = 6, 
                           group = as.character(id), 
                           label = ~date_label)
      }
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
    
    # Add layer control
    m <- m %>%
      addLayersControl(
        baseGroups = c("OSM", "Topo", "Terrain"),
        overlayGroups = as.character(names_plot),
        options = layersControlOptions(collapsed = FALSE)
      )
    
    # Print the map
    m
    
    # Save the map as an HTML file
    saveWidget(m, paste0('plots/',as.Date(Sys.Date(), format = "%Y%m%d"),'/baboon_night_interactive_map.html'), selfcontained = TRUE)
  }
  ## plot map interactive - a specific variation between individuals - campsite
#   {
#     # Generate a list of unique identifiers
#     #unique_ids <- unique(combined_data$individual_local_identifier)
#     #combined_data <- combined_data[combined_data$timestamp > as.Date("2024-03-01 00:00:00 CET"),]
#     
#     combined_data_grp <- combined_data %>%
#       filter(group_id %in% c("Campsite"))
#     # names for legend
#     names_plot <- unique(sort(combined_data$individual_local_identifier ))
#     # Create a color palette
#     pallete <- colorFactor("BuGn", domain = unique(sort(combined_data$timestamp)))
#     
#     
#     # Loop through each unique identifier to create a layer for each
#     # Create the basic Leaflet map
#     m <- leaflet() %>%
#       addTiles(group = "OSM") %>%
#       addProviderTiles(providers$Esri.WorldTopoMap, group = "Topo") %>%
#       addProviderTiles(providers$Esri.WorldImagery, group = "Terrain", options = providerTileOptions(noWrap = TRUE))    
#     
#     
#     
#     for(id in names_plot) {
#       data_subset <- subset(combined_data_grp, individual_local_identifier == id) 
#       
#       m <- m %>%
#         addCircleMarkers(data = data_subset, ~location.long, ~location.lat, 
#                          color = ~pallete(timestamp), 
#                          opacity = .4, fillOpacity = .4,
#                          radius = .5, 
#                          group = as.character(id))
#     }
#     
#     
#     m <- m %>% onRender("
# function(el, x) {
#   var map = this;
#   map.on('click', function(e) {
#     var lat = e.latlng.lat.toFixed(5);
#     var lon = e.latlng.lng.toFixed(5);
#     var popup = L.popup()
#       .setLatLng(e.latlng)
#       .setContent(lat + ', ' + lon)
#       .openOn(map);
#   });
# }
# ")
#     
#     # Add layer control
#     m <- m %>%
#       addLayersControl(
#         baseGroups = c("OSM", "Topo", "Terrain"),
#         overlayGroups = as.character(names_plot),
#         options = layersControlOptions(collapsed = FALSE)
#       )
#     
#     
#     # Print the map
#     m
#     # Save the map as an HTML file
#     saveWidget(m, paste0('plots/',as.Date(Sys.Date(), format = "%Y%m%d"),'/baboon_interactive_group_map_2024.html'), selfcontained = TRUE)
#     
#   }