## code is running some basic visualizations of MBRP data collected since Mar 2024
## the input is based on connection to movebank 
# install packages
{
  #library(moveVis)
  library(move2)
  library(ggplot2)
  library(lubridate)
  library(dplyr)
  library(ggmap)
  library(maps)
  
  library(sf)
  library(mapview)
  library(webshot)
  library(leaflet)
  library(htmlwidgets)
  library(RColorBrewer)
  library(units)
}
# parameters - fill in details 
{  
#movebank_store_credentials("USER", "PASSWORD", force = TRUE)
#ggmap::register_google(key = "KEY")
  time_interval <- "10 mins"
  date_start <- as.POSIXct("2024-03-01 00:00:00")
}
## load data
{
  baboon_data_2024 <- movebank_download_study(3445611111, sensor_type_id = c("gps"), 
                                              timestamp_start = date_start,
                                             # individual_id = c(3487912671, 3487912662, 3487912663, 3487844492, 3508338112, 3938663627),
                                              remove_movebank_outliers = TRUE)
  metadata_2024 <- mt_track_data(baboon_data_2024)
  
  baboon_data_2024 <- baboon_data_2024 %>%
    left_join(metadata_2024 %>% select(individual_local_identifier, tag_local_identifier, group_id, sex), by = c("individual_local_identifier" = "individual_local_identifier"))  %>%
    mt_filter_per_interval(unit = time_interval)
  
  baboon_data_2024$location.long <- sf::st_coordinates(baboon_data_2024)[,1]
  baboon_data_2024$location.lat <- sf::st_coordinates(baboon_data_2024)[,2]
  baboon_data_2024$group_id <- baboon_data_2024$group_id
  

   # Step 1: Identify matching columns
matching_columns <- Reduce(intersect, list(names(baboon_data_2024)))
  

# Step 2: Join tibbles while keeping only matching columns
combined_data <- bind_rows(
  select(as.data.frame(baboon_data_2024), matching_columns)
)


combined_data <- combined_data %>%
  mutate(plot_name = paste(group_id, year(timestamp), sep = "_")) %>%
  filter(plot_name != "Campsite_2020") %>%
  filter(tag_local_identifier != 6915)

matching_columns <- Reduce(intersect, list(names(mt_track_data(baboon_data_2024))))

# Step 2: Join tibbles while keeping only matching columns
combined_metatdata <- bind_rows(
  select(mt_track_data(baboon_data_2024), matching_columns)
)

# combined_data <- combined_data %>%
#   filter(group_id %in% c("Mlimafisi", "Clifford", "Leikiji"))

#baboon_data <- movebank_download_study(3445611111, sensor_type_id = c("gps"))
#metadata <- mt_track_data(baboon_data_2019)

}
## plot data dist
{

  recent_data <- combined_data[combined_data$timestamp > date_start,]
  recent_data$tag_local_identifier <- with(recent_data, reorder(tag_local_identifier, group_id))
  
  ordered_levels <- recent_data %>%
    arrange(group_id) %>%
    pull(tag_local_identifier) %>%
    unique()
  
  recent_data$tag_local_identifier <- factor(recent_data$tag_local_identifier, levels = ordered_levels)
  
  records <- ggplot(recent_data, 
       aes(x = timestamp, 
           y = tag_local_identifier,
           color = group_id)) +
       geom_point() +
       labs(x = "timestamp", y = "tagID") 
  
  ggsave(paste(as.Date(Sys.Date(), format = "%Y%m%d"),'_baboon_data_records.png'), plot = records, width = 10, height = 8, dpi = 300)
  

}
## batt trend
{

  recent_data <- combined_data[combined_data$timestamp > date_start,]
  recent_data <- recent_data[as.numeric(recent_data$eobs_battery_voltage) < 3800,]
 
  recent_data$tag_local_identifier <- with(recent_data, reorder(tag_local_identifier, group_id))
  
  ordered_levels <- recent_data %>%
    arrange(group_id) %>%
    pull(tag_local_identifier) %>%
    unique()
  
  recent_data$tag_local_identifier <- factor(recent_data$tag_local_identifier, levels = ordered_levels)
  
  records <- ggplot(recent_data, 
       aes(x = timestamp, 
           y = eobs_battery_voltage,
           color = tag_local_identifier)) +
       geom_point() +
       labs(x = "timestamp", y = "tagID") 
  
  ggsave(paste(as.Date(Sys.Date(), format = "%Y%m%d"),'_baboon_batt_records.png'), plot = records, width = 10, height = 8, dpi = 300)
  

}
## plot map interactive
{
# Generate a list of unique identifiers
  #unique_ids <- unique(combined_data$individual_local_identifier)
  #combined_data <- combined_data[combined_data$timestamp > as.Date("2024-03-01 00:00:00 CET"),]

 # combined_data <- combined_data %>%
 #    filter(group_id %in% c("Mlimafisi", "Leikiji"))
  # names for legend
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
  saveWidget(m, paste(as.Date(Sys.Date(), format = "%Y%m%d"),'_baboon_interactive_map_2024.html'), selfcontained = TRUE)

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
saveWidget(m, paste(as.Date(Sys.Date(), format = "%Y%m%d"),'_baboon_night_interactive_map.html'), selfcontained = TRUE)
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
    saveWidget(m, paste(as.Date(Sys.Date(), format = "%Y%m%d"),'_baboon_night_interactive_map.html'), selfcontained = TRUE)
  }
## plot map interactive - a specific variation between individuals - campsite
{
    # Generate a list of unique identifiers
    #unique_ids <- unique(combined_data$individual_local_identifier)
    #combined_data <- combined_data[combined_data$timestamp > as.Date("2024-03-01 00:00:00 CET"),]
    
    combined_data <- combined_data %>%
      filter(group_id %in% c("Campsite"))
    # names for legend
    names_plot <- unique(sort(combined_data$individual_local_identifier ))
    # Create a color palette
    pallete <- colorFactor("BuGn", domain = unique(sort(combined_data$timestamp)))
    
    
    # Loop through each unique identifier to create a layer for each
    # Create the basic Leaflet map
    m <- leaflet() %>%
      addTiles(group = "OSM") %>%
      addProviderTiles(providers$Esri.WorldTopoMap, group = "Topo") %>%
      addProviderTiles(providers$Esri.WorldImagery, group = "Terrain", options = providerTileOptions(noWrap = TRUE))    
    
    
    
    for(id in names_plot) {
      data_subset <- subset(combined_data, individual_local_identifier == id) 
      
      m <- m %>%
        addCircleMarkers(data = data_subset, ~location.long, ~location.lat, 
                         color = ~pallete(timestamp), 
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
    saveWidget(m, paste(as.Date(Sys.Date(), format = "%Y%m%d"),'_baboon_interactive_map_2024.html'), selfcontained = TRUE)
    
  }
## plot map interactive - battery
{
  # Install plotly if you haven't already
  install.packages("plotly")
  
  # Load the necessary libraries
  library(ggplot2)
  library(plotly)
  
  # Your ggplot code
  records <- ggplot(recent_data, 
                    aes(x = timestamp, 
                        y = eobs_battery_voltage,
                        color = tag_local_identifier,
                        text = tag_local_identifier)) +  # Add text aesthetic for tooltip
    geom_point() +
    labs(x = "timestamp", y = "tagID")
  
  # Convert ggplot to plotly
  interactive_plot <- ggplotly(records, tooltip = "text")
  
  # Display the interactive plot
  interactive_plot
}
  