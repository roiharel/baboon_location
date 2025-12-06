
# Nighttime locations (last fix of the day after 15:50)
data_filtered_night <- cleaned_data %>%
  mutate(date = date(timestamp)) %>%
  group_by(individual_local_identifier, date) %>%
  slice_tail(n = 1) %>%
  ungroup() %>%
  filter(format(timestamp, "%H:%M") >= "15:50")

# Morning locations (first fix of the next day after 02:00)
morning_locations <- cleaned_data %>%
  mutate(date = date(timestamp),
         prev_date = date - days(1)) %>%
  filter(format(timestamp, "%H:%M") >= "02:00") %>%
  group_by(individual_local_identifier, prev_date) %>%
  slice_head(n = 1) %>%
  ungroup() %>%
  select(individual_local_identifier, prev_date,
         date_morning = date,
         lat_mor = location.lat, lon_mor = location.long,
         time_mor = timestamp)

# Join night and morning locations
night_with_morning <- data_filtered_night %>%
  left_join(morning_locations,
            by = c("individual_local_identifier", "date" = "prev_date")) %>%
  mutate(distance_m = distHaversine(
    cbind(location.long, location.lat),
    cbind(lon_mor, lat_mor)
  ))

# Create popup text
night_with_morning <- night_with_morning %>%
  mutate(popup_text = paste0(
    "<b>ID:</b> ", individual_local_identifier,
    "<br><b>Group:</b> ", tag_local_identifier,
    "<br><b>Evening:</b> ", format(timestamp, "%Y-%m-%d %H:%M:%S"),
    "<br><b>Morning:</b> ", format(time_mor, "%Y-%m-%d %H:%M:%S"),
    "<br><b>Distance:</b> ", round(distance_m, 1), " m"
  ))

# Create leaflet map
m <- leaflet(data = night_with_morning) %>%
  addTiles() %>%
  setView(lng = mean(night_with_morning$location.long),
          lat = mean(night_with_morning$location.lat),
          zoom = 12) %>%
  addCircleMarkers(lng = ~location.long, lat = ~location.lat,
                   color = "blue", radius = 5,
                   popup = ~popup_text,
                   group = "Evening") %>%
  addCircleMarkers(lng = ~lon_mor, lat = ~lat_mor,
                   color = "orange", radius = 5,
                   popup = ~popup_text,
                   group = "Morning") %>%
  addPolylines(lng = ~c(location.long, lon_mor),
               lat = ~c(location.lat, lat_mor),
               color = "gray", weight = 2,
               popup = ~popup_text,
               group = "Transition") %>%
  addLayersControl(
    overlayGroups = c("Evening", "Morning", "Transition"),
    options = layersControlOptions(collapsed = FALSE)
  )

ggplot(night_with_morning, aes(x = distance_m, color = group_id)) +
  geom_density(alpha = 0.6, linewidth = 2) +
  xlim(0, 500) +
  scale_color_manual(values = color_mapping) +
  labs(title = "Density of Night-to-Morning Distances by Group",
       x = "Distance (m)", y = "Density") +
  theme_minimal() 


# Save Leaflet map to HTML
saveWidget(m, file = "plots/htmls/evening_morning_leaflet.html")
