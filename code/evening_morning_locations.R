
# Morning locations (first fix of the next day after 04:00)
morning_locations <- cleaned_data %>%
  mutate(date = date(timestamp),
         prev_date = date - days(1)) %>%
  filter(format(timestamp, "%H:%M") >= "02:00") %>%
  group_by(individual_local_identifier, prev_date) %>%
  slice_head(n = 1) %>%
  ungroup() %>%
  select(individual_local_identifier, prev_date, date_morning = date,
         lat_mor = location.lat, lon_mor = location.long,
         time_mor = timestamp)

# Join night and morning locations
night_with_morning <- data_filtered_night %>%
  left_join(morning_locations,
            by = c("individual_local_identifier", "date" = "prev_date"))

# Plot with connecting lines and group color

# Create ggplot with tooltip aesthetics
p <- ggplot(night_with_morning) +
  geom_point(aes(x = location.long, y = location.lat,
                 color = tag_local_identifier,
                 text = paste("Evening",
                              "<br>ID:", individual_local_identifier,
                              "<br>Date:", date)),
             size = 2) +
  geom_point(aes(x = lon_mor, y = lat_mor,
                 color = tag_local_identifier,
                 text = paste("Morning",
                              "<br>ID:", individual_local_identifier,
                              "<br>Date:", date + 1)),
             shape = 17, size = 2) +
  geom_segment(aes(x = location.long, y = location.lat,
                   xend = lon_mor, yend = lat_mor,
                   color = tag_local_identifier,
                   text = paste("Transition",
                                "<br>ID:", individual_local_identifier,
                                "<br>Date:", date)),
               arrow = arrow(length = unit(0.2, "cm")),
               linetype = "dashed") +
  labs(title = "Evening to Morning Transitions by Group",
       x = "Longitude", y = "Latitude", color = "Group ID") +
  theme_minimal() +
  theme(legend.position = "none")

# Create the interactive plot
p <- ggplotly(p, tooltip = "text")

# Save to HTML
saveWidget(p, file = "plots/htmls/evening_morning_transitions.html")




ggplot(night_with_morning) +
  geom_point(aes(x = location.long, y = location.lat, color = tag_local_identifier), size = 2) +
  geom_point(aes(x = lon_mor, y = lat_mor, color = tag_local_identifier), shape = 17, size = 2) +
  geom_segment(aes(x = location.long, y = location.lat,
                   xend = lon_mor, yend = lat_mor,
                   color = tag_local_identifier),
               arrow = arrow(length = unit(0.2, "cm")),
               linetype = "dashed") +
  labs(title = "Evening to Morning Transitions by Group",
       x = "Longitude", y = "Latitude", color = "Group ID") +
  theme_minimal() +
  theme(legend.position = "none")


