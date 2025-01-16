days_window <- 4 # window in days - max distance 
# calc max distance from origin - daily 

max_distance_summary <- recent_data %>%
  arrange(tag_local_identifier, timestamp) %>%  # Ensure data is sorted by individual and time
  mutate(date = as.Date(timestamp)) %>%         # Extract date
  group_by(tag_local_identifier, date) %>%
  mutate(
    start_long = first(location.long),  # Get starting longitude for the day
    start_lat = first(location.lat),    # Get starting latitude for the day
    distance_from_start = distHaversine(
      cbind(start_long, start_lat), 
      cbind(location.long, location.lat)
    ) / 1000  # Convert meters to kilometers
  ) %>%
  summarize(
    max_distance_from_start = max(distance_from_start, na.rm = TRUE),  # Max distance from the start
    .groups = "drop"
  )

max_distance_summary <- max_distance_summary %>%
  arrange(tag_local_identifier, date) %>%  # Ensure the data is sorted
  group_by(tag_local_identifier) %>%       # Group by identifier
  mutate(
    max_last_days = map_dbl(date, function(current_date) {
      relevant_values <- max_distance_from_start[
        date < current_date & date >= current_date - days_window
      ]
      if (length(relevant_values) > 0) {
        max(relevant_values, na.rm = TRUE)
      } else {
        NA_real_  # Return NA if no relevant values
      }
    })
  ) %>%
  ungroup()  # Remove grouping


max_distance_summary <- max_distance_summary %>%
  arrange(tag_local_identifier, date) %>%  # Ensure the data is sorted
  group_by(tag_local_identifier) %>%       # Group by identifier
  mutate(
    max_last_days = map_dbl(date, function(current_date) {
      relevant_values <- max_distance_from_start[
        date < current_date & date >= current_date - days_window
      ]
      if (length(relevant_values) > 0) {
        max(relevant_values, na.rm = TRUE)
      } else {
        NA_real_  # Return NA if no relevant values
      }
    })
  ) %>%
  ungroup()  # Remove grouping

# Join the calculated max_last_days to daily_summary
daily_summary <- daily_summary %>%
  left_join(
    max_distance_summary %>% select(tag_local_identifier, date, max_last_days),
    by = c("tag_local_identifier", "date")
  )

## plot all tog.
{
# Prepare the data for plotting
plot_data <- daily_summary %>%
  filter(!is.na(max_last_days)) %>%  # Remove rows with missing distance values
  select(tag_local_identifier, group_id, date, max_last_days)

# Create the interactive plot
interactive_plot <- plot_ly(
  data = plot_data,
  x = ~date,
  y = ~max_last_days,
  color = ~tag_local_identifier,  # Different line per tag
  type = 'scatter',
  mode = 'lines+markers',
  line = list(width = 2)
) %>%
  layout(
    xaxis = list(title = "Date"),
    yaxis = list(title = "Last Days Max Distance (km)"),
    legend = list(title = list(text = "Tag"),
                  itemclick = "toggleothers"  # Clicking a line shows only that line
    )
  )

interactive_plot <- interactive_plot %>%
  add_trace(
    text = ~paste("Tag ID:", tag_local_identifier, "<br>Group ID:", group_id),
    hoverinfo = "text"  # Customize hover text
  )

# Show the plot
output_file <- paste0('plots/',as.Date(Sys.Date(), format = "%Y%m%d"),'/all_ind_distance_plot.html')
saveWidget(interactive_plot, file = output_file, selfcontained = TRUE)

}

## plot group by group

# Get unique group IDs
group_ids <- unique(plot_data$group_id)

# Loop through each group ID and create a plot
for (group in group_ids) {
  # Filter data for the current group
  group_data <- plot_data %>% filter(group_id == group)
  
  # Create the plot
  group_plot <- plot_ly(
    data = group_data,
    x = ~date,
    y = ~max_last_days ,
    color = ~as.factor(tag_local_identifier),  # Color by tag ID within the group
    type = 'scatter',
    mode = 'lines+markers',
    line = list(width = 2)
  ) %>%
    layout(
      title = paste("max_distance_from_start  - Group", group),
      xaxis = list(title = "Date"),
      yaxis = list(title = "max_distance_from_start  (km)"),
      legend = list(
        title = list(text = "Tag ID"),  # Display legend with Tag IDs
        itemclick = "toggleothers"  # Clicking a line shows only that line
        
      )
    )
  
  interactive_plot <- interactive_plot %>%
    add_trace(
      text = ~paste("Tag ID:", tag_local_identifier, "<br>Animal ID:", individual_local_identifier, "<br>Group ID:", group_id),
      hoverinfo = "text"  # Customize hover text
    )
  
  
  # Save the plot as an HTML file
  output_file <- paste0('plots/',"/group_", group, "_distance_plot.html")
  
  output_file <- paste0()
  saveWidget(group_plot, file = output_file, selfcontained = TRUE)
}

