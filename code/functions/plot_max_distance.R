plot_max_distance <- function(cleaned_data, daily_summary, days_window, color_mapping) {
  # Calculate max distance summary
  max_distance_summary <- cleaned_data %>%
    dplyr::arrange(tag_id, timestamp) %>%
    dplyr::mutate(date = as.Date(timestamp)) %>%
    dplyr::group_by(tag_id, date) %>%
    dplyr::mutate(
      start_long = first(location.long),
      start_lat = first(location.lat),
      distance_from_start = distHaversine(
        cbind(start_long, start_lat), 
        cbind(location.long, location.lat)
      ) / 1000
    ) %>%
    dplyr::summarize(
      max_distance_from_start = max(distance_from_start, na.rm = TRUE),
      .groups = "drop"
    )
  
  # Calculate max distance for the last days
  max_distance_summary <- max_distance_summary %>%
    dplyr::arrange(tag_id, date) %>%
    dplyr::group_by(tag_id) %>%
    dplyr::mutate(
      max_last_days = map_dbl(date, function(current_date) {
        relevant_values <- max_distance_from_start[
          date < current_date & date >= current_date - days_window
        ]
        if (length(relevant_values) > 0) {
          max(relevant_values, na.rm = TRUE)
        } else {
          NA_real_
        }
      })
    ) %>%
    ungroup()
  
  # Join the calculated max_last_days to daily_summary
  daily_summary <- daily_summary %>%
    left_join(
      max_distance_summary %>% select(tag_id, date, max_last_days),
      by = c("tag_id", "date")
    ) %>% 
    drop_na()
  
  # Prepare the data for plotting
  plot_data <- daily_summary %>%
    dplyr::filter(!is.na(daily_summary$max_last_days)) %>%
    dplyr::select(tag_id, group_id, date, max_last_days)
  
  # Create the interactive plot
  interactive_plot <- plot_ly(
    data = plot_data,
    x = ~date,
    y = ~max_last_days,
    color = ~tag_id,
    colors = unname(color_mapping[plot_data$group_id]),  # Use the custom color palette
    type = 'scatter',
    mode = 'lines+markers',
    line = list(width = 2)
  ) %>%
    layout(
      xaxis = list(title = "Date"),
      yaxis = list(title = "Last Days Max Distance (km)"),
      legend = list(title = list(text = "Tag"),
                    itemclick = "toggleothers")
    ) %>%
    add_trace(
      text = ~paste("Tag ID:", tag_id, "<br>Group ID:", group_id),
      hoverinfo = "text"
    )
  
  # Save the plot
  output_file <- paste0('plots/htmls/','/all_ind_distance_plot.html')
  saveWidget(interactive_plot, file = output_file, selfcontained = TRUE)
  
  return(daily_summary)
}
