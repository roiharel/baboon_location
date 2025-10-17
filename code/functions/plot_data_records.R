plot_data_records <- function(cleaned_data) {
  # Filter recent data
  
  round_to_nearest <- function(x, values) {
    values[which.min(abs(values - x))] }
  
  cleaned_data$tag_local_identifier <- with(cleaned_data, reorder(tag_local_identifier, group_id))
  
  # Order levels
  ordered_levels <- cleaned_data %>%
    dplyr::arrange(group_id) %>%
    pull(tag_local_identifier) %>%
    unique()
  
  cleaned_data$tag_local_identifier <- factor(cleaned_data$tag_local_identifier, levels = ordered_levels)
  
  # Create and save the first plot
  records <- ggplot(cleaned_data, 
                    aes(x = timestamp, 
                        y = eobs_battery_voltage,
                        color = tag_local_identifier)) +
    geom_point() +
    labs(x = "timestamp", y = "tagID")
  interactive_plot <- ggplotly(records, tooltip = "text")
  saveWidget(interactive_plot, paste0('plots/htmls/','/baboon_data_batt_plot.html'), selfcontained = FALSE)
  
  # Calculate daily summary
  daily_summary <- cleaned_data %>%
    dplyr::mutate(date = as.Date(timestamp),            
                  time_diff = as.numeric(difftime(timestamp, lag(timestamp), units = "secs"))) %>%
    dplyr::group_by(tag_local_identifier, individual_local_identifier, group_id, date) %>%
    summarize(median_time_diff = median(time_diff, na.rm = TRUE), 
              gps_fix_count = n(),
              min_battery = min(eobs_battery_voltage, na.rm = TRUE),   
              .groups = "drop") %>%
    dplyr::mutate(rounded_time_diff = map_dbl(median_time_diff, round_to_nearest, values = c(60, 120, 7200))) %>%
    dplyr::mutate(rounded_time_diff = recode(rounded_time_diff, 
                                             "60" = "High",
                                             "120" = "Monitor",
                                             "7200" = "Rest")) 
  
  # Find the most recent `rounded_time_diff` for each tag
  most_recent_rounded_time_diff <- daily_summary %>%
    group_by(tag_local_identifier) %>%
    filter(date == max(date)) %>%
    dplyr::select(tag_local_identifier, 
                  last_rounded_time_diff = rounded_time_diff,  
                  last_batt_value = min_battery)               
  
  # Join this information back into the original `daily_summary`
  daily_summary <- daily_summary %>%
    left_join(most_recent_rounded_time_diff, by = "tag_local_identifier") 
  
  daily_summary <- daily_summary %>%
    dplyr::mutate(y_axis_label = interaction(group_id, tag_local_identifier, last_rounded_time_diff, last_batt_value, sep = " - ")) %>%
    dplyr::mutate(y_axis_label = factor(y_axis_label, 
                                        levels = unique(y_axis_label[order(group_id, tag_local_identifier)]))) 
  
  # Create and save the second plot
  daily_plot <- ggplot(daily_summary, 
                       aes(x = date, 
                           y = y_axis_label,
                           color = as.numeric(min_battery))) +
    geom_point(size = 3) +
    labs(x = "Date", y = "Tag ID") +
    theme(axis.text.y = element_text(angle = 0, hjust = 1)) +  
    scale_y_discrete(drop = FALSE) +  
    scale_color_gradientn(colors = heat.colors(20), 
                          limits = c(3590, 4000)) +
    scale_x_date(date_breaks = "1 week", date_labels = "%Y-%m-%d") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  interactive_plot <- ggplotly(daily_plot, tooltip = "text")
  
  saveWidget(interactive_plot, paste0('plots/htmls/','/baboon_data_records.html'), selfcontained = TRUE)
  
  return(list(cleaned_data = cleaned_data, daily_summary = daily_summary))
}
