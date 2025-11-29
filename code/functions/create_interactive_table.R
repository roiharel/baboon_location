create_interactive_table <- function(daily_summary) {
  # Create a table of tags, group, last download date, and battery level
  
  mark_status_change <- function(status, battery, tag, max_last_days) {
    style <- ""
    
    # Check if the tag is in the possible mortality list
    if (tag %in% possible_mortality) {
      style <- paste(style, "color: red; text-decoration: line-through; font-weight: bold;")  # Red color and strikethrough for mortality
    } else {
      # Check for max_last_days less than 0.5
      if (max_last_days < 0.5) {
        style <- paste(style, "color: red; font-weight: bold;")  # Red color for max_last_days < 0.5
      } else {
        # Check for 'rest' status and battery
        if (status == "Rest" && battery > set_units(3950, "mV")) {
          style <- paste(style, "color: blue; font-weight: bold;")  # Blue color for non-mortality
        }
        
        # Check for 'monitor' status and battery
        if ((status == "Monitor" || status == "High") && battery < set_units(3700, "mV")) {
          style <- paste(style, "color: blue; font-weight: bold;")  # Blue color for non-mortality
        }
      }
    }
    
    # Return HTML string with the style
    return(paste("<span style='", style, "'>", tag, "</span>", sep = ""))
  }
  
  last_rows_per_tag <- daily_summary %>%
    group_by(tag_id) %>%
    filter(date == max(date)) %>%
    dplyr::select(tag_id, animal_id, group_id, sex, age, date, rounded_time_diff, last_batt_value, max_last_days) %>%
    ungroup() %>%
    rename(status = rounded_time_diff) %>%
    mutate(max_last_days = signif(max_last_days, 2)) 
  
  
  
  first_days <- daily_summary %>%
    group_by(tag_id) %>%
    summarise(first_day = min(date), .groups = "drop")
  # Merge first day into last_rows_per_tag
  last_rows_per_tag <- last_rows_per_tag %>%
    rename(last_day = date) %>%
    left_join(first_days, by = "tag_id")  %>%
    select(tag_id, animal_id, group_id, first_day, last_day, everything()) %>%
    group_by(animal_id) %>%
    slice_max(last_day, with_ties = FALSE) %>%
    ungroup()
    
  # Prepare HTML formatted columns
  last_rows_per_tag_html <- last_rows_per_tag
  last_rows_per_tag_html$tag_id <- mapply(
    mark_status_change, 
    last_rows_per_tag$status, 
    last_rows_per_tag$last_batt_value,
    last_rows_per_tag$tag_id,
    last_rows_per_tag$max_last_days
  )
  #last_rows_per_tag_html <- last_rows_per_tag_html %>%
  #  select(-max_last_days)
  
  # Create the interactive table
  interactive_table <- datatable(
    last_rows_per_tag_html,
    escape = FALSE,
    options = list(
      paging = TRUE,
      searching = TRUE,
      ordering = TRUE,
      pageLength = nrow(last_rows_per_tag_html),
      lengthMenu = c(10, 20, nrow(last_rows_per_tag_html)),
      autoWidth = TRUE
    )
  )
  
  # Save the table as an HTML file
  saveWidget(interactive_table, 'plots/htmls/table_baboon_data_records.html', selfcontained = TRUE)
}
