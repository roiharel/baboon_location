library(dplyr)

# Read the summary table from CSV
summary_table <- read.csv('plots/summary_decoder_data.csv')

# Convert tag_id to character for matching
summary_table$tag_id <- as.character(summary_table$tag_id)

# Update last_day column in last_rows_per_tag_html based on summary_table
last_rows_per_tag_html <- last_rows_per_tag_html %>%
  mutate(
    tag_id_clean = as.character(gsub("<[^>]+>", "", tag_id)),  # Remove HTML tags from tag_id
    last_day = case_when(
      tag_id_clean %in% summary_table$tag_id ~ 
        as.Date(summary_table$last_date[match(tag_id_clean, summary_table$tag_id)]),
      TRUE ~ last_day
    )
  ) %>%
  select(-tag_id_clean)  # Remove helper column

# Find new tags not in last_rows_per_tag_html
existing_tags <- gsub("<[^>]+>", "", last_rows_per_tag_html$tag_id) %>% as.character()
new_tags <- setdiff(summary_table$tag_id, existing_tags)

# Add new tags to table
if (length(new_tags) > 0) {
  
  new_rows <- summary_table %>%
    filter(tag_id %in% new_tags) %>%
    mutate(
      tag_id = paste0("<span style=''>", tag_id, "</span>"),
      sex = NA,
      age = NA,
      status = NA,
      last_batt_value = NA,
      max_last_days = NA,
      first_day = as.Date(first_date),
      last_day = as.Date(last_date)
    ) %>%
    select(tag_id, animal_id, group_id, first_day, last_day, sex, age, status, last_batt_value, max_last_days)
  
  for (i in seq_len(nrow(new_rows))) {
    clean_tag <- gsub("<[^>]+>", "", new_rows$tag_id[i])
    cat(paste0("   . ", clean_tag, ": ", new_rows$animal_id[i], " (", new_rows$group_id[i], ")\n"))
  }
  
  last_rows_per_tag_html <- bind_rows(last_rows_per_tag_html, new_rows)
}

# Sort by tag_id (removing HTML tags for sorting)
last_rows_per_tag_html <- last_rows_per_tag_html %>%
  mutate(tag_id_numeric = as.numeric(gsub("<[^>]+>", "", tag_id))) %>%
  arrange(tag_id_numeric) %>%
  select(-tag_id_numeric)

cat("\n Metadata table updated successfully\n")
cat("Total records:", nrow(last_rows_per_tag_html), "\n")
cat("New tags added:", length(new_tags), "\n")
