
# load data and basic cleaning
download_data <- function(date_start, date_end) {
  baboon_data <- movebank_download_study(study_id = 3445611111, sensor_type_id = c("gps"), 
                                         timestamp_start = date_start,
                                         timestamp_end = date_end,
                                         # individual_id = c(3487912671, 3487912662, 3487912663, 3487844492, 3508338112, 3938663627),
                                         remove_movebank_outliers = TRUE) %>%
    filter(!st_is_empty(.))     # remove empty rows
  return(baboon_data)
}

arrange_data <- function(baboon_data, time_interval, speed_threshold) {
  
  # calc speed azimuth and clean speed outliers
  baboon_data %<>% dplyr::mutate(azimuth = mt_azimuth(.), speed = mt_speed(.))
  baboon_data$speed <- set_units(baboon_data$speed, "m/s")
  baboon_data <- baboon_data %>%
    filter(speed <= speed_threshold | is.na(speed))
  
  # add fields from metadata
  metadata <- mt_track_data(baboon_data)
  
  baboon_data <- baboon_data %>%
    left_join(metadata %>% 
                dplyr::select(deployment_id, group_id, sex), by = "deployment_id")  %>%
    mt_filter_per_interval(unit = time_interval)
  
  baboon_data$location.long <- sf::st_coordinates(baboon_data)[,1]
  baboon_data$location.lat <- sf::st_coordinates(baboon_data)[,2]
  baboon_data$group_id <- baboon_data$group_id
  
  
  # Identify matching columns
  matching_columns <- Reduce(intersect, list(names(baboon_data)))
  
  # Join tibbles while keeping only matching columns
  cleaned_data <- bind_rows(
    dplyr::select(as.data.frame(baboon_data), matching_columns))
  
  # Return the clustered data
  return(cleaned_data)

}


## load data and basic cleaning
baboon_data <- download_data(date_start, date_end)
cleaned_data_high <- arrange_data(baboon_data, time_interval_high, speed_threshold)
cleaned_data_low <- arrange_data(baboon_data, time_interval_low, speed_threshold)

group_ids <- unique(cleaned_data_high$group_id)
# Create a named vector for mapping to colors
color_mapping <- setNames(group_col[1:length(group_ids)], group_ids)

## save basic data
fwrite(cleaned_data_high, "data/gps_v1.csv", row.names = FALSE)
saveRDS(cleaned_data_high, "data/gps_v1.RDS")
saveRDS(cleaned_data_low, "data/gps_v1_1hour.RDS")

#write_parquet(cleaned_data_high, "gps_v1.parquet")