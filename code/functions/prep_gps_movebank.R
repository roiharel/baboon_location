
cols_to_keep <- c( #  "deployment_id",
  "geometry", "azimuth", "speed", "animal_id", "tag_id", 
  "group_id", "sex", "age", "location.long", "location.lat", "timestamp", "event_id",
  "ground_speed", "heading", "height_above_ellipsoid",
  "eobs_battery_voltage", "eobs_horizontal_accuracy_estimate", "eobs_key_bin_checksum",
  "eobs_speed_accuracy_estimate", "eobs_start_timestamp", "eobs_status",
  "eobs_temperature", "eobs_type_of_fix", "eobs_used_time_to_get_fix",
  "gps_dop", "gps_hdop", "gps_satellite_count"
)

cols_to_keep_limited <- c(
  "animal_id", "tag_id", "timestamp",
  "location.long", "location.lat",  
  "ground_speed", "heading", "height_above_ellipsoid", "azimuth", "speed",  
  "group_id", "sex", "age")

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
  
  # add fields from metadata
  metadata <- mt_track_data(baboon_data)
  metadata$age <- metadata$individual_comments
  
  baboon_data <- if ("deployment_id" %in% names(baboon_data)) {
    
    baboon_data %>%
      left_join(
        metadata %>%
          dplyr::select(
            deployment_id, tag_local_identifier, individual_local_identifier,
            group_id, sex, age
          ),
        by = "deployment_id"
      )
    
  } else {
    
    baboon_data %>%
      left_join(
        metadata %>%
          dplyr::select(
            individual_local_identifier, tag_local_identifier,
            group_id, sex, age
          ),
        by = "individual_local_identifier"
      )
  }
  
  baboon_data <- baboon_data %>%
    mt_filter_per_interval(unit = time_interval) %>%
    dplyr::rename(
      tag_id = tag_local_identifier,
      animal_id = individual_local_identifier
    )
  
  baboon_data$location.long <- sf::st_coordinates(baboon_data)[,1]
  baboon_data$location.lat <- sf::st_coordinates(baboon_data)[,2]
  baboon_data$group_id <- baboon_data$group_id
  
  baboon_data <- baboon_data[as.numeric(baboon_data$gps_satellite_count) != 0, ]

  # baboon_data <- baboon_data[baboon_data$eobs_status == "A", ] 
  
  # clean locations outside of Study area
  baboon_data <- baboon_data %>%
    filter(is.na(height_above_ellipsoid) | as.numeric(height_above_ellipsoid) < 2000) %>%
    filter(speed <= speed_threshold) %>%
    filter(location.long >= 36.7, location.long <= 37,
           location.lat >= 0.2, location.lat <= 0.6)
  
  # Identify matching columns
  matching_columns <- Reduce(intersect, list(names(baboon_data)))
  
  # Join tibbles while keeping only matching columns
  cleaned_data <- bind_rows(
    dplyr::select(as.data.frame(baboon_data), matching_columns))
  
  # Keep only the specified columns
  cleaned_data <- cleaned_data[, cols_to_keep, drop = FALSE]
  
  # Return the clustered data
  return(cleaned_data)

}


## load data and basic cleaning
baboon_data <- download_data(date_start, date_end)
cleaned_data_min <- arrange_data(baboon_data, time_interval_min, speed_threshold)
cleaned_data_hour <- arrange_data(baboon_data, time_interval_hour, speed_threshold)

## save basic data - full data frame
fwrite(cleaned_data_min, file.path(output_data_folder,"gps_v1_all_fields.csv"), row.names = FALSE)
saveRDS(cleaned_data_min, file.path(output_data_folder,"gps_v1_all_fields.RDS"))

## save basic data - limited fields 
cleaned_data_min_limited <- cleaned_data_min[, cols_to_keep_limited, drop = FALSE]
cleaned_data_hour_limited <- cleaned_data_hour[, cols_to_keep_limited, drop = FALSE]

fwrite(cleaned_data_min_limited, file.path(output_data_folder,"gps_v1.csv"), row.names = FALSE)
fwrite(cleaned_data_hour_limited, file.path(output_data_folder,"gps_v1_1hour.csv"), row.names = FALSE)

saveRDS(cleaned_data_min_limited, file.path(output_data_folder,"gps_v1.RDS"))
saveRDS(cleaned_data_hour_limited, file.path(output_data_folder,"gps_v1_1hour.RDS"))

write_parquet(cleaned_data_min_limited, file.path(output_data_folder,"gps_v1.parquet"))


