prep_location_mat <- function(
    input_rds_path,
    output_dir = "output",
    utm_zone = 37,
    hemisphere = "north",
    start_hour = "03:00:00",
    end_hour = "16:00:00",
    time_interval = "2 min"
) {
  # Load required libraries
  library(data.table)
  library(lubridate)
  library(sf)
  library(arrow)
  library(dplyr)
  
  message("Reading data...")
  cleaned_data <- setDT(readRDS(input_rds_path))
  
  # Floor timestamps to nearest minute
  cleaned_data[, timestamp := floor_date(as.POSIXct(timestamp, tz = "UTC"), unit = "minute")]
  
  # Get min/max date range
  min_date <- as.Date(min(cleaned_data$timestamp, na.rm = TRUE))
  max_date <- as.Date(max(cleaned_data$timestamp, na.rm = TRUE))
  
  message("Building time grid...")
  time_grid <- seq(
    from = as.POSIXct(paste(min_date, start_hour), tz = "UTC"),
    to   = as.POSIXct(paste(max_date, end_hour), tz = "UTC"),
    by   = time_interval
  )
  
  # Restrict to time window
  time_grid <- time_grid[
    format(time_grid, "%H:%M:%S") >= start_hour &
      format(time_grid, "%H:%M:%S") <= end_hour
  ]
  
  ids <- unique(cleaned_data$individual_local_identifier)
  
  message("Expanding grid...")
  full_grid <- CJ(
    timestamp = time_grid,
    individual_local_identifier = ids,
    sorted = FALSE
  )
  
  message("Joining with cleaned data...")
  aligned <- merge(
    full_grid,
    cleaned_data[, .(individual_local_identifier, timestamp, location.lat, location.long)],
    by = c("individual_local_identifier", "timestamp"),
    all.x = TRUE
  )
  
  message("Projecting to UTM...")
  utm_crs <- st_crs(paste0(
    "+proj=utm +zone=", utm_zone,
    ifelse(hemisphere == "south", " +south", ""),
    " +datum=WGS84 +units=m +no_defs"
  ))
  
  sf_use_s2(FALSE)
  
  sf_points <- aligned[!is.na(location.long) & !is.na(location.lat)] %>%
    st_as_sf(coords = c("location.long", "location.lat"), crs = 4326, remove = FALSE)
  
  coords <- st_coordinates(sf_points)
  coords_utm <- sf_project(from = st_crs(4326), to = utm_crs, pts = coords)
  
  processed_data <- st_drop_geometry(sf_points)
  setDT(processed_data)
  processed_data[, `:=`(utm_x = coords_utm[, 1], utm_y = coords_utm[, 2])]
  
  aligned_final <- merge(
    aligned,
    processed_data[, .(individual_local_identifier, timestamp, utm_x, utm_y)],
    by = c("individual_local_identifier", "timestamp"),
    all.x = TRUE
  )
  
  sf_use_s2(TRUE)
  
  message("Creating wide matrices...")
  lat_matrix <- dcast(aligned_final, timestamp ~ individual_local_identifier,
                      value.var = "location.lat", fun.aggregate = function(x) x[1], fill = NA_real_)
  lon_matrix <- dcast(aligned_final, timestamp ~ individual_local_identifier,
                      value.var = "location.long", fun.aggregate = function(x) x[1], fill = NA_real_)
  x_matrix <- dcast(aligned_final, timestamp ~ individual_local_identifier,
                    value.var = "utm_x", fun.aggregate = function(x) x[1], fill = NA_real_)
  y_matrix <- dcast(aligned_final, timestamp ~ individual_local_identifier,
                    value.var = "utm_y", fun.aggregate = function(x) x[1], fill = NA_real_)
  
  # Create output directory if it doesn't exist
  dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)
  
  message("Saving data to disk...")
  # Parquet outputs
  write_parquet(lat_matrix, file.path(output_dir, "lats.parquet"))
  write_parquet(lon_matrix, file.path(output_dir, "lons.parquet"))
  write_parquet(x_matrix,   file.path(output_dir, "xs.parquet"))
  write_parquet(y_matrix,   file.path(output_dir, "ys.parquet"))
  
  # CSV outputs
  fwrite(lat_matrix, file.path(output_dir, "lats.csv"))
  fwrite(lon_matrix, file.path(output_dir, "lons.csv"))
  fwrite(x_matrix,   file.path(output_dir, "xs.csv"))
  fwrite(y_matrix,   file.path(output_dir, "ys.csv"))
  
  # Save IDs and time grids
  fwrite(data.table(id = as.character(ids)), file.path(output_dir, "ids.csv"))
  fwrite(data.table(id = as.character(time_grid)), file.path(output_dir, "times.csv"))
  
  message("??? Processing complete. Outputs saved in: ", normalizePath(output_dir))
  
  # Return results invisibly
  invisible(list(
    lat_matrix = lat_matrix,
    lon_matrix = lon_matrix,
    x_matrix = x_matrix,
    y_matrix = y_matrix
  ))
}
