# Function to process decoder files and merge with movebank data
process_decoder_files <- function(bin_folder, decoder_path, baboon_data) {
  
  # Create a temporary working directory
  temp_dir <- file.path(bin_folder, "temp_decoder")
  dir.create(temp_dir, showWarnings = FALSE)
  
  # Get all .bin files
  bin_files <- list.files(bin_folder, pattern = "\\.bin$", full.names = TRUE)
  
  if (length(bin_files) == 0) {
    stop("No .bin files found in the specified folder")
  }
  
  cat("Found", length(bin_files), "bin files to process\n")
  
  # Process each bin file
  all_gps_data <- NULL
  
  for (bin_file in bin_files) {
    cat("\n--- Processing:", basename(bin_file), "---\n")
    
    # Create a copy and rename to logger.bin
    logger_copy <- file.path(temp_dir, "logger.bin")
    file.copy(bin_file, logger_copy, overwrite = TRUE)
    
    # Copy decoder to temp directory
    decoder_copy <- file.path(temp_dir, basename(decoder_path))
    file.copy(decoder_path, decoder_copy, overwrite = TRUE)
    
    # Run the decoder - change working directory to temp_dir
    cat("Running decoder...\n")
    
    # Save current working directory
    original_wd <- getwd()
    setwd(temp_dir)
    
    # Use system2 to run the decoder with input
    result <- tryCatch({
      # The decoder expects input "m" followed by enter
      system2(basename(decoder_copy), 
              input = "m\n",
              stdout = TRUE,
              stderr = TRUE)
    }, error = function(e) {
      cat("Error running decoder:", e$message, "\n")
      setwd(original_wd)
      return(NULL)
    }, finally = {
      setwd(original_wd)
    })
    
    if (!is.null(result)) {
      cat("Decoder output:", result[1:min(5, length(result))], "\n")
    }
    
    # Find and read all GPS files generated
    gps_files <- list.files(temp_dir, pattern = ".*gps.*", ignore.case = TRUE, full.names = TRUE)
    gps_files <- gps_files[!grepl("logger\\.bin", gps_files)]
    
    if (length(gps_files) > 0) {
      cat("Found", length(gps_files), "GPS file(s)\n")
      
      for (gps_file in gps_files) {
        cat("Reading:", basename(gps_file), "\n")
        
        gps_data <- tryCatch({
          read.csv(gps_file, stringsAsFactors = FALSE)
        }, error = function(e) {
          cat("Error reading file:", e$message, "\n")
          return(NULL)
        })
        
        if (!is.null(gps_data)) {
          all_gps_data <- rbind(all_gps_data, gps_data)
        }
      }
      
      # Delete the GPS files after reading
      file.remove(gps_files)
      cat("Deleted GPS files\n")
    } else {
      cat("No GPS files found\n")
    }
    
    # Clean up logger.bin and decoder copy for next iteration
    file.remove(logger_copy)
    if (file.exists(decoder_copy)) {
      file.remove(decoder_copy)
    }
  }
  
  # Clean up temp directory
  unlink(temp_dir, recursive = TRUE)
  
  cat("\n=== Decoding Complete ===\n")
  cat("Total rows from decoder:", nrow(all_gps_data), "\n")
  
  return(all_gps_data)
}

# Function to standardize column names and format decoder data
prepare_decoder_data <- function(decoder_data) {
  
  # Standardize column names to match move2 format (with hyphens)
  col_mapping <- list(
    "tag-serial-number" = "tag-local-identifier",
    "tag_serial_number" = "tag-local-identifier",
    "longitude" = "location-long",
    "latitude" = "location-lat",
    "height-above-ellipsoid" = "height-above-ellipsoid",
    "type-of-fix" = "eobs-type-of-fix",
    "battery-voltage" = "eobs-battery-voltage",
    "fix-battery-voltage" = "eobs-fix-battery-voltage",
    "temperature" = "eobs-temperature",
    "speed-over-ground" = "ground-speed",
    "heading-degree" = "heading",
    "speed-accuracy-estimate" = "eobs-speed-accuracy-estimate",
    "horizontal-accuracy-estimate" = "eobs-horizontal-accuracy-estimate",
    "dop" = "gps-dop",
    "satellite-count" = "gps-satellite-count",
    "used-time-to-get-fix" = "eobs-used-time-to-get-fix",
    "key-bin-checksum" = "eobs-key-bin-checksum",
    "start-timestamp" = "eobs-start-timestamp",
    "timestamp-of-fix" = "timestamp"
  )
  
  # Rename columns
  names(decoder_data) <- sapply(names(decoder_data), function(col) {
    if (col %in% names(col_mapping)) {
      return(col_mapping[[col]])
    } else {
      # Convert underscores to hyphens for any unmapped columns
      return(gsub("_", "-", col))
    }
  })
  
  # Convert timestamp columns to datetime (using hyphenated names)
  if ("timestamp" %in% names(decoder_data)) {
    decoder_data$timestamp <- as.POSIXct(decoder_data$timestamp, 
                                         format = "%Y-%m-%d %H:%M:%S")
  }
  
  if ("eobs-start-timestamp" %in% names(decoder_data)) {
    decoder_data$`eobs-start-timestamp` <- as.POSIXct(decoder_data$`eobs-start-timestamp`, 
                                                      format = "%Y-%m-%d %H:%M:%S")
  }
  
  # Convert numeric columns (using hyphenated names)
  numeric_cols <- c("location-long", "location-lat", "height-above-ellipsoid",
                    "ground-speed", "heading", "eobs-battery-voltage", 
                    "eobs-fix-battery-voltage", "eobs-temperature",
                    "eobs-speed-accuracy-estimate", "eobs-horizontal-accuracy-estimate",
                    "gps-dop", "gps-satellite-count", "eobs-used-time-to-get-fix",
                    "eobs-key-bin-checksum")
  
  for (col in numeric_cols) {
    if (col %in% names(decoder_data)) {
      decoder_data[[col]] <- as.numeric(decoder_data[[col]])
    }
  }
  
  return(decoder_data)
}

# =====================================================
# USAGE:
# =====================================================

# Set your paths
bin_folder <- "~/MBRP/data/loggers"
decoder_exe <- "~/MBRP/data/loggers/decoder_v21_win64.exe"

# Process and merge
decoder_data <- process_decoder_files(bin_folder, decoder_exe, baboon_data)
write.csv(decoder_data,'data/decoder_data_dec2025.csv')


#####################################################

# Function to merge decoder data with movebank data
merge_decoder_with_movebank <- function(baboon_data, decoder_data) {
  
  # Prepare decoder data
  decoder_prep <- prepare_decoder_data(decoder_data)
  
  cat("\nMerging data...\n")
  cat("Movebank data rows:", nrow(baboon_data), "\n")
  cat("Decoder data rows:", nrow(decoder_prep), "\n")
  
  # Convert movebank data (move2) to sf if needed
  if (inherits(baboon_data, "move2")) {
    baboon_sf <- sf::st_as_sf(baboon_data)
  } else if (inherits(baboon_data, "sf")) {
    baboon_sf <- baboon_data
  } else {
    baboon_sf <- sf::st_as_sf(baboon_data)
  }
  
  # Create geometry column for decoder data if not present
  if (!"geometry" %in% names(decoder_prep)) {
    # Check if we have location columns
    if ("location-long" %in% names(decoder_prep) && "location-lat" %in% names(decoder_prep)) {
      decoder_prep <- sf::st_as_sf(decoder_prep, 
                                   coords = c("location-long", "location-lat"),
                                   crs = 4326)
    }
  }
  
  # Get columns from movebank data
  movebank_cols <- names(baboon_sf)
  
  # Add missing columns from movebank to decoder data (filled with NA)
  for (col in movebank_cols) {
    if (!col %in% names(decoder_prep)) {
      decoder_prep[[col]] <- NA
    }
  }
  
  # Select and reorder columns to match movebank data (in same order)
  decoder_prep <- decoder_prep[, movebank_cols]
  
  # Bind rows
  combined_data <- rbind(baboon_sf, decoder_prep)
  
  cat("Combined data rows:", nrow(combined_data), "\n")
  cat("Data merged successfully!\n")
  
  return(combined_data)
}


baboon_data_complete <- merge_decoder_with_movebank(baboon_data, decoder_data)
