process_decoder_files_acc <- function(bin_folder, decoder_path, baboon_data) {
  
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
  for (bin_file in bin_files) {
    cat("\n--- Processing:", basename(bin_file), "---\n")
    
    # Copy bin file as logger.bin
    logger_copy <- file.path(temp_dir, "logger.bin")
    file.copy(bin_file, logger_copy, overwrite = TRUE)
    
    # Copy decoder to temp directory
    decoder_copy <- file.path(temp_dir, basename(decoder_path))
    file.copy(decoder_path, decoder_copy, overwrite = TRUE)
    
    # Run decoder in ACC mode
    cat("Running decoder in ACC mode...\n")
    original_wd <- getwd()
    setwd(temp_dir)
    
    tryCatch({
      result <- system2(basename(decoder_copy),
                        input = "2\n",
                        stdout = TRUE,
                        stderr = TRUE)
      cat("Decoder output:", result[1:min(5, length(result))], "\n")
    }, error = function(e) {
      cat("Error running decoder:", e$message, "\n")
    }, finally = {
      setwd(original_wd)
    })
    
    # Clean up logger.bin and decoder copy
    file.remove(logger_copy)
    if (file.exists(decoder_copy)) file.remove(decoder_copy)
  }
  
  cat("\n=== ACC Decoding Complete ===\n")
  
  # Scan info_tag*.txt files only
  info_files <- list.files(temp_dir, pattern = "^info_tag.*\\.txt$", full.names = TRUE)
  intervals <- data.frame(tag_id = character(), acc_interval = numeric(), stringsAsFactors = FALSE)
  
  if (length(info_files) > 0) {
    for (file in info_files) {
      cat("Scanning:", basename(file), "\n")
      
      lines <- readLines(file, warn = FALSE)
      acc_lines <- grep("ACC HRES INTERVAL:", lines, value = TRUE)
      
      if (length(acc_lines) > 0) {
        last_line <- tail(acc_lines, 1)
        
        # Extract interval in seconds
        interval <- NA
        m <- regexpr("[0-9]+\\s+seconds", last_line)
        if (m > 0) {
          interval <- as.numeric(gsub(" seconds.*", "", regmatches(last_line, m)))
        }
        
        # Extract tag_id from filename
        tag_id <- sub("info_tag([0-9]+)\\.txt", "\\1", basename(file))
        
        intervals <- rbind(intervals, data.frame(tag_id = tag_id,
                                                 acc_interval = interval,
                                                 stringsAsFactors = FALSE))
      }
    }
    cat("\n=== Info Tag Scan Complete ===\n")
    print(intervals)
  } else {
    cat("No info_tag*.txt files found\n")
  }
  
  # Return only intervals metadata
  return(intervals)
}

intervals <- process_decoder_files_acc(bin_folder, decoder_exe, baboon_data)

# Keep only tag_local_identifier and group_id
meta_subset <- metadata[, c("tag_local_identifier", "group_id")]

# Merge intervals with metadata
intervals_group <- merge(intervals,
                   meta_subset,
                   by.x = "tag_id",
                   by.y = "tag_local_identifier",
                   all.x = TRUE)

print(intervals_group)
