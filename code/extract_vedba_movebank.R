# Load required libraries
library(move2)
library(dplyr)
library(lubridate)
library( stringr )
library( data.table )

# Step 1: Define parameters for data retrieval
date_start <- as.POSIXct("2024-12-01 00:00:00", tz = "UTC")  # Adjust start date as needed
date_end <- as.POSIXct("2024-12-10 03:00:00", tz = "UTC")    # Adjust end date as needed
study_id <- 3445611111  # Replace with your specific study ID

# Step 2: Download acceleration data from Movebank
acc_data <- movebank_download_study(study_id,
                sensor_type_id = "acceleration",
                timestamp_start = date_start,                   
                timestamp_end = date_end)

metadata <- mt_track_data(acc_data)

## read in the complete data. This is the data downloaded from Movebank with "All sensors" selected (both GPS and ACC)
acc_data <- as.data.frame(acc_data) #to read in data, download code safes it, then read that file in 

## this makes the timestamp into local time by adding three hours
acc_data$timestamp <- as.POSIXct(x= acc_data$timestamp, format=c("%Y-%m-%d %H:%M:%S"), tz='UTC' ) ## turns the timestamp into a POSIX element
acc_data$local_timestamp <- lubridate::with_tz(acc_data$timestamp, tzone = "Africa/Nairobi")

##subset data but check for weird browser vs r download differences in column names 
acc_data_trim <- try(acc_data[,
                    c( 'individual_local_identifier',
                       'local_timestamp' , 
                       'eobs_accelerations_raw', 
                       'eobs_acceleration_sampling_frequency_per_axis') ,]) ## keep only the necessary columns, wrapped in try function, to exclude underscores etc.


acc_data_trim <- acc_data_trim %>%
  left_join(metadata %>% 
              dplyr::select(individual_local_identifier, tag_local_identifier, group_id, sex), by = c("individual_local_identifier" = "individual_local_identifier"))


d2 <- as.data.frame(str_split(acc_data_trim$eobs_accelerations_raw, " ", simplify = T))

for(i in 1:ncol(d2)){
  d2[,i] <- as.numeric(as.character((d2[,i])))
}

names(d2) <- paste(rep(c("x","y","z"),ncol(d2)/3),rep(1:(ncol(d2)/3), each = 3), sep = '')

d2$timestamp <- acc_data_trim$timestamp
d2$tag <- acc_data_trim$tag

inds <- complete.cases(d2)
acc_data_trim <- acc_data_trim[ inds ,]
d2 <- d2[ inds ,]
 
names(d2) <- c( paste( rep(c("x","y","z"), ncol(d2)/3), rep(1:(ncol(d2)/3), each = 3), sep = ''), 'timestamp')

x_d <- d2[,grepl('x',names(d2))]
head(x_d)

y_d <- d2[,grepl('y',names(d2))]
head(y_d)

z_d <- d2[,grepl('z',names(d2))]
head(z_d)

tag_names <- unique(acc_data_trim$tag_local_identifier)
acc_calib <- read.csv('DATA/acc_calib.csv')
acc_calib$Tag <- as.factor(acc_calib$Tag)
acc_calib$x0 <- as.numeric(acc_calib$x0)
acc_calib$y0 <- as.numeric(acc_calib$y0)
acc_calib$z0 <- as.numeric(acc_calib$z0)
acc_calib$Sx <- as.numeric(acc_calib$Sx)
acc_calib$Sy <- as.numeric(acc_calib$Sy)
acc_calib$Sz <- as.numeric(acc_calib$Sz)

num_rows <- nrow(acc_data_trim)
num_cols <- ncol(x_d)  # Assuming x_d, y_d, z_d have the same number of columns

# Initialize matrices with NA
x_cal <- matrix(NA, nrow = num_rows, ncol = num_cols)
y_cal <- matrix(NA, nrow = num_rows, ncol = num_cols)
z_cal <- matrix(NA, nrow = num_rows, ncol = num_cols)

# Match indices between acc_calib and acc_data_trim
ind_id <- match(acc_data_trim$tag_local_identifier, acc_calib$Tag)

# Calculate XYZ-acceleration AccData m/sec2 using vectorized operations
acc_data_trim$x_cal <- (x_d - acc_calib$x0[ind_id]) * acc_calib$Sx[ind_id] * 9.81
acc_data_trim$y_cal <- (y_d - acc_calib$y0[ind_id]) * acc_calib$Sy[ind_id] * -9.81
acc_data_trim$z_cal <- (z_d - acc_calib$z0[ind_id]) * acc_calib$Sz[ind_id] * 9.81

acc_data_trim$ave_vedba <- apply( sqrt( apply( acc_data_trim$x_cal, 1, FUN = function(x) abs( dy_acc( x ) ) )**2 + 
                                          apply( acc_data_trim$y_cal, 1, FUN = function(x) abs( dy_acc( x ) ) )**2 + 
                                          apply( acc_data_trim$z_cal, 1, FUN = function(x) abs( dy_acc( x ) ) )**2) , 2, FUN = mean )
acc_data_trim$log_vedba <- log( acc_data_trim$ave_vedba )


saveRDS(acc_data_trim,"d1.RDS")

#####################
# Install and load necessary packages

# Load necessary libraries
library(foreach)
library(doParallel)

# Define the dy_acc function
dy_acc <- function(vect, win_size = 9){
  pad_size <- win_size/2 - 0.5
  padded <- unlist(c(rep(NA, pad_size), vect, rep(NA, pad_size)))
  acc_vec <- rep(NA, length = length(vect))
  
  for(i in 1:length(vect)){
    win <- padded[i:(i+(2*pad_size))]
    m_ave <- mean(win, na.rm = TRUE)
    acc_comp <- vect[i] - m_ave
    acc_vec[i] <- acc_comp 
  }
  
  return(unlist(acc_vec))
}

# Set up parallel backend
cl <- makeCluster(detectCores() - 1)
registerDoParallel(cl)

# Calculate the vectorial sum and mean for each row in parallel
results <- foreach(i = 1:nrow(acc_data_trim$x_cal), .combine = c) %dopar% {
  x_component <- abs(dy_acc(acc_data_trim$x_cal[i, ]))
  y_component <- abs(dy_acc(acc_data_trim$y_cal[i, ]))
  z_component <- abs(dy_acc(acc_data_trim$z_cal[i, ]))
  
  vectorial_sum <- sqrt(x_component^2 + y_component^2 + z_component^2)
  mean(vectorial_sum, na.rm = TRUE)
  
  
}

# Stop the cluster
stopCluster(cl)

# Print results
print(results)

# Calculate the vectorial sum, mean, and median pitch for each row in parallel
results <- foreach(i = 1:nrow(acc_data_trim$x_cal), .combine = rbind) %dopar% {
  x_component <- abs(dy_acc(acc_data_trim$x_cal[i, ]))
  y_component <- abs(dy_acc(acc_data_trim$y_cal[i, ]))
  z_component <- abs(dy_acc(acc_data_trim$z_cal[i, ]))
  
  vectorial_sum <- sqrt(x_component^2 + y_component^2 + z_component^2)
  mean_vectorial_sum <- mean(vectorial_sum, na.rm = TRUE)
  
  # Calculate pitch for each element
  pitch <- atan2(y_component, sqrt(x_component^2 + z_component^2)) * (180 / pi)
  
  # Calculate median pitch
  median_pitch <- median(pitch, na.rm = TRUE)
  
  # Return both mean vectorial sum and median pitch
  c(mean_vectorial_sum, median_pitch)
}

# Optionally, name the columns for clarity
colnames(results) <- c("Mean_Vectorial_Sum", "Median_Pitch")
