# Load required libraries
library(move2)
library(dplyr)
library(lubridate)
library( stringr )
library( data.table )

dy_acc <- function(vect, win_size = 7){
  
  pad_size <- win_size/2 - 0.5
  
  padded <- unlist( c(rep(NA, pad_size), vect, rep(NA, pad_size)) )
  acc_vec <- rep(NA, length = length( vect ) )
  
  ## sliding window
  for(i in 1:length(vect)){
    win <- padded[i:(i+(2*pad_size))] ## subset the window
    m_ave <- mean( win, na.rm = T ) ## take the average over the window
    acc_comp <- vect[ i ] - m_ave ## finds the difference between the static component (mean) and the actual value. This is the dynamic component of the acceleration at this time point
    acc_vec[i] <- acc_comp 
  }
  
  return( unlist( acc_vec) )
}

# Step 1: Define parameters for data retrieval
date_start <- as.POSIXct("2024-11-01 00:00:00", tz = "UTC")  # Adjust start date as needed
date_end <- as.POSIXct("2024-11-02 23:59:59", tz = "UTC")    # Adjust end date as needed
study_id <- 3445611111  # Replace with your specific study ID

# Step 2: Download acceleration data from Movebank
acc_data <- movebank_download_study(3445611111,
                sensor_type_id = "acceleration",
                timestamp_start = date_start,                   
                timestamp_end = date_end)


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


d2 <- as.data.frame(str_split(acc_data_trim$eobs_accelerations_raw, " ", simplify = T))

for(i in 1:ncol(d2)){
  d2[,i] <- as.numeric(as.character((d2[,i])))
}

names(d2) <- paste(rep(c("x","y","z"),ncol(d2)/3),rep(1:(ncol(d2)/3), each = 3), sep = '')

d2$timestamp <- acc_data_trim$timestamp
d2$tag <- acc_data_trim$tag

d2[!complete.cases(d2),]

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

acc_data_trim$ave_vedba <- apply( sqrt( apply( x_d, 1, FUN = function(x) abs( dy_acc( x ) ) )**2 + apply( y_d, 1, FUN = function(x) abs( dy_acc( x ) ) )**2 + apply( z_d, 1, FUN = function(x) abs( dy_acc( x ) ) )**2) , 2, FUN = mean )
acc_data_trim$log_vedba <- log( acc_data_trim$ave_vedba )
