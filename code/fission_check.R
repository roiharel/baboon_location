library(dplyr)
library(lubridate)
library(geosphere)
library(ggplot2)

cleaned_data <- read.csv("gps_v1.csv")
cleaned_data <- na.omit(clean_data)

cleaned_data$timestamp <- as.POSIXct(cleaned_data$timestamp, format="%Y-%m-%d %H:%M:%S")

data <- cleaned_data %>%
  group_by(individual_local_identifier) %>%
  mutate(time_interval = floor_date(timestamp, "10 minutes")) %>%
  group_by(individual_local_identifier, group_id, time_interval) %>%
  summarize(
    median_longitude = median(location.long, na.rm = TRUE),
    median_latitude = median(location.lat, na.rm = TRUE),
    .groups = 'drop'  # Ensures the result is not grouped
  )

# Calculate centroids for each group within each interval
centroids <- data %>%
  group_by(time_interval, group_id) %>%
  summarize(centroid_lat = mean(median_lat, na.rm = TRUE),
            centroid_lon = mean(median_lon, na.rm = TRUE),
            .groups = 'drop')

# Calculate distance from centroid and determine outliers for each threshold
data <- data %>%
  left_join(centroids, by = c("interval", "group_id")) %>%
  rowwise() %>%
  mutate(distance_from_centroid = distHaversine(c(median_lon, median_lat), c(centroid_lon, centroid_lat)),
         is_outlier_400 = distance_from_centroid > 400,
         is_outlier_1000 = distance_from_centroid > 1000)

# Filter data to include only outliers and assign color intensity
outlier_data <- data %>%
  filter(as.POSIXct(interval) > as.POSIXct("2025-01-01")) %>%
  mutate(outlier_level = case_when(
    is_outlier_1000 ~ "1000m",
    is_outlier_400 ~ "400m",
    TRUE ~ NA_character_
  )) %>%
  filter(!is.na(outlier_level))

# Plot the data with y-axis sorted by group_id
p <- ggplot(outlier_data, aes(x = interval, y = reorder(individual_local_identifier, group_id), color = as.factor(group_id))) +
  geom_point(aes(alpha = outlier_level), size = .1) +
  scale_alpha_manual(values = c("400m" = 0.3, "1000m" = 1.0)) +
  labs(x = "Time") +
  theme_minimal() +
  theme(axis.text.y = element_blank(),  # Remove y-axis text
        axis.title.y = element_blank(), # Remove y-axis title
        legend.position = "none")       # Remove legend

# Convert the ggplot to an interactive plotly plot
interactive_plot <- ggplotly(p)

# Display the interactive plot
interactive_plot


##### ANOTHER TRAIL #####
# Load necessary libraries
library(dplyr)
library(lubridate)
library(geosphere)
library(ggplot2)

# Add interval column and calculate median lat/lon
data <- data.frame(cleaned_data) %>%
  mutate(interval = floor_date(as.POSIXct(timestamp), "10 minutes")) %>%
  group_by(individual_local_identifier, interval, group_id) %>%
  summarize(median_lat = median(location.lat, na.rm = TRUE),
            median_lon = median(location.long, na.rm = TRUE),
            .groups = 'drop')

# Calculate centroids for each group within each interval
centroids <- data %>%
  group_by(interval, group_id) %>%
  summarize(centroid_lat = median(median_lat, na.rm = TRUE),
            centroid_lon = median(median_lon, na.rm = TRUE),
            .groups = 'drop')

# Calculate distance from centroid and determine outliers
data <- data %>%
  left_join(centroids, by = c("interval", "group_id")) %>%
  rowwise() %>%
  mutate(distance_from_centroid = distHaversine(c(median_lon, median_lat), c(centroid_lon, centroid_lat)),
         is_outlier = distance_from_centroid > 1000)

# Filter data to include only outliers
outlier_data <- data %>%
  filter(is_outlier == TRUE) %>%
  filter(as.POSIXct(interval) > as.POSIXct("2025-01-01"))

# Plot the data with y-axis sorted by group_id
ggplot(outlier_data, aes(x = interval, y = reorder(individual_local_identifier, group_id), color = as.factor(group_id))) +
  geom_point() +
  labs(x = "Time",
       color = "Group ID") +
  theme_minimal() +
  theme(axis.text.y = element_blank(),  # Remove y-axis text
        axis.title.y = element_blank()) # Remove y-axis title


# Load the plotly library
library(plotly)

# Create the ggplot
p <- ggplot(outlier_data, aes(x = interval, y = reorder(individual_local_identifier, group_id), color = as.factor(group_id))) +
  geom_point() +
  labs(x = "Time",
       color = "Group ID") +
  theme_minimal() +
  theme(axis.text.y = element_blank(),  # Remove y-axis text
        axis.title.y = element_blank()) # Remove y-axis title

# Convert the ggplot to an interactive plotly plot
interactive_plot <- ggplotly(p)

# Display the interactive plot
interactive_plot


