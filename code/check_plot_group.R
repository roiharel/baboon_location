library(dplyr)
library(ggplot2)
library(plotly)
library(zoo) # for rollmedian


group_night_dat <- full_dat_meta[full_dat_meta$group_id == "LizardRock" , ]
group_night_dat <- na.omit(group_night_dat)


# Assuming `group_night_dat` is already prepared and includes the `sleep_bouts` column
# Map sleep_bouts to activity states
group_night_dat <- group_night_dat %>%
  mutate(activity_state = ifelse(sleep_bouts == 1, "Inactive", "Active"))

# Plot using ggplot2
p <- ggplot(group_night_dat, aes(x = local_timestamp, 
                                 y = individual_local_identifier, 
                                 fill = activity_state)) +
  geom_tile() + # Use tiles to represent states
  labs(x = "Time", y = "Individual", fill = "Activity State") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_manual(values = c("Active" = "white", "Inactive" = "blue")) # Customize colors

p
# Convert to interactive plotly plot

saveWidget(interactive_plot, paste0('plots/htmls/','/baboon_data_batt_plot.html')
           , selfcontained = FALSE)

ggplotly(p, tooltip = c("x", "y", "fill"))


library(dplyr)
library(ggplot2)
library(plotly)
library(zoo) # for rollmedian
library(lubridate) # for date-time manipulation

# Assuming `group_night_dat` is already prepared and includes the `sleep_bouts` column
# Map sleep_bouts to activity states
group_night_dat <- group_night_dat %>%
  mutate(activity_state = ifelse(sleep_bouts == 1, "Inactive", "Active"))

# Join with sleep_per to get onset and waking times
group_night_dat <- group_night_dat %>%
  left_join(sleep_per, by = c("tag" = "tag", "night" = "night"))

# Convert onset and waking times to POSIXct
group_night_dat <- group_night_dat %>%
  mutate(
    onset_time = as.POSIXct(paste(night_date, onset_time), format = "%Y-%m-%d %H:%M:%S"),
    waking_time = as.POSIXct(paste(night_date, waking_time), format = "%Y-%m-%d %H:%M:%S")
  )

# Plot using ggplot2
p <- ggplot(group_night_dat, aes(x = local_timestamp, 
                                 y = individual_local_identifier, 
                                 fill = activity_state)) +
  geom_tile() + # Use tiles to represent states
  geom_point(aes(x = onset_time, y = individual_local_identifier), shape = 8, color = "black", size = 3, na.rm = TRUE) + # Add stars for sleep onset
  geom_point(aes(x = waking_time, y = individual_local_identifier), shape = 8, color = "black", size = 3, na.rm = TRUE) + # Add stars for wake times
  labs(x = "Time", y = "Individual", fill = "Activity State") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_manual(values = c("Active" = "blue", "Inactive" = "red")) # Customize colors
p
# Convert to interactive plotly plot
#ggplotly(p, tooltip = c("x", "y", "fill"))
