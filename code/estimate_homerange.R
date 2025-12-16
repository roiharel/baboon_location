library(ctmm)
library(furrr)
library(sf)
library(purrr)
library(tidyverse)
library(magrittr)
library(leaflet)
library(lubridate)
library(webshot2)


cleaned_data <- readRDS("gps_v1_1hour.RDS")
cleaned_data <- cleaned_data %>% filter(!is.na(group_id))
#cleaned_data$individual_local_identifier <- cleaned_data$group_id 

# Compute tracking metrics per individual
tracking_metrics <- cleaned_data %>%
  group_by(individual_local_identifier, group_id) %>%
  arrange(timestamp) %>%
  summarise(
    start = min(timestamp),
    end = max(timestamp),
    duration_days = as.numeric(difftime(end, start, units = "days")),
    median_gap = median(diff(timestamp)),
    .groups = "drop"
  )

# Select best individual per group: longest duration, smallest median gap
best_individuals <- tracking_metrics %>%
  group_by(group_id) %>%
  slice_max(order_by = duration_days, n = 1, with_ties = FALSE) %>%
  arrange(group_id)

# Filter cleaned_data to keep only best individuals
filtered_data <- cleaned_data %>%
  filter(individual_local_identifier %in% best_individuals$individual_local_identifier)

# Convert to telemetry object
DATA <- as.telemetry(filtered_data)

AKDEs <- FITs <- SVFs <- list()
for(i in 1:length(DATA)){
  print(i)
  SVFs[[i]] <- variogram(DATA[[i]])
  GUESS <- ctmm.guess(DATA[[i]],
                      interactive=FALSE)
  FITs[[i]] <- ctmm.select(DATA[[i]],
                           GUESS,
                           trace=0,
                           cores=0)
  AKDEs[[i]] <- akde(DATA[[i]],
                     FITs[[i]],
                     grid=list(dr=10, 
                               align.to.origin=FALSE))
}
# make names of variograms, fits, and UDs the same as data
names(AKDEs) <- names(FITs) <- names(SVFs) <- names(DATA)

sf_points <- map2(DATA, names(DATA), ~{
  df <- as.sf(.x)
  df$individual_id <- .y
  df
}) %>% bind_rows()


akde_sf <- map2(AKDEs, names(AKDEs), ~{
  poly <- as.sf(.x)
  poly$individual_id <- .y
  poly
}) %>% bind_rows()

# # Extract 50% and 95% polygons for each individual
# akde_sf <- map2(AKDEs, names(AKDEs), ~{
#   levels <- c(0.5, 0.95)
#   map_df(levels, function(lvl) {
#     poly_list <- as.sf(.x, level = lvl)
#     poly_est <- poly_list[["est"]]  # extract only the "est" polygon
#     poly_est$individual_id <- .y
#     poly_est$level <- paste0(lvl * 100, "%")
#     poly_est
#   })
# })

group_col <- readRDS("data/group_colors.RDS")
# Define color palette
group_names <- c(
  "Maroon", "Chartreuse", "Bronze", "Emerald", "Lilac", "Copper", "Magenta",
  "LapisSplinter", "Lapis", "Periwinkle", "PhantomWest", "Teal", "SneakySilver", "Purple",
  "RubyRunners", "Green", "Jade"
)

group_colors <- setNames(group_col, group_names)
pal <- colorFactor(palette = group_colors, domain = group_names)
group_ids <- unique(akde_sf$group_id)

akde_sf <- st_transform(akde_sf, crs = 4326)
sf_points <- st_transform(sf_points, crs = 4326)

id_group_map <- cleaned_data %>%
  select(individual_local_identifier, group_id) %>%
  distinct()

id_group_map <- id_group_map %>%
  rename(individual_id = individual_local_identifier)

sf_points <- sf_points %>%
  left_join(id_group_map, by = "individual_id")

akde_sf <- akde_sf %>%
  left_join(id_group_map, by = "individual_id")


group_ids_unique <- sort(unique(akde_sf$group_id))

# Create color palette using actual group_ids from data
pal <- colorFactor(palette = group_col, domain = group_ids_unique)

# Get unique group IDs
group_ids <- unique(akde_sf$group_id)

# Create the leaflet map
m <- leaflet() %>%
  addTiles(group = "OSM") %>%
  addProviderTiles(providers$Esri.WorldTopoMap, group = "Topo") %>%
  addProviderTiles(providers$Esri.WorldImagery, group = "Terrain", options = providerTileOptions(noWrap = TRUE)) %>%
  
  # Add polygons for each group_id as separate layers
  purrr::reduce(group_ids, function(map, gid) {
    map %>% addPolygons(
      data = dplyr::filter(akde_sf, group_id == gid),
      color = "transparent",
      fillColor = ~pal(group_id),
      weight = 1,
      fillOpacity = 0.5,
      popup = ~group_id,
      group = gid
    )
  }, .init = .) %>%
  
  # Add layer controls for each group_id
  addLayersControl(
    baseGroups = c("OSM", "Topo", "Terrain"),
    overlayGroups = group_ids_unique,
    options = layersControlOptions(collapsed = FALSE)
  ) %>%
  
  # Add legend
  addLegend(
    position = "bottomright",
    colors = group_col,
    labels = group_names,
    opacity = .5
  )

htmlwidgets::saveWidget(m, "homeranges.html", selfcontained = TRUE)
webshot("akde_map.html", file = "homeranges.pdf", vwidth = 1200, vheight = 800)
##
# 
# PKDE <- pkde(DATA,AKDEs,kernel="individual")
# 
# pkde_sf <- map2(PKDE, names(PKDE), ~{
#   poly <- as.sf(.x)
#   poly$individual_id <- .y
#   poly
# }) %>% 
#   bind_rows()
# 
# library(ggplot2)
# library(sf)
# library(ggspatial)
# library(cowplot)
# 
# # Filter out the Jade group
# akde_filtered <- akde_sf %>% filter(group_id != "Green")
# 
# # Static map
# p <- ggplot() +
#   annotation_map_tile(type = "osm", zoomin = -1) +  # background map
#   geom_sf(data = akde_filtered, aes(fill = group_id), color = "black", alpha = 0.4, size = 0.2) +
#   scale_fill_manual(values = group_col, name = "Group ID", labels = group_names) +
#   annotation_scale(location = "bl", width_hint = 0.3) +
#   annotation_north_arrow(location = "tl", which_north = "true",
#                          style = north_arrow_fancy_orienteering) +
#   theme_minimal(base_size = 14) +
#   theme(
#     legend.position = "right",
#     panel.grid.major = element_line(color = "gray90"),
#     panel.background = element_rect(fill = "white")
#   )
# 
# # Save as JPG
# ggsave("akde_map.jpg", plot = p, width = 10, height = 8, dpi = 300)



