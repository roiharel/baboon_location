# MBRP

## Overview
MBRP documentation project aims to provide comprehensive details about the various functionalities and scripts used for managing and analyzing tracking data.

Plotting Tracks
basic_monitoring_collars.R are used for processing and visualizing animal tracking data.
The script processes and visualizes animal tracking data collected since March 2024. It connects to Movebank, downloads GPS data, cleans it, and generates various plots and interactive visualizations using libraries like ggplot2, leaflet, and plotly. The script also creates summary tables and maps based on the tracking data.

Interactive Maps
The plot_leaflet_basic.R script generates interactive maps using Leaflet, visualizing animal tracking data with different base layers and overlays. It filters and groups the data, creates color-coded markers for various data subsets, and includes user-interactive features such as clickable coordinates and layer controls. The script saves the resulting maps as HTML files for easy viewing and sharing.

Sleep Site Maps
The sleep_site_mapbox.R script clusters GPS data points to identify sleep sites using DBSCAN, calculates centroids for each cluster, and visualizes the proportions of different groups at each sleep site using pie charts on an interactive Leaflet map. The map includes various base layers and allows for easy visualization of the clustered data.

baboon_movement_plots_and_graphs (John)
This code processes and visualizes baboon movement data. It downloads GPS data from Movebank, cleans and merges it with metadata, and creates several plots: a bar plot showing the number of individuals per group, a line plot of group distribution over time, faceted plots showing distribution per group over time, and a spatial distribution plot of group movements by month.

## Monitoring tracking data

[Field control table](https://roiharel.github.io/MBRP/plots/htmls/table_baboon_data_records.html)

[Map - day&night](https://roiharel.github.io/MBRP/plots/htmls/baboon_interactive_map.html)

[Cum. dist - possible mortality](https://roiharel.github.io/MBRP/plots/htmls/all_ind_distance_plot.html)

[Data records](https://roiharel.github.io/MBRP/plots/htmls/baboon_data_records.html)

[Battery trajectory](https://roiharel.github.io/MBRP/plots/htmls/baboon_data_batt_plot.html)

[Map - night](https://roiharel.github.io/MBRP/plots/htmls/baboon_night_interactive_map.html)

[Map - proprtion sleep sites](https://roiharel.github.io/MBRP/plots/htmls/prop_sleep_site_map.html)

[Map - Sleeping sites transitions](https://roiharel.github.io/MBRP/plots/htmls/transitions_group_sleeping_sites_map.html)


## kml - 1. last week, 2. by group - all time
[Last week](https://roiharel.github.io/MBRP/plots/kmls/last_week.kmz)

[TrickyTeal](https://roiharel.github.io/MBRP/plots/kmls/day/TrickyTeal.kmz)

[SneakySilver](https://roiharel.github.io/MBRP/plots/kmls/day/SneakySilver.kmz)

[RubyRunners](https://roiharel.github.io/MBRP/plots/kmls/day/RubyRunners.kmz)

[Purple](https://roiharel.github.io/MBRP/plots/kmls/day/Purple.kmz)

[PhantomWest](https://roiharel.github.io/MBRP/plots/kmls/day/PhantomWest.kmz)

[Periwinkle](https://roiharel.github.io/MBRP/plots/kmls/day/Periwinkle.kmz)

[Maroon](https://roiharel.github.io/MBRP/plots/kmls/day/Maroon.kmz)

[Magenta](https://roiharel.github.io/MBRP/plots/kmls/day/Magenta.kmz)

[Lilac](https://roiharel.github.io/MBRP/plots/kmls/day/Lilac.kmz)

[LapisSplinter](https://roiharel.github.io/MBRP/plots/kmls/day/LapisSplinter.kmz)

[Lapis](https://roiharel.github.io/MBRP/plots/kmls/day/Lapis.kmz)

[Jade](https://roiharel.github.io/MBRP/plots/kmls/day/Jade.kmz)

[Green](https://roiharel.github.io/MBRP/plots/kmls/day/Green.kmz)

[Emerald](https://roiharel.github.io/MBRP/plots/kmls/day/Emerald.kmz)

[Copper](https://roiharel.github.io/MBRP/plots/kmls/day/Copper.kmz)

[Chartreuse](https://roiharel.github.io/MBRP/plots/kmls/day/Chartreuse.kmz)

[Bronze](https://roiharel.github.io/MBRP/plots/kmls/day/Bronze.kmz)

## Animations by group

[All groups](https://roiharel.github.io/MBRP/plots/animations/MBRP_all_groups_Jan2025.mp4)


## Weather comparison
[MRC](https://roiharel.github.io/MBRP/plots/htmls/validate_weather_data.html)