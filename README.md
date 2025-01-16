# MBRP

## Overview
MBRP documentation project aims to provide comprehensive details about the various functionalities and scripts used for managing and analyzing tracking data.

Plotting Tracks
plot_tracks_2024.R are used for processing and visualizing animal tracking data.
The script processes and visualizes animal tracking data collected since March 2024. It connects to Movebank, downloads GPS data, cleans it, and generates various plots and interactive visualizations using libraries like ggplot2, leaflet, and plotly. The script also creates summary tables and maps based on the tracking data.

Interactive Maps
The plot_leaflet_basic.R script generates interactive maps using Leaflet, visualizing animal tracking data with different base layers and overlays. It filters and groups the data, creates color-coded markers for various data subsets, and includes user-interactive features such as clickable coordinates and layer controls. The script saves the resulting maps as HTML files for easy viewing and sharing.

Sleep Site Maps
The sleep_site_mapbox.R script clusters GPS data points to identify sleep sites using DBSCAN, calculates centroids for each cluster, and visualizes the proportions of different groups at each sleep site using pie charts on an interactive Leaflet map. The map includes various base layers and allows for easy visualization of the clustered data.

baboon_movement_plots_and_graphs (John)
This code processes and visualizes baboon movement data. It downloads GPS data from Movebank, cleans and merges it with metadata, and creates several plots: a bar plot showing the number of individuals per group, a line plot of group distribution over time, faceted plots showing distribution per group over time, and a spatial distribution plot of group movements by month.

[Interactive Map](https://roiharel.github.io/MBRP/plots/prop_sleep_site_map_2024.html)

## Map - all
[View the map](https://<your-username>.github.io/MBRP/example.html)

## Map - sleep
[View the map](https://<your-username>.github.io/MBRP/example.html)

## Tracking effort
[View the plot](https://<your-username>.github.io/MBRP/example.html)

## Battery
[View the plot](https://<your-username>.github.io/MBRP/example.html)

