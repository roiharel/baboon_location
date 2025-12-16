# MBRP - Mpala Baboon Research Project

## Overview

The MBRP (Mpala Baboon Research Project) is a comprehensive data processing and monitoring system for analyzing GPS and accelerometer tracking data from wild baboons. This project provides automated workflows for downloading, cleaning, and visualizing movement data to support field research and real-time monitoring of collared individuals across multiple social groups.

## Data Pipeline

### Data Acquisition

**Source**: All tracking data is downloaded from [Movebank](https://www.movebank.org/) (Study ID: 3445611111)

The project retrieves two main types of sensor data:
- **GPS data**: Location coordinates, speed, heading, battery voltage, and fix quality metrics
- **Acceleration data**: Raw tri-axial accelerometer readings for behavioral analysis

Data is downloaded using the `move2` R package, which connects directly to the Movebank API.  The download process includes automatic removal of Movebank-flagged outliers and filtering of empty records.

### Data Cleaning and Processing

Raw data undergoes several cleaning steps to ensure quality and usability:

#### GPS Data Cleaning (`code/functions/prep_gps_movebank.R`)
- **Speed filtering**: Removes biologically implausible movements (threshold: 5 m/s)
- **Spatial filtering**: Restricts data to study area boundaries (36.7°-37°E, 0. 2°-0.6°N)
- **Elevation filtering**: Removes erroneous fixes above 2000m elevation
- **Satellite filtering**: Excludes fixes with zero satellite counts
- **Temporal filtering**:  Regularizes data to specified time intervals (1 min, 10 min, 1 hour options)

#### Metadata Integration
- Individual identifiers (animal_id, tag_id)
- Social group membership (group_id)
- Sex and age class
- Deployment information

#### Additional Processing
- **Coordinate extraction**: Converts spatial objects to lat/lon and UTM coordinates
- **Movement metrics**: Calculates speed, azimuth, and heading between consecutive locations
- **Time zones**: Converts UTC timestamps to local time (Africa/Nairobi, UTC+3)

### Data Outputs

The pipeline generates multiple output formats optimized for different analytical purposes:

#### A. Cleaned GPS Data Files
**Location**:  `data/processed/YYYY/gps/v1_cleaned/`

1. **`gps_v1.RDS`** - Primary cleaned dataset (R spatial format)
   - Contains all fields from original data plus derived metrics
   - Preserved as `move2` object for spatial operations
   - Fields include: timestamp, location, speed, heading, battery, group_id, sex, age

2. **Location Matrices** (Parquet & CSV formats):
   - **`lats.parquet/csv`** - Latitude matrix (time × individual)
   - **`lons.parquet/csv`** - Longitude matrix (time × individual)
   - **`xs.parquet/csv`** - UTM X coordinate matrix
   - **`ys.parquet/csv`** - UTM Y coordinate matrix
   - **`ids.csv`** - List of animal identifiers
   - **`times.csv`** - Complete time grid

   These matrices are time-aligned with regular intervals (default:  2 minutes) during daytime hours (03:00-16:00), facilitating: 
   - Synchronous analysis across individuals
   - Group-level movement analysis
   - Social network construction
   - Inter-individual distance calculations


#### B. Interactive Visualizations (HTML)
**Location**: `plots/htmls/`

Generated for real-time field monitoring: 

1. **`table_baboon_data_records.html`** - Interactive data table
   - Current status of all collars
   - Battery levels, fix rates, last download dates
   - Color-coded alerts for maintenance needs

2. **`baboon_interactive_map.html`** - Day and night locations
   - All GPS points with group color-coding
   - Filterable by individual, group, and date

3. **`baboon_night_interactive_map.html`** - Nighttime locations only
   - Sleep site identification (17:00-05:00 hours)

4. **`prop_sleep_site_map.html`** - Sleep site usage analysis
   - DBSCAN clustering of nighttime locations
   - Proportional representation of group usage per site

5. **`transitions_group_sleeping_sites_map.html`** - Sleep site transitions
   - Movement patterns between sleeping sites
   - Group-level ranging behavior

6. **`all_ind_distance_plot.html`** - Cumulative distance traveled
   - Used to detect collar failures and potential mortalities
   - 4-day rolling window analysis

7. **`baboon_data_records. html`** - Data collection timeline
   - Records per individual over time
   - Gaps indicate transmission issues

8. **`baboon_data_batt_plot.html`** - Battery voltage trajectories
   - Tracks battery depletion rates
   - Predicts collar longevity

#### C. KML/KMZ Files for Field Use
**Location**: `plots/kmls/`

Google Earth-compatible files for field navigation: 

- **`last_week. kmz`** - Recent locations (7-day window)
- **Group-specific files** (e.g., `TrickyTeal. kmz`, `RubyRunners.kmz`)
  - Separate KMZ for each social group
  - Complete tracking history per group
  - Useful for field observations and collar recovery

#### D. Animations
**Location**: `plots/animations/`

- **`MBRP_all_groups_Jan2025.mp4`** - Animated movement trajectories
  - Temporal visualization of all groups
  - Illustrates ranging patterns and inter-group dynamics

## Monitoring and Analysis Workflows

### Real-Time Collar Monitoring

The `basic_monitoring_collars.R` script runs automated checks: 
- Downloads latest data from Movebank
- Generates all visualization outputs
- Flags potential issues: 
  - **Red alerts**: Collars inactive >21 days or mortality suspects
  - **Blue alerts**: Low battery requiring status change
  - **Orange alerts**: Recent data gaps

### Sleep Site Analysis

Sleep site clustering (`updated_sleep_site_John. R`):
1. Filters nighttime GPS data (17:00-05:00)
2. Applies DBSCAN clustering (eps=0.005, minPts=5)
3. Calculates site centroids and group usage
4. Identifies shared vs. exclusive sleeping sites

### Movement Analysis

Distance calculations and ranging metrics:
- Daily path length
- Home range estimation
- Inter-individual distances
- Group cohesion metrics

---

## Quick Links

### Monitoring Tracking Data

[Field control table](https://roiharel.github.io/MBRP/plots/htmls/table_baboon_data_records.html)

[Map - day&night](https://roiharel.github.io/MBRP/plots/htmls/baboon_interactive_map.html)

[Cum.  dist - possible mortality](https://roiharel.github.io/MBRP/plots/htmls/all_ind_distance_plot.html)

[Data records](https://roiharel.github.io/MBRP/plots/htmls/baboon_data_records.html)

[Battery trajectory](https://roiharel.github.io/MBRP/plots/htmls/baboon_data_batt_plot.html)

[Map - night](https://roiharel.github.io/MBRP/plots/htmls/baboon_night_interactive_map.html)

[Map - proportion sleep sites](https://roiharel.github.io/MBRP/plots/htmls/prop_sleep_site_map.html)

[Map - Sleeping sites transitions](https://roiharel.github.io/MBRP/plots/htmls/transitions_group_sleeping_sites_map.html)

### KML Files

[Last week](https://roiharel.github.io/MBRP/plots/kmls/last_week.kmz)

**By Group:**
[TrickyTeal](https://roiharel.github.io/MBRP/plots/kmls/day/TrickyTeal.kmz) | 
[SneakySilver](https://roiharel.github.io/MBRP/plots/kmls/day/SneakySilver.kmz) | 
[RubyRunners](https://roiharel.github.io/MBRP/plots/kmls/day/RubyRunners.kmz) | 
[Purple](https://roiharel.github.io/MBRP/plots/kmls/day/Purple.kmz) | 
[PhantomWest](https://roiharel.github.io/MBRP/plots/kmls/day/PhantomWest.kmz) | 
[Periwinkle](https://roiharel.github.io/MBRP/plots/kmls/day/Periwinkle.kmz) | 
[Maroon](https://roiharel.github.io/MBRP/plots/kmls/day/Maroon.kmz) | 
[Magenta](https://roiharel.github.io/MBRP/plots/kmls/day/Magenta.kmz) | 
[Lilac](https://roiharel.github.io/MBRP/plots/kmls/day/Lilac.kmz) | 
[LapisSplinter](https://roiharel.github.io/MBRP/plots/kmls/day/LapisSplinter.kmz) | 
[Lapis](https://roiharel.github.io/MBRP/plots/kmls/day/Lapis.kmz) | 
[Jade](https://roiharel.github.io/MBRP/plots/kmls/day/Jade.kmz) | 
[Green](https://roiharel.github.io/MBRP/plots/kmls/day/Green.kmz) | 
[Emerald](https://roiharel.github.io/MBRP/plots/kmls/day/Emerald.kmz) | 
[Copper](https://roiharel.github.io/MBRP/plots/kmls/day/Copper.kmz) | 
[Chartreuse](https://roiharel.github.io/MBRP/plots/kmls/day/Chartreuse.kmz) | 
[Bronze](https://roiharel.github.io/MBRP/plots/kmls/day/Bronze. kmz)

### Animations

[All groups](https://roiharel.github.io/MBRP/plots/animations/MBRP_all_groups_Jan2025.mp4)

### Weather Comparison

[MRC](https://roiharel.github.io/MBRP/plots/htmls/validate_weather_data.html)

---

## Technical Requirements

### R Packages
- `move2` - Movebank data access
- `sf`, `lwgeom` - Spatial data handling
- `dplyr`, `data.table` - Data manipulation
- `ggplot2`, `plotly` - Visualization
- `leaflet` - Interactive maps
- `dbscan` - Clustering algorithms
- `arrow` - Parquet file I/O

### External Dependencies
- Movebank account credentials
- Google Maps API key (for basemaps)
- Decoder software (for local data extraction)
