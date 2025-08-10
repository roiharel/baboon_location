import pandas as pd
import simplekml
from datetime import datetime, timedelta
import matplotlib.pyplot as plt
import matplotlib.colors as mcolors
import numpy as np
import os
from matplotlib.colors import to_rgb, to_hex

# Parameters
time_window_minutes = 60
buffer_time = 2
morning_hour = 9

# Read the CSV file
os.chdir('C:\\Users\\meerkat\\Documents\\MBRP')  # Change to the correct directory
data = pd.read_csv('data\\gps_v1.csv')

# Convert timestamp to datetime for easier manipulation
#data['timestamp'] = pd.to_datetime(data['timestamp'], format='%Y-%m-%d %H:%M:%S.%f')
data['timestamp'] = pd.to_datetime(data['timestamp'], format='ISO8601')
# Define distinct base colors for each group
base_colors = {
    "Maroon": "#800000",        # Maroon
    "Chartreuse": "#7FFF00",    # Chartreuse
    "Bronze": "#CD7F32",        # Bronze
    "Emerald": "#50C878",       # Emerald
    "Lilac": "#C8A2C8",         # Lilac
    "Copper": "#B87333",        # Copper
    "Magenta": "#FF00FF",       # Magenta
    "LapisSplinter": "#87CEFA", # LapisSplinter
    "Lapis": "#26619C",         # Lapis
    "Periwinkle": "#CCCCFF",    # Periwinkle
    "PhantomWest": "#FF0000",   # Red
    "Teal": "#008080",          # Teal
    "sneakySilver": "#C0C0C0",  # Silver
    "Purple": "#800080"         # Purple
}

# Function to generate a gradient of colors
def generate_gradient(base_color, num_colors):
    # Convert hex color to RGB using mcolors
    base_color_rgb = to_rgb(base_color)
    
    # Generate gradient in RGB
    gradient_rgb = [
        (
            base_color_rgb[0] * (1 - i / num_colors),
            base_color_rgb[1] * (1 - i / num_colors),
            base_color_rgb[2] * (1 - i / num_colors)
        )
        for i in range(num_colors)
    ]
    
    # Convert RGB back to hex using mcolors
    gradient_hex = [to_hex(color) for color in gradient_rgb]
    
    return gradient_hex

# Full tracks
grouped = data.groupby('group_id')
for group_index, (group_id, group_data) in enumerate(grouped):
    kml = simplekml.Kml()
    
    # Sort data by individual and timestamp
    group_data = group_data.sort_values(by=['individual_local_identifier', 'timestamp'])
    
    # Get unique individuals in the group
    individuals = group_data['individual_local_identifier'].unique()
    num_individuals = len(individuals)
    
    # Generate a gradient of colors for the group
    base_color = base_colors[group_id]
    gradient_colors = generate_gradient(base_color, num_individuals)
    
    # Map each individual to a color
    color_map = {individual: gradient_colors[i] for i, individual in enumerate(individuals)}
    
    # Iterate over each individual
    for individual_id, individual_data in group_data.groupby('individual_local_identifier'):
        # Create a folder for each individual
        folder = kml.newfolder(name=individual_id)
        folder.visibility = 0  # Set folder visibility to 0 (hidden)
        
        # Initialize the start time for the first segment
        start_time = individual_data['timestamp'].min()
        end_time = start_time + timedelta(minutes=time_window_minutes)
        
        while start_time < individual_data['timestamp'].max():
            # Filter data within the current time window
            window_data = individual_data[
                (individual_data['timestamp'] >= start_time - timedelta(minutes=buffer_time)) &
                (individual_data['timestamp'] <= end_time + timedelta(minutes=buffer_time))
            ]
            
            if not window_data.empty:
                label = f"{individual_id} {start_time.strftime('%Y-%m-%d %H:%M:%S')}"
                line = folder.newlinestring(name=label)
                line.coords = list(zip(window_data['location.long'], window_data['location.lat']))
                
                # Set the color using the color map with alpha
                hex_color = color_map[individual_id]
                kml_color = 'b2' + hex_color[1:]  # Add alpha to hex color
                line.style.linestyle.color = kml_color
                line.style.linestyle.width = 5  # Set line width
                
                # Set the timespan for each line
                line.timespan.begin = start_time.strftime('%Y-%m-%dT%H:%M:%SZ')
                line.timespan.end = end_time.strftime('%Y-%m-%dT%H:%M:%SZ')
            
            # Move to the next time window
            start_time = end_time
            end_time = start_time + timedelta(minutes=time_window_minutes)
    
    # Save the KMZ file with the group_id as part of the filename
    kml.savekmz(f"plots\\kmls\\day\\{group_id}.kmz")
    print(f"KMZ file for group {group_id} saved successfully.")

# Get the current date and calculate the start of the last week
current_date = datetime.now().astimezone()  # Make current_date timezone-aware
last_week_start = current_date - timedelta(days=7)

# Filter data for the last week
data_last_week = data[(data['timestamp'] >= last_week_start) & (data['timestamp'] <= current_date)]

# Get the current date and calculate the start of the last week
# data['timestamp'] = pd.to_datetime(data['timestamp']).dt.tz_localize('UTC')

current_date = datetime.now().astimezone()  # Make current_date timezone-aware
last_week_start = current_date - timedelta(days=7)

# Filter data for the last week
data_last_week = data[(data['timestamp'] >= last_week_start) & (data['timestamp'] <= current_date)]

# Group the data by 'group_id'
grouped = data_last_week.groupby('group_id')

# Create a KMZ file for the last week
kml = simplekml.Kml()

# Iterate over each group
for group_id, group_data in grouped:
    # Get the color for the group from base_colors
    group_color = base_colors.get(group_id, "#000000")  # Default to black if not found

    # Iterate over each individual in the group
    for individual_id, individual_data in group_data.groupby('individual_local_identifier'):
        # Create a folder for each individual
        tag_local_identifier = individual_data['tag_local_identifier'].iloc[0] if not individual_data['tag_local_identifier'].empty else 'Unknown'
        folder = kml.newfolder(name=f"{individual_id} ({tag_local_identifier})")
        folder.visibility = 0  # Set folder visibility to 0 (hidden)
        
        # Initialize the start time for the first segment
        start_time = individual_data['timestamp'].min()
        end_time = start_time + timedelta(hours=1)
        
        while start_time < individual_data['timestamp'].max():
            # Filter data within the current time window
            window_data = individual_data[
                (individual_data['timestamp'] >= start_time) &
                (individual_data['timestamp'] < end_time)
            ]
            
            if not window_data.empty:
                label = f"{individual_id} ({tag_local_identifier}) {start_time.strftime('%Y-%m-%d %H:%M:%S')}"
                line = folder.newlinestring(name=label)
                line.coords = list(zip(window_data['location.long'], window_data['location.lat']))
                
                # Set the color using the group color with alpha
                kml_color = 'b2' + group_color[1:]  # Add alpha to hex color
                line.style.linestyle.color = kml_color
                line.style.linestyle.width = 5  # Set line width
                
                # Set the timespan for each line
                line.timespan.begin = start_time.strftime('%Y-%m-%dT%H:%M:%SZ')
                line.timespan.end = end_time.strftime('%Y-%m-%dT%H:%M:%SZ')
            
            # Move to the next time window
            start_time = end_time
            end_time = start_time + timedelta(hours=1)

# Save the KMZ file for the last week
kml.savekmz("plots\\kmls\\last_week.kmz")
print("KMZ file for the last week saved successfully.")