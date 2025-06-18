# Ensure required packages are installed
import subprocess
import sys

def install(package):
    subprocess.check_call([sys.executable, "-m", "pip", "install", package])

# List of required packages
required_packages = ["pandas", "simplekml", "matplotlib", "numpy"]

for package in required_packages:
    try:
        __import__(package)
    except ImportError:
        print(f"Installing {package}...")
        install(package)

# Import the required libraries
import pandas as pd
import simplekml
from datetime import datetime, timedelta
import matplotlib.pyplot as plt
import matplotlib.colors as mcolors
import numpy as np
import os

# Parameter for the time window in minutes
time_window_minutes = 30
buffer_time = 5
morning_hour = 9

# Read the CSV file
os.chdir('c:\\Users\\user01\\Documents\\Github\\MBRP')

# Check if the file exists
if not os.path.exists('gps_v1.csv'):
    raise FileNotFoundError("The file gps_v1.csv does not exist in the specified directory.")

# Read the CSV file
try:
    data = pd.read_csv('gps_v1.csv')
except pd.errors.EmptyDataError:
    raise ValueError("The file gps_v1.csv is empty or has no columns to parse.")


# Convert timestamp to datetime for easier manipulation
# data['timestamp'] = pd.to_datetime(data['timestamp'], format='%Y-%m-%d %H:%M:%S.%f')

data.timestamp = data.orientation_quaternion_raw_z
data.timestamp = pd.to_datetime(data.timestamp)
data.individual_local_identifier = data.sensor_type_id

# Group the data by 'group_id'
grouped = data.groupby('group_id')

# Define distinct base colors for each group
base_colors = [
    (1.0, 0.9, 0.6),  # Light Yellow
    (1.0, 0.6, 0.6),  # Light Red
    (0.6, 1.0, 0.6),  # Light Green
    (0.6, 0.6, 1.0),  # Light Blue
    (1.0, 1.0, 0.6),  # Light Lime
    (1.0, 0.6, 1.0),  # Light Magenta
    (0.3, 0.7, 0.7),  # Teal
    (1.0, 0.8, 0.6),  # Light Orange
    (0.8, 0.6, 1.0),  # Light Purple
    (0.9, 0.9, 0.9),  # Light Grey
    (0.9, 0.7, 0.5)   # Light Brown
]

# Function to generate a gradient of colors
def generate_gradient(base_color, num_colors):
    return [mcolors.to_hex((base_color[0] * (1 - i / num_colors),
                            base_color[1] * (1 - i / num_colors),
                            base_color[2] * (1 - i / num_colors))) for i in range(num_colors)]

## full tracks
# Create a KMZ file for each group
for group_index, (group_id, group_data) in enumerate(grouped):
    kml = simplekml.Kml()
    
    # Sort data by individual and timestamp
    group_data = group_data.sort_values(by=['individual_local_identifier', 'timestamp'])
    
    # Get unique individuals in the group
    individuals = group_data['individual_local_identifier'].unique()
    num_individuals = len(individuals)
    
    # Generate a gradient of colors for the group
    base_color = base_colors[group_index % len(base_colors)]
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
            window_data = individual_data[(individual_data['timestamp'] >= start_time - timedelta(minutes=buffer_time)) & 
                                          (individual_data['timestamp'] <= end_time + timedelta(minutes=buffer_time))]
            
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
                
                # Set the visibility of the line to 0
                # line.visibility = 0
            
            # Move to the next time window
            start_time = end_time
            end_time = start_time + timedelta(minutes=time_window_minutes)
                
    # Save the KMZ file with the group_id as part of the filename
    kml.savekmz(f"plots\\kmls\\day\\{group_id}.kmz")

## Morning tracks
# Filter data to include only tracks before noon
data_mor = data[data['timestamp'].dt.hour < morning_hour]

grouped = data_mor.groupby('group_id')

# Create a KMZ file for each group
for group_index, (group_id, group_data) in enumerate(grouped):
    kml = simplekml.Kml()
    
    # Sort data by individual and timestamp
    group_data = group_data.sort_values(by=['individual_local_identifier', 'timestamp'])
    
    # Get unique individuals in the group
    individuals = group_data['individual_local_identifier'].unique()
    num_individuals = len(individuals)
    
    # Generate a gradient of colors for the group
    base_color = base_colors[group_index % len(base_colors)]
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
            window_data = individual_data[(individual_data['timestamp'] >= start_time - timedelta(minutes=buffer_time)) & 
                                          (individual_data['timestamp'] <= end_time + timedelta(minutes=buffer_time))]
            
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
                
                # Set the visibility of the line to 0
                # line.visibility = 0
            
            # Move to the next time window
            start_time = end_time
            end_time = start_time + timedelta(minutes=time_window_minutes)
                
    # Save the KMZ file with the group_id as part of the filename
 ###   kml.savekmz(f"morning\\{group_id}.kmz")

 # Get the current date and calculate the start of the last week
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
        folder = kml.newfolder(name=f"{individual_id} ({tag_local_identifier})")
        folder.visibility = 0  # Set folder visibility to 0 (hidden)

        tag_local_identifier = individual_data['tag_local_identifier'].iloc[0] if not individual_data['tag_local_identifier'].empty else 'Unknown'
        # Create a multiline for the individual
        line = folder.newlinestring(name=f"{individual_id} ({tag_local_identifier}) - Last Week")
        line.coords = list(zip(individual_data['location.long'], individual_data['location.lat']))

        # Set the color using the group color with alpha
        kml_color = 'b2' + group_color[1:]  # Add alpha to hex color
        line.style.linestyle.color = kml_color
        line.style.linestyle.width = 5  # Set line width

        # Set the timespan for the multiline
        line.timespan.begin = individual_data['timestamp'].min().strftime('%Y-%m-%dT%H:%M:%SZ')
        line.timespan.end = individual_data['timestamp'].max().strftime('%Y-%m-%dT%H:%M:%SZ')

# Save the KMZ file for the last week
kml.savekmz("plots\\kmls\\last_week.kmz")
print("KMZ file for the last week saved successfully.")