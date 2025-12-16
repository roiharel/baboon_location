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
time_window_days = 10
buffer_time = 2
morning_hour = 9

# Read the CSV file
os.chdir('C:\\Users\\meerkat\\Documents\\MBRP')  # Change to the correct directory
#data = pd.read_csv('data\\gps_v1.csv')
data = pd.read_csv('Z:/baboon/working/data/processed/2025/gps/v1_cleaned/gps_v1.csv')


# Convert timestamp to datetime for easier manipulation
#data['timestamp'] = pd.to_datetime(data['timestamp'], format='%Y-%m-%d %H:%M:%S.%f')
data['timestamp'] = pd.to_datetime(data['timestamp'], format='ISO8601')
# Define distinct base colors for each group [rrggbb]
# base_colors = {
#     "Maroon": "#800000",        # Maroon
#     "Chartreuse": "#7FFF00",    # Chartreuse
#     "Bronze": "#CD7F32",        # Bronze
#     "Emerald": "#50C878",       # Emerald
#     "Lilac": "#C8A2C8",         # Lilac
#     "Copper": "#B87333",        # Copper
#     "Magenta": "#FF00FF",       # Magenta
#     "LapisSplinter": "#87CEFA", # LapisSplinter
#     "Lapis": "#26619C",         # Lapis
#     "Periwinkle": "#CCCCFF",    # Periwinkle
#     "PhantomWest": "#FF0000",   # Red
#     "Teal": "#008080",          # Teal
#     "SneakySilver": "#C0C0C0",  # Silver
#     "Purple": "#800080",         # Purple
#     "Green": "#008000",         # Green
#     "Jade": "#00A86B",
#     "RubyRunners": "#E0115F"
# }
# [ggbbrr]


base_colors = {
    "Maroon":    "#000080",  # Maroon      (bbggrr: 00 00 80)
    "Chartreuse":"#00FF7F",  # Chartreuse  (bbggrr: 00 FF 7F)
    "Bronze":    "#327FCD",  # Bronze      (bbggrr: 32 7F CD)
    "Emerald":   "#78C850",  # Emerald     (bbggrr: 78 C8 50)
    "Lilac":     "#C8A2C8",  # Lilac       (bbggrr: A2 C8 C8) #A2C8C8
    "Copper":    "#3373B8",  # Copper      (bbggrr: 33 73 B8)
    "Magenta":   "#FF00FF",  # Magenta     (bbggrr: FF 00 FF)
    "LapisSplinter":"#FACE87",# LapisSplinter (bbggrr: FA CE 87)
    "Lapis":     "#961C26",  # Lapis       (bbggrr: 1C 96 26) # 
    "Periwinkle":"#CCCCFF",  # Periwinkle  (bbggrr: CC FF CC) # CCFFCC
    "PhantomWest":"#0000FF", # Red         (bbggrr: 00 00 FF)
    "TrickyTeal":      "#808000",  # Teal        (bbggrr: 80 80 00)
    "SneakySilver":"#C0C0C0",# Silver      (bbggrr: C0 C0 C0)
    "Purple":    "#800080",  # Purple      (bbggrr: 00 80 80) #008080
    "Green":     "#008000",  # Green       (bbggrr: 00 80 00)
    "Jade":      "#00A86B",  # Jade        (bbggrr: A8 86 00)
    "RubyRunners":"#1E119B"  # RubyRunners (bbggrr: 11 5F E0) # 9b111e
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
    group_data = group_data.sort_values(by=['animal_id', 'timestamp'])
    
    # Get unique individuals in the group
    individuals = group_data['animal_id'].unique()
    num_individuals = len(individuals)
    
    # Generate a gradient of colors for the group
    base_color = base_colors[group_id]
    gradient_colors = generate_gradient(base_color, num_individuals)
    
    # Map each individual to a color
    color_map = {individual: gradient_colors[i] for i, individual in enumerate(individuals)}
    
    # Iterate over each individual
    for individual_id, individual_data in group_data.groupby('animal_id'):
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
last_period_start = current_date - timedelta(days=time_window_days)

# Filter data for the last period
data_last_period = data[(data['timestamp'] >= last_period_start) & (data['timestamp'] <= current_date)]

# Group the data by 'group_id'
grouped = data_last_period.groupby('group_id')

# Create a KMZ file for the last period
kml = simplekml.Kml()

# Iterate over each group
for group_id, group_data in grouped:
    # Get the color for the group from base_colors
    group_color = base_colors.get(group_id, "#000000")  # Default to black if not found

    # Iterate over each individual in the group
    for individual_id, individual_data in group_data.groupby('animal_id'):
        # Create a folder for each individual
        tag_id = individual_data['tag_id'].iloc[0] if not individual_data['tag_id'].empty else 'Unknown'
        folder = kml.newfolder(name=f"{individual_id} ({tag_id}) [{group_id}] ")
        folder.visibility = 0  # Set folder visibility to 0 (hidden)
        
        # Initialize the start time for the first segment
        start_time = individual_data['timestamp'].min()
        end_time = start_time + timedelta(minutes=20)
        
        while start_time <= individual_data['timestamp'].max():
            # Filter data within the current time window
            window_data = individual_data[
                (individual_data['timestamp'] >= start_time - timedelta(minutes=buffer_time)) &
                (individual_data['timestamp'] <= end_time + timedelta(minutes=buffer_time))
            ]
            
            if not window_data.empty:
                label = f"{group_id} {individual_id} ({tag_id}) {start_time.strftime('%Y-%m-%d %H:%M:%S')}"
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
            end_time = start_time + timedelta(minutes=20)

# Save the KMZ file for the last week
kml.savekmz("plots\\kmls\\last_week.kmz")
print("KMZ file for the last week saved successfully.")