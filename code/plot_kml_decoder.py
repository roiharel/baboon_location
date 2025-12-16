## Decoder Data
 
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

# Define distinct base colors for each group
base_colors = {
    "Maroon":    "#000080",
    "Chartreuse":"#00FF7F",
    "Bronze":    "#327FCD",
    "Emerald":   "#78C850",
    "Lilac":     "#C8A2C8",
    "Copper":    "#3373B8",
    "Magenta":   "#FF00FF",
    "LapisSplinter":"#FACE87",
    "Lapis":     "#961C26",
    "Periwinkle":"#CCCCFF",
    "PhantomWest":"#0000FF",
    "TrickyTeal":      "#808000",
    "SneakySilver":"#C0C0C0",
    "Purple":    "#800080",
    "Green":     "#008000",
    "Jade":      "#00A86B",
    "RubyRunners":"#1E119B"
}

# Function to generate a gradient of colors
def generate_gradient(base_color, num_colors):
    base_color_rgb = to_rgb(base_color)
    gradient_rgb = [
        (
            base_color_rgb[0] * (1 - i / num_colors),
            base_color_rgb[1] * (1 - i / num_colors),
            base_color_rgb[2] * (1 - i / num_colors)
        )
        for i in range(num_colors)
    ]
    gradient_hex = [to_hex(color) for color in gradient_rgb]
    return gradient_hex

# Decoder data processing
decoder_data = pd.read_csv('data/decoder_data_dec2025.csv')

decoder_data = decoder_data.rename(columns={
    'tag.serial.number': 'tag_id',
    'start.timestamp': 'timestamp',
    'longitude': 'location.long',
    'latitude': 'location.lat'
})

decoder_data['timestamp'] = pd.to_datetime(decoder_data['timestamp'], format='%Y-%m-%d %H:%M:%S.%f')

# Filter data to remove points outside the study area
decoder_data = decoder_data.loc[(decoder_data['location.long'] >= 36.7) & 
                                 (decoder_data['location.long'] <= 37.0) &
                                 (decoder_data['location.lat'] >= 0.2) & 
                                 (decoder_data['location.lat'] <= 0.6)]

# Read metadata to get animal_id and group_id based on tag_id
metadata = pd.read_csv('Z:/baboon/working/data/processed/2025/metadata/Baboons MBRP Mpala Kenya-reference-data.csv')

metadata = metadata.rename(columns={
    'tag-id': 'tag_id',
    'animal-id': 'animal_id',
    'animal-group-id': 'group_id'
})

# Merge decoder data with metadata
data_decoder = decoder_data.merge(metadata[['tag_id', 'animal_id', 'group_id']], on='tag_id', how='left')

# Create a single KMZ file for all decoder data
kml = simplekml.Kml()

grouped = data_decoder.groupby('group_id', dropna=False)

for group_id, group_data in grouped:
    group_color = base_colors.get(group_id, "#000000")
    
    group_folder = kml.newfolder(name=f"Group: {group_id}")

    for individual_id, individual_data in group_data.groupby('animal_id', dropna=False):
        tag_id = individual_data['tag_id'].iloc[0] if not individual_data['tag_id'].empty else 'Unknown'
        line_buffer_minutes = 120 if str(tag_id).startswith(('0', '5')) else buffer_time
        segment_minutes_val = 240 if str(tag_id).startswith('0') else 20
        
        folder = group_folder.newfolder(name=f"{individual_id} ({tag_id})")
        folder.visibility = 0
        
        start_time = individual_data['timestamp'].min()
        end_time = start_time + timedelta(minutes=segment_minutes_val)
        
        while start_time <= individual_data['timestamp'].max():
            window_data = individual_data[
                (individual_data['timestamp'] >= start_time - timedelta(minutes=line_buffer_minutes)) &
                (individual_data['timestamp'] <= end_time + timedelta(minutes=line_buffer_minutes))
            ]
            
            if not window_data.empty:
                label = f"{individual_id} ({tag_id}) {start_time.strftime('%Y-%m-%d %H:%M:%S')}"
                line = folder.newlinestring(name=label)
                line.coords = list(zip(window_data['location.long'], window_data['location.lat']))
                
                kml_color = 'b2' + group_color[1:]
                line.style.linestyle.color = kml_color
                line.style.linestyle.width = 5

                line.timespan.begin = start_time.strftime('%Y-%m-%dT%H:%M:%SZ')
                line.timespan.end = end_time.strftime('%Y-%m-%dT%H:%M:%SZ')
            
            start_time = end_time
            end_time = start_time + timedelta(minutes=segment_minutes_val)

kml.savekmz("plots\\kmls\\all_decoder_data.kmz")
print("KMZ file for all decoder data saved successfully.")

# Display and save summary table
summary_table = data_decoder.groupby('tag_id').agg({
    'timestamp': ['min', 'max'],
    'animal_id': 'first',
    'group_id': 'first'
}).reset_index()
summary_table.columns = ['tag_id', 'first_date', 'last_date', 'animal_id', 'group_id']

summary_table.to_csv('plots\\summary_decoder_data.csv', index=False)
print("\nSummary Table:")
print(summary_table)
print("\nSummary table saved to plots\\summary_decoder_data.csv")