import pandas as pd
import numpy as np
import folium

# Load the data
data = pd.read_csv('dantzig42_d.txt', header=None, delim_whitespace=True)

# Create a map
m = folium.Map()

# Iterate over the rows in the data
for i in range(len(data)):
    for j in range(i+1, len(data)):
        # Get the latitude and longitude of the start and end points
        start_lat = data.iloc[i, 0]
        start_lon = data.iloc[i, 1]
        end_lat = data.iloc[j, 0]
        end_lon = data.iloc[j, 1]

        # Add a line to the map connecting the start and end points
        folium.PolyLine([(start_lat, start_lon), (end_lat, end_lon)], color='blue').add_to(m)

# Save the map
m.save('map.html')
