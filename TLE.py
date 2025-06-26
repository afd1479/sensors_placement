# This is pre installtion code 
# - conda install -c anaconda skyfield-data
# - pip install sgp4==2.13
# - conda install -c conda-forge skyfield



# This code works very well. Without Alt

from skyfield.api import Topos, load, wgs84
from datetime import datetime

# Function to compute satellite positions
def get_satellite_positions(satellites, time=None):
    if time is None:
        time = datetime.utcnow()

    ts = load.timescale()
    t = ts.utc(time.year, time.month, time.day, time.hour, time.minute, time.second)

    positions = {}
    for satellite in satellites:
        name = satellite.name
        geocentric = satellite.at(t)
        lat, lon = wgs84.latlon_of(geocentric)
        positions[name] = (lat.degrees, lon.degrees)
    return positions

# Load TLE data from file
satellites = load.tle_file('TLE_data.txt')  # Replace with your TLE file path

# Get current positions of the satellites
positions = get_satellite_positions(satellites)
for name, (lat, lon) in positions.items():
    print(f"{name}: Latitude = {lat}, Longitude = {lon}")
    
    
    
    
############# With Alt

from skyfield.api import Topos, load, wgs84
from datetime import datetime



# Function to compute satellite positions and altitude
def get_satellite_positions_and_altitude(satellites, time=None):
    if time is None:
        time = datetime.utcnow()

    ts = load.timescale()
    t = ts.utc(time.year, time.month, time.day, time.hour, time.minute, time.second)

    positions = {}
    for satellite in satellites:
        name = satellite.name
        geocentric = satellite.at(t)
        subpoint = wgs84.subpoint(geocentric)
        lat, lon = subpoint.latitude.degrees, subpoint.longitude.degrees
        # Altitude in kilometers
        altitude = subpoint.elevation.m / 1000  # converting meters to kilometers
        positions[name] = (lat, lon, altitude)
    return positions

# Load TLE data from file
satellites = load.tle_file('TLE_data.txt')  # Replace with your TLE file path

# Get current positions and altitudes of the satellites
positions = get_satellite_positions_and_altitude(satellites)
for name, (lat, lon, alt) in positions.items():
    print(f"{name}: Latitude = {lat}°, Longitude = {lon}°, Altitude = {alt} km")
    
    
################ One Sat over time period 

from skyfield.api import load, wgs84, EarthSatellite
from datetime import datetime, timedelta

# TLE data for IRIDIUM 106
line1 = "1 41917U 17003A   24035.64030421 -.00000037  00000+0 -20349-4 0  9990"
line2 = "2 41917  86.3933  89.4340 0001965 104.7596 255.3818 14.34217095369430"

# Create satellite object from TLE lines
satellite = EarthSatellite(line1, line2)

# Load timescale
ts = load.timescale()

# Define the time period
start_time = datetime.utcnow()
end_time = start_time + timedelta(days=1)  # For one day
time_interval = timedelta(minutes=30)  # Interval of 30 minutes

# Loop over the time period
current_time = start_time
while current_time <= end_time:
    t = ts.utc(current_time.year, current_time.month, current_time.day,
               current_time.hour, current_time.minute, current_time.second)

    # Calculate position using subpoint method
    geocentric = satellite.at(t)
    subpoint = wgs84.subpoint(geocentric)
    lat, lon, alt = subpoint.latitude.degrees, subpoint.longitude.degrees, subpoint.elevation.km

    print(f"Time: {current_time}, Latitude: {lat}, Longitude: {lon}, Altitude: {alt} km")

    current_time += time_interval
    
    
###### This if for the whole satalites systems


from skyfield.api import load, wgs84, EarthSatellite
from datetime import datetime, timedelta

# Load TLE data from a file
def load_tle_data(file_path):
    with open(file_path) as f:
        lines = f.readlines()

    satellites = []
    for i in range(0, len(lines), 3):
        name = lines[i].strip()
        line1 = lines[i + 1].strip()
        line2 = lines[i + 2].strip()
        satellites.append((name, line1, line2))
    return satellites

# Function to get positions over time
def get_positions_over_time(satellites, start_time, end_time, time_interval):
    ts = load.timescale()
    positions = {}

    for name, line1, line2 in satellites:
        satellite = EarthSatellite(line1, line2)
        positions[name] = []
        
        current_time = start_time
        while current_time <= end_time:
            t = ts.utc(current_time.year, current_time.month, current_time.day,
                       current_time.hour, current_time.minute, current_time.second)
            geocentric = satellite.at(t)
            subpoint = wgs84.subpoint(geocentric)
            lat, lon, alt = subpoint.latitude.degrees, subpoint.longitude.degrees, subpoint.elevation.km
            positions[name].append((current_time, lat, lon, alt))
            current_time += time_interval

    return positions

# Load TLE data from your file
file_path = 'TLE_data.txt'
satellites = load_tle_data(file_path)

# Define the time period
start_time = datetime.utcnow()
end_time = start_time + timedelta(days=1)  # For one day
time_interval = timedelta(minutes=30)  # Interval of 30 minutes

# Get positions
satellite_positions = get_positions_over_time(satellites, start_time, end_time, time_interval)

# Print positions
for name, positions in satellite_positions.items():
    print(f"Positions for {name}:")
    for time, lon, lat, alt in positions:
        print(f"Time: {time}, Longitude: {lon}, Latitude: {lat}, Altitude: {alt} km")



        
 #### save the data for files

# Save positions to a file
output_file = 'satellite_positions_with_time.txt'
with open(output_file, 'w') as file:
    for name, positions in satellite_positions.items():
        #file.write(f"Positions for {name}:\n")
        for time, lat, lon, alt in positions:
            #file.write(f"Time: {time}, Latitude: {lat}, Longitude: {lon}, Altitude: {alt} km\n")
            file.write(f" {name}, {time},   {lon}, {lat}, {alt} \n")
        #file.write("\n")  # Adding a newline for readability between satellites
        
        
        
        
  ####### Code and save one time screen

from skyfield.api import Topos, load, wgs84
from datetime import datetime

# Function to compute satellite positions and altitude
def get_satellite_positions_and_altitude(satellites, time=None):
    if time is None:
        time = datetime.utcnow()

    ts = load.timescale()
    t = ts.utc(time.year, time.month, time.day, time.hour, time.minute, time.second)

    positions = {}
    for satellite in satellites:
        name = satellite.name
        geocentric = satellite.at(t)
        subpoint = wgs84.subpoint(geocentric)
        lat, lon = subpoint.latitude.degrees, subpoint.longitude.degrees
        # Altitude in kilometers
        altitude = subpoint.elevation.m   
        positions[name] = (lon, lat, altitude)
    return positions

# Load TLE data from file
satellites = load.tle_file('TLE_data.txt')  # Replace with your TLE file path

# Get current positions and altitudes of the satellites
positions = get_satellite_positions_and_altitude(satellites)
for name, (lon, lat, alt) in positions.items():
    print(f"{name}: Longitude = {lon}°, Latitude = {lat}°, Altitude = {alt} ")
    
output_file = 'satellite_positions_2.txt'
with open(output_file, 'w') as file:
    for name, (lon, lat, alt) in positions.items():
        #file.write(f"Positions for {name}:\n")
        #for time, lat, lon, alt in positions:
        file.write(f"  {name} ,  {lon} ,  {lat},  {alt} \n")
        #file.write("\n")  # Adding a newline for readability between satellites 