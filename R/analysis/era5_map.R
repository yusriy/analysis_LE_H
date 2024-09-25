library(ggplot2)
library(ggmap)
library(ncdf4)

# Access the API key stored in .Renviron
api_key <- Sys.getenv("GOOGLE_MAPS_API_KEY")

# Register the Google Maps API key
register_google(key = api_key)

# Open the NetCDF file
nc_file <- nc_open("data/era5/era5_nearCEMACS_v2.nc")

# Extract the temperature data (t2m) and the associated dimensions
temp <- ncvar_get(nc_file, "t2m")  # Extract temperature in Kelvin
lat <- ncvar_get(nc_file, "latitude")
lon <- ncvar_get(nc_file, "longitude")
time <- ncvar_get(nc_file, "time")

# Convert the time dimension to a date format
time_units <- ncatt_get(nc_file, "time", "units")$value
time_origin <- strsplit(time_units, "since ")[[1]][2]
time <- as.POSIXct(time * 3600, origin = time_origin, tz = "UTC")

# Close the NetCDF file
nc_close(nc_file)

# Find the indices of the nearest longitude and latitude for the nine locations
lon_idx_1 <- which.min(abs(lon - 100.0))
lat_idx_1 <- which.min(abs(lat - 5.25))

lon_idx_2 <- which.min(abs(lon - 100.25))
lat_idx_2 <- which.min(abs(lat - 5.25))

lon_idx_3 <- which.min(abs(lon - 100.50))
lat_idx_3 <- which.min(abs(lat - 5.25))

lon_idx_4 <- which.min(abs(lon - 100.0))
lat_idx_4 <- which.min(abs(lat - 5.5))

lon_idx_5 <- which.min(abs(lon - 100.25))
lat_idx_5 <- which.min(abs(lat - 5.5))

lon_idx_6 <- which.min(abs(lon - 100.50))
lat_idx_6 <- which.min(abs(lat - 5.5))

lon_idx_7 <- which.min(abs(lon - 100.0))
lat_idx_7 <- which.min(abs(lat - 5.75))

lon_idx_8 <- which.min(abs(lon - 100.25))
lat_idx_8 <- which.min(abs(lat - 5.75))

lon_idx_9 <- which.min(abs(lon - 100.50))
lat_idx_9 <- which.min(abs(lat - 5.75))

# Extract the temperature data for each specified coordinate
temp1 <- temp[lon_idx_1, lat_idx_1, ]
temp2 <- temp[lon_idx_2, lat_idx_2, ]
temp3 <- temp[lon_idx_3, lat_idx_3, ]
temp4 <- temp[lon_idx_4, lat_idx_4, ]
temp5 <- temp[lon_idx_5, lat_idx_5, ]
temp6 <- temp[lon_idx_6, lat_idx_6, ]
temp7 <- temp[lon_idx_7, lat_idx_7, ]
temp8 <- temp[lon_idx_8, lat_idx_8, ]
temp9 <- temp[lon_idx_9, lat_idx_9, ]

# Convert the temperature data from Kelvin to Celsius
temp1_celsius <- temp1 - 273.15
temp2_celsius <- temp2 - 273.15
temp3_celsius <- temp3 - 273.15
temp4_celsius <- temp4 - 273.15
temp5_celsius <- temp5 - 273.15
temp6_celsius <- temp6 - 273.15
temp7_celsius <- temp7 - 273.15
temp8_celsius <- temp8 - 273.15
temp9_celsius <- temp9 - 273.15

# Create a data frame for the time series
temp_df <- data.frame(
  time = rep(time, 9),
  temperature = c(temp1_celsius, temp2_celsius, temp3_celsius, temp4_celsius, temp5_celsius,
                  temp6_celsius, temp7_celsius, temp8_celsius, temp9_celsius),
  location = rep(c("(5.25, 100)", "(5.25, 100.25)", "(5.25, 100.50)", 
                   "(5.5, 100)", "(5.5, 100.25)", "(5.5, 100.50)", 
                   "(5.75, 100)", "(5.75, 100.25)", "(5.75, 100.50)"), each = length(time))
)

# Plot the temperature time series
ggplot(temp_df, aes(x = time, y = temperature, color = location)) +
  geom_line() +
  labs(title = "Temperature Time Series at Specified Coordinates",
       x = "Time",
       y = "Temperature (°C)",
       color = "Location") +
  theme_minimal()

# Plotting the points on the map
map_center <- c(lon = 100.25, lat = 5.5)  # Center map between the points
zoom_level <- 9
basemap <- get_map(location = map_center, zoom = zoom_level, source = "google", maptype = "terrain")

# Define the coordinates for the nine points
points_df <- data.frame(
  lon = c(100.0, 100.25, 100.50, 100.0, 100.25, 100.50, 100.0, 100.25, 100.50),
  lat = c(5.25, 5.25, 5.25, 5.5, 5.5, 5.5, 5.75, 5.75, 5.75),
  location = c("1 (5.25, 100)", "2 (5.25, 100.25)", "3 (5.25, 100.50)", 
               "4 (5.5, 100)", "5 (5.5, 100.25)", "6 (5.5, 100.50)", 
               "7 (5.75, 100)", "8 (5.75, 100.25)", "9 (5.75, 100.50)")
)

# Plot the points on the map
ggmap(basemap) +
  geom_point(data = points_df, aes(x = lon, y = lat, color = location), size = 3) +
  geom_text(data = points_df, aes(x = lon, y = lat, label = location), vjust = -1.5, color = "black") +
  labs(title = "Specified Temperature Points on Map",
       x = "Longitude",
       y = "Latitude") +
  theme_minimal()

