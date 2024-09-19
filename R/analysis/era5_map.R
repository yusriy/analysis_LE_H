library(ggplot2)
library(ggmap)
library(ncdf4)

# Open the NetCDF file
nc_file <- nc_open("data/era5/era5_nearCEMACS.nc")

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

# Find the indices of the nearest longitude and latitude for the specified coordinates
lon_idx_1 <- which.min(abs(lon - 100.0))
lat_idx_1 <- which.min(abs(lat - 5.5))

lon_idx_2 <- which.min(abs(lon - 100.25))
lat_idx_2 <- which.min(abs(lat - 5.5))

lon_idx_3 <- which.min(abs(lon - 100.0))
lat_idx_3 <- which.min(abs(lat - 5.75))

lon_idx_4 <- which.min(abs(lon - 100.25))
lat_idx_4 <- which.min(abs(lat - 5.75))

# Extract the temperature data for each specified coordinate
temp1 <- temp[lon_idx_1, lat_idx_1, ]
temp2 <- temp[lon_idx_2, lat_idx_2, ]
temp3 <- temp[lon_idx_3, lat_idx_3, ]
temp4 <- temp[lon_idx_4, lat_idx_4, ]

# Convert the temperature data from Kelvin to Celsius
temp1_celsius <- temp1 - 273.15
temp2_celsius <- temp2 - 273.15
temp3_celsius <- temp3 - 273.15
temp4_celsius <- temp4 - 273.15

# Create a data frame for the time series
temp_df <- data.frame(
  time = rep(time, 4),
  temperature = c(temp1_celsius, temp2_celsius, temp3_celsius, temp4_celsius),
  location = rep(c("(5.5, 100)", "(5.5, 100.25)", "(5.75, 100)", "(5.75, 100.25)"), each = length(time))
)

ggplot(temp_df, aes(x = time, y = temperature, color = location)) +
  geom_line() +
  labs(title = "Temperature Time Series at Specified Coordinates",
       x = "Time",
       y = "Temperature (°C)",
       color = "Location") +
  theme_minimal()

map_center <- c(lon = 100.125, lat = 5.625)  # Center map between the points
zoom_level <- 9
basemap <- get_map(location = map_center, zoom = zoom_level, source = "google", maptype = "terrain")

# Define the coordinates for the four points
points_df <- data.frame(
  lon = c(100.0, 100.25, 100.0, 100.25),
  lat = c(5.5, 5.5, 5.75, 5.75),
  location = c("(5.5, 100)", "(5.5, 100.25)", "(5.75, 100)", "(5.75, 100.25)")
)

# Plot the points on the map
ggmap(basemap) +
  geom_point(data = points_df, aes(x = lon, y = lat, color = location), size = 3) +
  geom_text(data = points_df, aes(x = lon, y = lat, label = location), vjust = -1.5, color = "black") +
  labs(title = "Specified Temperature Points on Map",
       x = "Longitude",
       y = "Latitude") +
  theme_minimal()


