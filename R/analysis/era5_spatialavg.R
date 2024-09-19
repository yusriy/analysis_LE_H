library(ncdf4)
library(ggplot2)

# Open the NetCDF file
nc_file <- nc_open("data/era5/era5_nearCEMACS.nc")

# Extract the temperature data (t2m) and the associated dimensions
temp <- ncvar_get(nc_file, "t2m")  # Extract temperature in raw units (needs scaling)
lat <- ncvar_get(nc_file, "latitude")
lon <- ncvar_get(nc_file, "longitude")
time <- ncvar_get(nc_file, "time")

# Get the scaling factor and offset for t2m from the file's attributes
scale_factor <- ncatt_get(nc_file, "t2m", "scale_factor")$value
add_offset <- ncatt_get(nc_file, "t2m", "add_offset")$value

# Apply the scaling factor and offset to convert to actual temperature in Kelvin
temp <- temp * scale_factor + add_offset

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

# Conduct spatial averaging
temp_avg <- (temp1_celsius + temp2_celsius + temp3_celsius + temp4_celsius) / 4

# Create a data frame for the averaged time series
temp_avg_df <- data.frame(
  time = time,
  temperature = temp_avg
)

# Plot the time series
ggplot(temp_avg_df, aes(x = time, y = temperature)) +
  geom_line(color = "blue") +
  labs(title = "Spatially Averaged Temperature Time Series",
       x = "Time",
       y = "Temperature (°C)") +
  theme_minimal()
