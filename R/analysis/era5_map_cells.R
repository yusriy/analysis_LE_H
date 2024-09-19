library(ggplot2)
library(ggmap)
library(ncdf4)

# Open the NetCDF file
nc_file <- nc_open("data/era5/era5_nearCEMACS.nc")
print(nc_file)

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

# Find the indices of the nearest longitude and latitude
lon_idx <- which.min(abs(lon - 100.200258))
lat_idx <- which.min(abs(lat - 5.468021))

# Adjust lat_idx to get the top two latitudes
lat_idx_top1 <- lat_idx - 1
lat_idx_top2 <- lat_idx - 2

# Extract the temperature data for the top two points
temp1 <- temp[lon_idx, lat_idx_top1, ]
temp2 <- temp[lon_idx, lat_idx_top2, ]

# Convert the temperature data from Kelvin to Celsius
temp1_celsius <- temp1 - 273.15
temp2_celsius <- temp2 - 273.15

# Create a data frame for the time series
temp_df <- data.frame(
  time = rep(time, 2),
  temperature = c(temp1_celsius, temp2_celsius),
  location = rep(c("Point 1", "Point 2"), each = length(time))
)

ggplot(temp_df, aes(x = time, y = temperature, color = location)) +
  geom_line() +
  labs(title = "Temperature Time Series for Top Two Points",
       x = "Time",
       y = "Temperature (°C)",
       color = "Location") +
  theme_minimal()



