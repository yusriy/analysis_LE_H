library(ncdf4)
library(ggplot2)
library(dplyr)

# Open the NetCDF file
nc_file <- nc_open("data/era5/era5_nearCEMACS.nc")

# Extract the u10 and v10 wind components and the associated dimensions
u10 <- ncvar_get(nc_file, "u10")  # U-component of wind (m/s)
v10 <- ncvar_get(nc_file, "v10")  # V-component of wind (m/s)
lat <- ncvar_get(nc_file, "latitude")
lon <- ncvar_get(nc_file, "longitude")
time <- ncvar_get(nc_file, "time")

# Convert the time dimension to a date format in UTC
time_units <- ncatt_get(nc_file, "time", "units")$value
time_origin <- strsplit(time_units, "since ")[[1]][2]
time <- as.POSIXct(time * 3600, origin = time_origin, tz = "UTC")

# Convert time to Malaysian Time (MYT), which is UTC +8
time_myt <- format(time, tz = "Asia/Kuala_Lumpur", usetz = TRUE)

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

# Extract the u10 and v10 data for each specified coordinate
u10_1 <- u10[lon_idx_1, lat_idx_1, ]
v10_1 <- v10[lon_idx_1, lat_idx_1, ]

u10_2 <- u10[lon_idx_2, lat_idx_2, ]
v10_2 <- v10[lon_idx_2, lat_idx_2, ]

u10_3 <- u10[lon_idx_3, lat_idx_3, ]
v10_3 <- v10[lon_idx_3, lat_idx_3, ]

u10_4 <- u10[lon_idx_4, lat_idx_4, ]
v10_4 <- v10[lon_idx_4, lat_idx_4, ]

# Calculate wind speed for each point
wind_speed1 <- sqrt(u10_1^2 + v10_1^2)
wind_speed2 <- sqrt(u10_2^2 + v10_2^2)
wind_speed3 <- sqrt(u10_3^2 + v10_3^2)
wind_speed4 <- sqrt(u10_4^2 + v10_4^2)

# Create data frames for each point in MYT
wind_df1 <- data.frame(time = as.POSIXct(time_myt, tz = "Asia/Kuala_Lumpur"), wind_speed = wind_speed1, location = "(5.5, 100)")
wind_df2 <- data.frame(time = as.POSIXct(time_myt, tz = "Asia/Kuala_Lumpur"), wind_speed = wind_speed2, location = "(5.5, 100.25)")
wind_df3 <- data.frame(time = as.POSIXct(time_myt, tz = "Asia/Kuala_Lumpur"), wind_speed = wind_speed3, location = "(5.75, 100)")
wind_df4 <- data.frame(time = as.POSIXct(time_myt, tz = "Asia/Kuala_Lumpur"), wind_speed = wind_speed4, location = "(5.75, 100.25)")

# Combine the data frames
wind_all_df <- bind_rows(wind_df1, wind_df2, wind_df3, wind_df4)

# Calculate hourly averages for each location
hourly_avg_wind_df <- wind_all_df %>%
  mutate(hour = format(time, "%H", tz = "Asia/Kuala_Lumpur")) %>%
  group_by(location, hour) %>%
  summarize(wind_speed_avg = mean(wind_speed))

# Convert hour back to time format for plotting
hourly_avg_wind_df$hour <- as.POSIXct(hourly_avg_wind_df$hour, format = "%H", tz = "Asia/Kuala_Lumpur")

# Plot the hourly average wind speed time series for each coordinate in MYT
ggplot(hourly_avg_wind_df, aes(x = hour, y = wind_speed_avg, color = location)) +
  geom_line() +
  labs(title = "Hourly Averaged Wind Speed Over Entire Dataset (m/s) in MYT",
       x = "Hour of the Day (MYT)",
       y = "Wind Speed (m/s)",
       color = "Location") +
  theme_minimal()
