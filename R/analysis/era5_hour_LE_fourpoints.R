library(ncdf4)
library(ggplot2)
library(dplyr)

# Open the NetCDF file
nc_file <- nc_open("data/era5/era5_nearCEMACS.nc")

# Extract the latent heat flux (slhf) and the associated dimensions
slhf <- ncvar_get(nc_file, "slhf")  # Extract latent heat flux in J/m^2
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

# Extract the latent heat flux data for each specified coordinate
slhf1 <- slhf[lon_idx_1, lat_idx_1, ]
slhf2 <- slhf[lon_idx_2, lat_idx_2, ]
slhf3 <- slhf[lon_idx_3, lat_idx_3, ]
slhf4 <- slhf[lon_idx_4, lat_idx_4, ]

# Convert J/m^2 to W/m^2 by dividing by 3600 seconds (assuming hourly data) and take absolute values
slhf1_wm2 <- abs(slhf1) / 3600
slhf2_wm2 <- abs(slhf2) / 3600
slhf3_wm2 <- abs(slhf3) / 3600
slhf4_wm2 <- abs(slhf4) / 3600

# Create data frames for each point in MYT
slhf1_df <- data.frame(time = as.POSIXct(time_myt, tz = "Asia/Kuala_Lumpur"), latent_heat_flux = slhf1_wm2, location = "(5.5, 100)")
slhf2_df <- data.frame(time = as.POSIXct(time_myt, tz = "Asia/Kuala_Lumpur"), latent_heat_flux = slhf2_wm2, location = "(5.5, 100.25)")
slhf3_df <- data.frame(time = as.POSIXct(time_myt, tz = "Asia/Kuala_Lumpur"), latent_heat_flux = slhf3_wm2, location = "(5.75, 100)")
slhf4_df <- data.frame(time = as.POSIXct(time_myt, tz = "Asia/Kuala_Lumpur"), latent_heat_flux = slhf4_wm2, location = "(5.75, 100.25)")

# Combine the data frames
slhf_all_df <- bind_rows(slhf1_df, slhf2_df, slhf3_df, slhf4_df)

# Calculate hourly averages for each location
hourly_avg_df <- slhf_all_df %>%
  mutate(hour = format(time, "%H", tz = "Asia/Kuala_Lumpur")) %>%
  group_by(location, hour) %>%
  summarize(latent_heat_flux_avg = mean(latent_heat_flux))

# Convert hour back to time format for plotting
hourly_avg_df$hour <- as.POSIXct(hourly_avg_df$hour, format = "%H", tz = "Asia/Kuala_Lumpur")

# Plot the hourly average latent heat flux time series for each coordinate in MYT
ggplot(hourly_avg_df, aes(x = hour, y = latent_heat_flux_avg, color = location)) +
  geom_line() +
  labs(title = "Hourly Averaged Latent Heat Flux Over Entire Dataset (W/m²) in MYT",
       x = "Hour of the Day (MYT)",
       y = "Latent Heat Flux (W/m²)",
       color = "Location") +
  theme_minimal()
