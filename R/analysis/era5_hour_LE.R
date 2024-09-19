library(ncdf4)
library(dplyr)
library(ggplot2)

# Specify the path to your NetCDF file
file_path <- "data/era5/era5_nearCEMACS_v2.nc"

# Open the NetCDF file
nc_file <- nc_open(file_path)

# Extract dimensions
lat <- ncvar_get(nc_file, "latitude")
lon <- ncvar_get(nc_file, "longitude")
time <- ncvar_get(nc_file, "time")

# Convert the time dimension to a date format in UTC
time_units <- ncatt_get(nc_file, "time", "units")$value
time_origin <- strsplit(time_units, "since ")[[1]][2]
time <- as.POSIXct(time * 3600, origin = time_origin, tz = "UTC")

# Convert time to Malaysian Time (MYT), which is UTC +8
time_myt <- as.POSIXct(format(time, tz = "Asia/Kuala_Lumpur", usetz = TRUE))

# Extract the mslhf variable (raw values, no scaling or offset applied)
mslhf <- ncvar_get(nc_file, "mslhf")

# Handle missing values
mslhf_fill_value <- -32767
mslhf[mslhf == mslhf_fill_value] <- NA

# Take the absolute value of the latent heat flux
mslhf <- abs(mslhf)

# Indices for the four locations
lon_idx_1 <- which.min(abs(lon - 100.0))
lat_idx_1 <- which.min(abs(lat - 5.5))

lon_idx_2 <- which.min(abs(lon - 100.25))
lat_idx_2 <- which.min(abs(lat - 5.5))

lon_idx_3 <- which.min(abs(lon - 100.0))
lat_idx_3 <- which.min(abs(lat - 5.75))

lon_idx_4 <- which.min(abs(lon - 100.25))
lat_idx_4 <- which.min(abs(lat - 5.75))

# Extract the latent heat flux time series for the four locations
mslhf_series_1 <- mslhf[lon_idx_1, lat_idx_1, ]
mslhf_series_2 <- mslhf[lon_idx_2, lat_idx_2, ]
mslhf_series_3 <- mslhf[lon_idx_3, lat_idx_3, ]
mslhf_series_4 <- mslhf[lon_idx_4, lat_idx_4, ]

# Perform spatial averaging for the pairs of locations
mslhf_avg_1_3 <- (mslhf_series_1 + mslhf_series_3) / 2
mslhf_avg_2_4 <- (mslhf_series_2 + mslhf_series_4) / 2

# Create data frames for the spatially averaged time series
mslhf_avg_df_1_3 <- data.frame(time = time_myt, latent_heat_flux = mslhf_avg_1_3, location = "Avg (5.5,100) & (5.75,100)")
mslhf_avg_df_2_4 <- data.frame(time = time_myt, latent_heat_flux = mslhf_avg_2_4, location = "Avg (5.5,100.25) & (5.75,100.25)")

# Combine data frames
mslhf_avg_df <- bind_rows(mslhf_avg_df_1_3, mslhf_avg_df_2_4)

# Calculate hourly averages over the entire dataset for each hour of the day
hourly_avg_df <- mslhf_avg_df %>%
  mutate(hour = format(time, "%H", tz = "Asia/Kuala_Lumpur")) %>%
  group_by(location, hour) %>%
  summarize(latent_heat_flux_avg = mean(latent_heat_flux, na.rm = TRUE))

# Calculate daily averages
daily_avg_df <- mslhf_avg_df %>%
  mutate(date = as.Date(time, tz = "Asia/Kuala_Lumpur")) %>%
  group_by(location, date) %>%
  summarize(latent_heat_flux_avg = mean(latent_heat_flux, na.rm = TRUE))

# Calculate monthly averages
monthly_avg_df <- mslhf_avg_df %>%
  mutate(month = format(time, "%Y-%m", tz = "Asia/Kuala_Lumpur")) %>%
  group_by(location, month) %>%
  summarize(latent_heat_flux_avg = mean(latent_heat_flux, na.rm = TRUE))

# Convert hour back to time format for plotting
hourly_avg_df$hour <- as.POSIXct(hourly_avg_df$hour, format = "%H", tz = "Asia/Kuala_Lumpur")

# Convert month back to a date format for plotting
monthly_avg_df$month <- as.Date(paste0(monthly_avg_df$month, "-01"))

# Plot the hourly average latent heat flux over 24 hours
hourly_plot <- ggplot(hourly_avg_df, aes(x = hour, y = latent_heat_flux_avg, color = location)) +
  geom_line() +
  ylim(0, 300) +  # Set y-axis limits
  labs(title = "Hourly Averaged Latent Heat Flux Over 24 Hours",
       x = "Hour of the Day (MYT)",
       y = "Latent Heat Flux (W/m²)",
       color = "Location") +
  theme_minimal()

# Plot the daily average latent heat flux
daily_plot <- ggplot(daily_avg_df, aes(x = date, y = latent_heat_flux_avg, color = location)) +
  geom_line() +
  ylim(0, 300) +  # Set y-axis limits
  labs(title = "Daily Averaged Latent Heat Flux",
       x = "Date",
       y = "Latent Heat Flux (W/m²)",
       color = "Location") +
  theme_minimal()

# Plot the monthly average latent heat flux
monthly_plot <- ggplot(monthly_avg_df, aes(x = month, y = latent_heat_flux_avg, color = location)) +
  geom_line() +
  ylim(0, 300) +  # Set y-axis limits
  labs(title = "Monthly Averaged Latent Heat Flux",
       x = "Month",
       y = "Latent Heat Flux (W/m²)",
       color = "Location") +
  theme_minimal()

# Display the plots
print(hourly_plot)
print(daily_plot)
print(monthly_plot)

# Close the NetCDF file
nc_close(nc_file)