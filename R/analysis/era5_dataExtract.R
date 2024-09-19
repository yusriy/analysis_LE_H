library(ncdf4)
library(dplyr)

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

# Get a list of all variable names
variables <- names(nc_file$var)

# Indices for the four locations
lon_idx_1 <- which.min(abs(lon - 100.0))
lat_idx_1 <- which.min(abs(lat - 5.5))

lon_idx_2 <- which.min(abs(lon - 100.25))
lat_idx_2 <- which.min(abs(lat - 5.5))

lon_idx_3 <- which.min(abs(lon - 100.0))
lat_idx_3 <- which.min(abs(lat - 5.75))

lon_idx_4 <- which.min(abs(lon - 100.25))
lat_idx_4 <- which.min(abs(lat - 5.75))

# Create empty data frames for each location
df_1 <- data.frame(time = time_myt)
df_2 <- data.frame(time = time_myt)
df_3 <- data.frame(time = time_myt)
df_4 <- data.frame(time = time_myt)

# Loop through each variable and extract data for the four locations
for (var in variables) {
  var_data <- ncvar_get(nc_file, var)
  
  # Handle missing values
  var_fill_value <- ncatt_get(nc_file, var, "_FillValue")$value
  var_data[var_data == var_fill_value] <- NA
  
  # Add data for each location to the corresponding data frame
  df_1[[var]] <- var_data[lon_idx_1, lat_idx_1, ]
  df_2[[var]] <- var_data[lon_idx_2, lat_idx_2, ]
  df_3[[var]] <- var_data[lon_idx_3, lat_idx_3, ]
  df_4[[var]] <- var_data[lon_idx_4, lat_idx_4, ]
}

# Calculate wind speed for each location
calculate_wind_speed <- function(u10, v10) {
  sqrt(u10^2 + v10^2)
}

df_1$wind_speed <- calculate_wind_speed(df_1$u10, df_1$v10)
df_2$wind_speed <- calculate_wind_speed(df_2$u10, df_2$v10)
df_3$wind_speed <- calculate_wind_speed(df_3$u10, df_3$v10)
df_4$wind_speed <- calculate_wind_speed(df_4$u10, df_4$v10)

# Absolute the mslhf and msshf values and assign them to LE_era5 and H_era5
df_1$LE_era5 <- abs(df_1$mslhf)
df_1$H_era5 <- abs(df_1$msshf)

df_2$LE_era5 <- abs(df_2$mslhf)
df_2$H_era5 <- abs(df_2$msshf)

df_3$LE_era5 <- abs(df_3$mslhf)
df_3$H_era5 <- abs(df_3$msshf)

df_4$LE_era5 <- abs(df_4$mslhf)
df_4$H_era5 <- abs(df_4$msshf)

# Save each data frame to a separate CSV file in the data/ subfolder
write.csv(df_1, "data/location_5.5_100.csv", row.names = FALSE)
write.csv(df_2, "data/location_5.5_100.25.csv", row.names = FALSE)
write.csv(df_3, "data/location_5.75_100.csv", row.names = FALSE)
write.csv(df_4, "data/location_5.75_100.25.csv", row.names = FALSE)

# Close the NetCDF file
nc_close(nc_file)