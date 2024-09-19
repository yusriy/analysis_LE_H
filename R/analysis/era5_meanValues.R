library(ncdf4)
library(dplyr)
library(ggplot2)

# Open the NetCDF file
nc_file <- nc_open("path_to_your_file.nc")

# Extract dimensions
lat <- ncvar_get(nc_file, "latitude")
lon <- ncvar_get(nc_file, "longitude")
time <- ncvar_get(nc_file, "time")

# Convert the time dimension to a date format in UTC
time_units <- ncatt_get(nc_file, "time", "units")$value
time_origin <- strsplit(time_units, "since ")[[1]][2]
time <- as.POSIXct(time * 3600, origin = time_origin, tz = "UTC")

# Example: Processing the 2-meter temperature (t2m)
# Extract the t2m variable
t2m_raw <- ncvar_get(nc_file, "t2m")

# Get scale factor and offset
t2m_scale_factor <- 0.000115822714677147
t2m_add_offset <- 301.568423533955

# Apply scale factor and offset
t2m <- t2m_raw * t2m_scale_factor + t2m_add_offset

# Handle missing values
t2m_fill_value <- -32767
t2m[t2m_raw == t2m_fill_value] <- NA

# Convert temperature from Kelvin to Celsius
t2m_celsius <- t2m - 273.15

# Example: Processing the U wind component at 10m (u10)
u10_raw <- ncvar_get(nc_file, "u10")
u10_scale_factor <- 0.00018873631362085
u10_add_offset <- 0.900121257606129
u10 <- u10_raw * u10_scale_factor + u10_add_offset
u10_fill_value <- -32767
u10[u10_raw == u10_fill_value] <- NA

# Example: Processing the Mean surface latent heat flux (mslhf)
mslhf_raw <- ncvar_get(nc_file, "mslhf")
mslhf_scale_factor <- 0.00519679899612838
mslhf_add_offset <- -194.353764170982
mslhf <- mslhf_raw * mslhf_scale_factor + mslhf_add_offset
mslhf_fill_value <- -32767
mslhf[mslhf_raw == mslhf_fill_value] <- NA

# You can similarly process other variables based on their respective scale factors and offsets.

# Example of plotting the processed temperature data (using only the first grid point as an example)
lon_idx <- 1  # Longitude index
lat_idx <- 1  # Latitude index

t2m_series <- t2m_celsius[lon_idx, lat_idx, ]

# Create a data frame for plotting
temp_df <- data.frame(
  time = time,
  temperature = t2m_series
)

# Plot the temperature time series
ggplot(temp_df, aes(x = time, y = temperature)) +
  geom_line(color = "blue") +
  labs(title = "2m Temperature Time Series",
       x = "Time",
       y = "Temperature (°C)") +
  theme_minimal()

# Close the NetCDF file
nc_close(nc_file)