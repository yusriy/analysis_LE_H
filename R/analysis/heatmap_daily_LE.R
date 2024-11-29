library(ncdf4)
library(ggplot2)
library(dplyr)
library(rnaturalearth)
library(rnaturalearthdata)
library(viridis)
library(lubridate)

# Load the ERA5 NetCDF data
file_path <- "OneDrive/USM/Documents/research/Data_analysis/analysis_LE_H/data/era5/era5_nearCEMACS_v2.nc"

nc_file <- nc_open(file_path)

# Extract dimensions and variables
lat <- ncvar_get(nc_file, "latitude")
lon <- ncvar_get(nc_file, "longitude")
time <- ncvar_get(nc_file, "time")  # Time in hours since origin
mslhf <- ncvar_get(nc_file, "mslhf")  # Latent heat flux
mslhf <- abs(mslhf)

# Convert time to dates
time_units <- ncatt_get(nc_file, "time", "units")$value
time_origin <- strsplit(time_units, "since ")[[1]][2]
time <- as.POSIXct(time * 3600, origin = time_origin, tz = "UTC")

# Convert to Malaysian Time (UTC+8)
time_myt <- as.POSIXct(format(time, tz = "Asia/Kuala_Lumpur", usetz = TRUE))

# Close the NetCDF file
nc_close(nc_file)

# Reshape data into a data frame
lon_rep <- rep(lon, each = length(lat))
lat_rep <- rep(lat, length(lon))
mslhf_reshaped <- data.frame(
  lon = lon_rep,
  lat = lat_rep,
  time = rep(time_myt, each = length(lon) * length(lat)),
  mslhf = as.vector(mslhf)
)

# Calculate daily averages for the entire dataset
mslhf_daily_avg <- mslhf_reshaped %>%
  mutate(date = as.Date(time)) %>%
  group_by(lon, lat, date) %>%
  summarize(mslhf_avg = mean(mslhf, na.rm = TRUE), .groups = "drop")

# Load map data
world <- ne_countries(scale = "large", returnclass = "sf")

# Create output folder for saving plots
output_folder <- "OneDrive/USM/Documents/research/Data_analysis/analysis_LE_H/fig_paper2/daily_maps"
if (!dir.exists(output_folder)) {
  dir.create(output_folder)
}

# Calculate the overall min and max of mslhf_avg for consistent color scaling
mslhf_range <- range(mslhf_daily_avg$mslhf_avg, na.rm = TRUE)

# Generate daily plots for the entire dataset
unique_dates <- unique(mslhf_daily_avg$date)

for (current_date in unique_dates) {
  # Filter data for the current date
  mslhf_current <- mslhf_daily_avg %>%
    filter(date == current_date)
  
  # Create the plot
  p <- ggplot() +
    # Add the map
    geom_sf(data = world, fill = "grey80", color = "white") +
    # Add the mslhf heatmap
    geom_tile(data = mslhf_current, aes(x = lon, y = lat, fill = mslhf_avg)) +
    scale_fill_viridis_c(
      name = "Daily Avg LE (W/m^2)", 
      option = "C", 
      limits = mslhf_range  # Use the consistent range
    ) +
    # Fix aspect ratio
    coord_sf(xlim = range(lon), ylim = range(lat), expand = FALSE) +
    # Add labels and theme
    labs(
      title = paste("Daily Avg Latent Heat Flux (LE)", as.character(current_date)),
      x = "Longitude",
      y = "Latitude"
    ) +
    theme_minimal() +
    theme(
      panel.grid.major = element_line(color = "gray90"),
      legend.position = "right"
    )
  
  # Save the plot
  ggsave(
    filename = paste0(output_folder, "/mslhf_daily_", as.character(current_date), ".jpeg"),
    plot = p,
    width = 10,
    height = 8,
    dpi = 300
  )
}
