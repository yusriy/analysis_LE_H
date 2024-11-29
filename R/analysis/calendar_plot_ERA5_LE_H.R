library(ggplot2)
library(dplyr)
library(lubridate)
library(patchwork)
library(ncdf4)

# Load the ERA5 NetCDF data
file_path <- "OneDrive/USM/Documents/research/Data_analysis/analysis_LE_H/data/era5/era5_nearCEMACS_v2.nc"
nc_file <- nc_open(file_path)

# Extract dimensions and variables
lat <- ncvar_get(nc_file, "latitude")
lon <- ncvar_get(nc_file, "longitude")
time <- ncvar_get(nc_file, "time")
mslhf <- abs(ncvar_get(nc_file, "mslhf"))  # Latent heat flux
msshf <- abs(ncvar_get(nc_file, "msshf"))  # Sensible heat flux

# Convert time to dates
time_units <- ncatt_get(nc_file, "time", "units")$value
time_origin <- strsplit(time_units, "since ")[[1]][2]
time <- as.POSIXct(time * 3600, origin = time_origin, tz = "UTC")

# Convert to Malaysian Time (UTC+8)
time_myt <- as.POSIXct(format(time, tz = "Asia/Kuala_Lumpur", usetz = TRUE))

# Indices for the nine specific locations with point names
specific_locations <- data.frame(
  lon = c(100.0, 100.25, 100.5, 100.0, 100.25, 100.5, 100.0, 100.25, 100.5),
  lat = c(5.25, 5.25, 5.25, 5.5, 5.5, 5.5, 5.75, 5.75, 5.75),
  point = c(
    "Point 1", "Point 2", "Point 3",
    "Point 4", "Point 5", "Point 6",
    "Point 7", "Point 8", "Point 9"
  )
)

# Invert the order of locations for plotting
specific_locations <- specific_locations %>%
  arrange(desc(lat), lon)

# Extract data for the specific locations
location_data_list <- list()
for (i in seq_len(nrow(specific_locations))) {
  lon_idx <- which.min(abs(lon - specific_locations$lon[i]))
  lat_idx <- which.min(abs(lat - specific_locations$lat[i]))
  
  # Extract the mslhf and msshf time series for the location
  mslhf_series <- mslhf[lon_idx, lat_idx, ]
  msshf_series <- msshf[lon_idx, lat_idx, ]
  
  # Create a data frame for this location
  location_data <- data.frame(
    time = time_myt,
    mslhf = mslhf_series,
    msshf = msshf_series,
    point = specific_locations$point[i]
  )
  
  # Add to the list
  location_data_list[[i]] <- location_data
}

# Combine data for all locations
combined_location_data <- bind_rows(location_data_list)

# Close the NetCDF file
nc_close(nc_file)

# Process data for calendar heatmaps
daily_avg_data <- combined_location_data %>%
  mutate(
    date = as.Date(time),
    month = month(date, label = TRUE),
    day = day(date)
  ) %>%
  group_by(point, month, day) %>%
  summarize(
    mslhf_avg = mean(mslhf, na.rm = TRUE),
    msshf_avg = mean(msshf, na.rm = TRUE),
    .groups = "drop"
  )

# Get separate ranges for LE and H
le_range <- range(daily_avg_data$mslhf_avg, na.rm = TRUE)
h_range <- c(0, 30)  # Fixed scale for H

# Generate heatmaps for LE and H
heatmaps <- list()
for (i in seq_len(nrow(specific_locations))) {
  point_name <- specific_locations$point[i]
  
  # Filter data for the current location
  location_data <- daily_avg_data %>%
    filter(point == point_name)
  
  # Determine if the current plot is in the bottom row
  is_bottom_row <- i > (nrow(specific_locations) - 3)
  
  # Create the LE heatmap
  le_heatmap <- ggplot(location_data, aes(x = day, y = month, fill = mslhf_avg)) +
    geom_tile(color = "white") +
    scale_fill_gradient2(
      name = expression(LE ~ (W ~ m^-2)), 
      low = "blue", mid = "white", high = "red", midpoint = 0,
      limits = le_range
    ) +
    labs(
      title = paste(point_name, "LE"),
      x = NULL,
      y = NULL
    ) +
    theme_minimal() +
    theme(
      panel.grid = element_blank(),
      axis.text.x = if (is_bottom_row) element_text(size = 8) else element_blank()  # Conditional x-axis text
    )
  
  # Create the H heatmap
  h_heatmap <- ggplot(location_data, aes(x = day, y = month, fill = msshf_avg)) +
    geom_tile(color = "white") +
    scale_fill_gradient2(
      name = expression(H ~ (W ~ m^-2)), 
      low = "blue", mid = "white", high = "red", midpoint = 15,  # Adjust midpoint
      limits = h_range
    ) +
    labs(
      title = paste(point_name, "H"),
      x = NULL,
      y = NULL
    ) +
    theme_minimal() +
    theme(
      panel.grid = element_blank(),
      axis.text.x = if (is_bottom_row) element_text(size = 8) else element_blank()  # Conditional x-axis text
    )
  # Combine LE and H heatmaps side-by-side
  heatmaps[[i]] <- le_heatmap + h_heatmap
}

# Combine all heatmaps into a single plot
combined_plot <- wrap_plots(heatmaps, ncol = 3, guides = "collect") &
  theme(
    legend.position = "bottom",
    legend.title = element_text(size = 10),
    legend.text = element_text(size = 8)
  )

# Display the combined plot
print(combined_plot)

# Save the combined plot
ggsave("OneDrive/USM/Documents/research/Data_analysis/analysis_LE_H/LE_H_combined_heatmaps_one_legend.jpeg", 
       plot = combined_plot, width = 20, height = 15, dpi = 300)
