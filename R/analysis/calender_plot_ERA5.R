library(ggplot2)
library(dplyr)
library(lubridate)
library(patchwork) # For combining plots

# Ensure the time column is converted to Date
mslhf_reshaped <- mslhf_reshaped %>%
  mutate(date = as.Date(time))  # Convert to Date format

# Extract month and day, and calculate daily averages for each location
mslhf_daily_avg <- mslhf_reshaped %>%
  mutate(
    month = month(date, label = TRUE),  # Extract month as a factor with labels (Jan, Feb, ...)
    day = day(date)                     # Extract day of the month
  ) %>%
  group_by(lon, lat, month, day) %>%
  summarize(mslhf_avg = mean(mslhf, na.rm = TRUE), .groups = "drop")

# Get unique locations for looping
unique_locations <- mslhf_daily_avg %>%
  distinct(lon, lat) %>%
  mutate(location = paste("Lon:", round(lon, 2), "Lat:", round(lat, 2)))

# Generate calendar heatmaps for each location
heatmaps <- list()
for (i in seq_len(nrow(unique_locations))) {
  loc <- unique_locations[i, ]
  
  # Filter data for the current location
  location_data <- mslhf_daily_avg %>%
    filter(lon == loc$lon, lat == loc$lat)
  
  # Create the heatmap for the current location
  heatmap <- ggplot(location_data, aes(x = day, y = month, fill = mslhf_avg)) +
    geom_tile(color = "white") +  # Add grid lines between tiles
    scale_fill_viridis_c(name = "Daily Avg LE\n(W/m^2)", option = "C") +
    labs(
      title = loc$location,  # Use location as title
      x = "Day of the Month",
      y = "Month"
    ) +
    theme_minimal() +
    theme(
      panel.grid = element_blank(),         # Remove gridlines
      axis.text.x = element_text(size = 8), # Smaller x-axis labels
      axis.text.y = element_text(size = 10),# Adjust y-axis labels
      legend.position = "right"             # Place legend on the right
    )
  
  # Add the heatmap to the list
  heatmaps[[i]] <- heatmap
}

# Combine heatmaps into a single plot
combined_plot <- wrap_plots(heatmaps, ncol = 3)

# Display the combined plot
print(combined_plot)

# Save the combined plot
ggsave("OneDrive/USM/Documents/research/Data_analysis/analysis_LE_H/LE_calendar_heatmaps_by_location.jpeg", plot = combined_plot, width = 15, height = 10, dpi = 300)
