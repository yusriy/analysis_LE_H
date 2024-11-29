library(ggplot2)
library(dplyr)
library(raster)
library(ggnewscale)

# Load the GEBCO bathymetry data
gebco_path <- "/Users/yusriy/Library/CloudStorage/OneDrive-Personal/USM/Documents/research/Data_analysis/analysis_LE_H/data/GEBCO_22_Nov_2024_1a2bc33e797c/gebco_2024_n13.3525_s-1.3353_w90.6638_e105.8857.nc"
gebco_raster <- raster(gebco_path)

# Crop the raster to the relevant bounding box
xmin <- 99.8
xmax <- 100.55
ymin <- 5.2
ymax <- 5.8
gebco_cropped <- crop(gebco_raster, extent(xmin, xmax, ymin, ymax))

# Separate sea depths (<= 0) and terrain heights (> 0)
gebco_depths <- gebco_cropped
gebco_heights <- gebco_cropped

gebco_depths[gebco_depths > 0] <- NA  # Keep only sea depths
gebco_heights[gebco_heights <= 0] <- NA  # Keep only terrain heights

# Convert raster to data frames for ggplot2
depths_df <- as.data.frame(gebco_depths, xy = TRUE)
colnames(depths_df) <- c("Longitude", "Latitude", "Depth")

heights_df <- as.data.frame(gebco_heights, xy = TRUE)
colnames(heights_df) <- c("Longitude", "Latitude", "Height")

# ERA5 points and EC station
era5_points <- data.frame(
  Longitude = c(100.0, 100.25, 100.5, 100.0, 100.25, 100.5, 100.0, 100.25, 100.5),
  Latitude = c(5.25, 5.25, 5.25, 5.5, 5.5, 5.5, 5.75, 5.75, 5.75),
  Label = paste("Point", 1:9)
)

ec_station <- data.frame(
  Longitude = 100.2,
  Latitude = 5.48,
  Label = "MY-MKH"
)

# Generate latitude and longitude gridlines
lat_gridlines <- seq(ymin, ymax, by = 0.1)
lon_gridlines <- seq(xmin, xmax, by = 0.1)

# Plot with terrain heights and sea depths
p1 <- ggplot() +
  # Add bathymetry data with shades of blue
  geom_raster(data = depths_df, aes(x = Longitude, y = Latitude, fill = Depth)) +
  scale_fill_gradientn(
    colors = rev(c("#f7fbff", "#deebf7", "#c6dbef", "#9ecae1", "#6baed6", "#3182bd", "#08519c")),
    name = "Depth (m)"
  ) +
  new_scale_fill() +  # Required for adding another fill scale
  # Add terrain heights with shades of green
  geom_raster(data = heights_df, aes(x = Longitude, y = Latitude, fill = Height)) +
  scale_fill_gradientn(
    colors = rev(c("#f1f8e9", "#dcedc8", "#aed581", "#81c784", "#4caf50")),
    name = "Height (m)",
    na.value = NA
  ) +
  # Add latitude and longitude gridlines
  geom_hline(yintercept = lat_gridlines, color = "gray70", linetype = "dashed", size = 0.3) +
  geom_vline(xintercept = lon_gridlines, color = "gray70", linetype = "dashed", size = 0.3) +
  # Add ERA5 points
  geom_point(data = era5_points, aes(x = Longitude, y = Latitude), color = "navyblue", size = 3) +
  geom_text(data = era5_points, aes(x = Longitude, y = Latitude, label = Label), color = "navyblue", nudge_y = 0.02, size = 4.5) + # Increased font size for points
  # Add EC station
  geom_point(data = ec_station, aes(x = Longitude, y = Latitude), color = "black", shape = 8, size = 4) +
  geom_text(data = ec_station, aes(x = Longitude, y = Latitude, label = Label), nudge_y = 0.02, size = 4, color = "black") +
  # Adjust theme
  theme_minimal() +
  theme(
    legend.position = c(0.05, 0.95),  # Legend at top-left corner
    legend.justification = c(0, 1),  # Align to top-left
    legend.background = element_rect(fill = "white", color = "gray80", size = 0.2),
    legend.key.width = unit(0.5, "cm"),
    legend.key.height = unit(0.5, "cm"),  # Taller legend keys
    legend.text = element_text(size = 10),  # Larger text size for legend
    legend.title = element_text(size = 11)  # Larger title size for legend
  ) +
  coord_fixed() +
  labs(title = NULL, x = NULL, y = NULL)

# Display the plot
print(p1)

# Save the plot
ggsave("/Users/yusriy/Library/CloudStorage/OneDrive-Personal/USM/Documents/research/Data_analysis/analysis_LE_H/fig_paper2/map_with_taller_legends.jpeg", 
       plot = p1, width = 10, height = 8, dpi = 300)