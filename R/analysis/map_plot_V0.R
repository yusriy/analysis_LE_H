library(ggplot2)
library(dplyr)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(ggspatial)

# Define coordinates of the nine ERA5 locations
era5_locations <- data.frame(
  name = c("Point 1", "Point 2", "Point 3", 
           "Point 4", "Point 5", "Point 6", 
           "Point 7", "Point 8", "Point 9"),
  latitude = c(5.25, 5.25, 5.25, 5.50, 5.50, 5.50, 5.75, 5.75, 5.75),
  longitude = c(100.0, 100.25, 100.50, 100.0, 100.25, 100.50, 100.0, 100.25, 100.50)
)

# Define the EC station location
ec_station <- data.frame(
  name = "MY-MKH",
  latitude = 5.46,
  longitude = 100.20
)

# Convert to spatial data frames
era5_sf <- st_as_sf(era5_locations, coords = c("longitude", "latitude"), crs = 4326)
ec_station_sf <- st_as_sf(ec_station, coords = c("longitude", "latitude"), crs = 4326)

# Extract the coordinates for annotation
era5_df <- cbind(era5_locations, st_coordinates(era5_sf))
ec_station_df <- cbind(ec_station, st_coordinates(ec_station_sf))

# Get a high-resolution base map of the region
world <- ne_countries(scale = "medium", returnclass = "sf")

# Plot the map
map <- ggplot(data = world) +
  geom_sf(fill = "white", color = "gray50") +  # World map as the base layer
  geom_point(data = era5_df, aes(x = X, y = Y), shape = 16, size = 4, color = "blue") +  # ERA5 points as blue circles
  geom_point(data = ec_station_df, aes(x = X, y = Y), shape = 8, size = 5, color = "red") +  # EC station as a red asterisk
  geom_text(data = era5_df, aes(x = X, y = Y, label = name), color = "black", size = 3, vjust = -1) +  # Annotate ERA5 points
  geom_text(data = ec_station_df, aes(x = X, y = Y, label = name), color = "red", size = 4, vjust = -1) +  # Annotate EC station
  coord_sf(xlim = c(99.9, 100.6), ylim = c(5.2, 5.8), expand = FALSE) +  # Adjust plot limits
  annotation_scale(location = "bl", width_hint = 0.2) +  # Scale bar at bottom left
  theme_minimal() +
  theme(
    panel.grid.major = element_line(color = "gray80", linetype = "dashed"),
    panel.background = element_rect(fill = "aliceblue"),
    plot.title = element_blank(),  # Remove title
    axis.title.x = element_blank(),  # Remove x-axis label
    axis.title.y = element_blank(),  # Remove y-axis label
    axis.text = element_text(size = 10),  # Keep latitude and longitude tick labels visible
    axis.ticks = element_line(color = "black"),  # Add ticks for the axes
    panel.border = element_rect(colour = "black", fill = NA)  # Add frame around plot
  )
map
# Save the map to file
ggsave("fig_paper2/Map_ERA5_Locations_MY_MKH_Annotated.jpeg", map, width = 10, height = 7, dpi = 300)