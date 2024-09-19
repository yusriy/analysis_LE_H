library(ggplot2)
library(ggspatial)
library(sf)
library(prettymapr)
library(ggmap)

# Load environment variables from the .Renviron file
readRenviron(".Renviron")


# Check if the API key is loaded correctly
if (api_key == "") {
  stop("API key not found. Please ensure it is set in the .Renviron file.")
}


# Register Google Maps API key
register_google(key = api_key)

# Your remaining code...

#### To plot the map ####
# Define the coordinates
latitude <- 5.5
longitude <- 100.25

# Create a data frame with the point of interest
point_of_interest <- data.frame(
  lon = longitude,
  lat = latitude
)

# Convert to an sf object
point_sf <- st_as_sf(point_of_interest, coords = c("lon", "lat"), crs = 4326)

# Create the map
ggplot() +
  geom_sf(data = point_sf, color = "red", size = 3) +
  annotation_map_tile(zoom = 8, type = "osm") +
  coord_sf(xlim = c(longitude - 1, longitude + 1), ylim = c(latitude - 1, latitude + 1)) +
  theme_minimal() +
  labs(
    title = "Map showing the point at 5°30'N, 100°15'E with Grid Lines",
    x = "Longitude",
    y = "Latitude"
  ) +
  annotation_scale(location = "bl") +
  annotation_north_arrow(location = "bl", which_north = "true", 
                         style = north_arrow_fancy_orienteering)



#### Zoomed Out Map ####


library(ggplot2)
library(sf)


# Define the coordinates
latitude <- 5.5
longitude <- 100.25

# Define the resolution of the ERA5 grid
resolution <- 0.25  # Change to 0.5 for 0.5° resolution

# Calculate the grid cell bounds
lat_min <- floor(latitude / resolution) * resolution
lat_max <- lat_min + resolution
lon_min <- floor(longitude / resolution) * resolution
lon_max <- lon_min + resolution

# Create a data frame for the point of interest
point_of_interest <- data.frame(
  lon = longitude,
  lat = latitude
)

# Create a polygon for the grid cell
grid_cell <- st_polygon(list(rbind(
  c(lon_min, lat_min),
  c(lon_max, lat_min),
  c(lon_max, lat_max),
  c(lon_min, lat_max),
  c(lon_min, lat_min)
)))

# Convert to sf objects
point_sf <- st_as_sf(point_of_interest, coords = c("lon", "lat"), crs = 4326)
grid_sf <- st_sfc(grid_cell, crs = 4326)


# Get a basemap from Google Maps
map <- get_googlemap(center = c(lon = longitude, lat = latitude), zoom = 10, maptype = "terrain")

# Create the map
ggmap(map) +
  geom_sf(data = grid_sf, fill = NA, color = "blue", size = 1, inherit.aes = FALSE) +
  geom_sf(data = point_sf, color = "red", size = 3, inherit.aes = FALSE) +
  labs(
    title = paste("ERA5 Grid Cell at Resolution", resolution, "°\n(5°30'N, 100°15'E)"),
    x = "Longitude",
    y = "Latitude"
  )


#### To plot the ERA5 grid surroung the station for spatial averaging ####
library(ggplot2)
library(dplyr)

# Define the coordinates of your point of interest
station_lat <- 5.468018
station_lon <- 100.200252

# Define the resolution of the ERA5 grid
resolution <- 0.25

# Calculate the nearest grid points
lat_min <- floor(station_lat / resolution) * resolution
lat_max <- lat_min + resolution
lon_min <- floor(station_lon / resolution) * resolution
lon_max <- lon_min + resolution

# Create a data frame with the coordinates of the surrounding grid cells
grid_points <- expand.grid(
  lat = seq(lat_min - resolution, lat_max + resolution, by = resolution),
  lon = seq(lon_min - resolution, lon_max + resolution, by = resolution)
)

# Create a data frame for the station
station_point <- data.frame(
  lon = station_lon,
  lat = station_lat
)

# Plot the grid points and the station point on a map
ggplot() +
  geom_point(data = grid_points, aes(x = lon, y = lat), color = "blue") +
  geom_text(data = grid_points, aes(x = lon, y = lat, label = paste0("(", lat, ", ", lon, ")")), 
            vjust = -1, color = "blue") +
  geom_point(data = station_point, aes(x = lon, y = lat), color = "red", size = 3) +
  geom_text(data = station_point, aes(x = lon, y = lat, label = "Station"), 
            vjust = -1, color = "red") +
  coord_fixed(ratio = 1) +
  labs(
    title = "ERA5 Grid Points and Station Location",
    x = "Longitude",
    y = "Latitude"
  ) +
  theme_minimal() +
  theme(panel.grid.major = element_line(color = "grey80"), 
        panel.grid.minor = element_line(color = "grey90"))
