library(ncdf4)
library(dplyr)
library(ggplot2)
library(scales)
library(patchwork)  # For combining plots

# Specify the path to your NetCDF file
file_path <- "data/era5/era5_nearCEMACS_v2.nc"

# Create the output folder if it doesn't exist
output_folder <- "fig_paper2"
if (!dir.exists(output_folder)) {
  dir.create(output_folder)
}

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

# Indices for the nine locations
lon_idx_1 <- which.min(abs(lon - 100.0))
lat_idx_1 <- which.min(abs(lat - 5.25))

lon_idx_2 <- which.min(abs(lon - 100.25))
lat_idx_2 <- which.min(abs(lat - 5.25))

lon_idx_3 <- which.min(abs(lon - 100.50))
lat_idx_3 <- which.min(abs(lat - 5.25))

lon_idx_4 <- which.min(abs(lon - 100.0))
lat_idx_4 <- which.min(abs(lat - 5.5))

lon_idx_5 <- which.min(abs(lon - 100.25))
lat_idx_5 <- which.min(abs(lat - 5.5))

lon_idx_6 <- which.min(abs(lon - 100.50))
lat_idx_6 <- which.min(abs(lat - 5.5))

lon_idx_7 <- which.min(abs(lon - 100.0))
lat_idx_7 <- which.min(abs(lat - 5.75))

lon_idx_8 <- which.min(abs(lon - 100.25))
lat_idx_8 <- which.min(abs(lat - 5.75))

lon_idx_9 <- which.min(abs(lon - 100.50))
lat_idx_9 <- which.min(abs(lat - 5.75))

# Extract the latent heat flux time series for the nine locations
mslhf_series_1 <- mslhf[lon_idx_1, lat_idx_1, ]
mslhf_series_2 <- mslhf[lon_idx_2, lat_idx_2, ]
mslhf_series_3 <- mslhf[lon_idx_3, lat_idx_3, ]
mslhf_series_4 <- mslhf[lon_idx_4, lat_idx_4, ]
mslhf_series_5 <- mslhf[lon_idx_5, lat_idx_5, ]
mslhf_series_6 <- mslhf[lon_idx_6, lat_idx_6, ]
mslhf_series_7 <- mslhf[lon_idx_7, lat_idx_7, ]
mslhf_series_8 <- mslhf[lon_idx_8, lat_idx_8, ]
mslhf_series_9 <- mslhf[lon_idx_9, lat_idx_9, ]

# Create data frames for the time series of each location with custom labels
locations <- c("Point 1 (5.25, 100)", "Point 2 (5.25, 100.25)", "Point 3 (5.25, 100.50)", 
               "Point 4 (5.5, 100)", "Point 5 (5.5, 100.25)", "Point 6 (5.5, 100.50)", 
               "Point 7 (5.75, 100)", "Point 8 (5.75, 100.25)", "Point 9 (5.75, 100.50)")

mslhf_df_1 <- data.frame(time = time_myt, latent_heat_flux = mslhf_series_1, location = locations[1])
mslhf_df_2 <- data.frame(time = time_myt, latent_heat_flux = mslhf_series_2, location = locations[2])
mslhf_df_3 <- data.frame(time = time_myt, latent_heat_flux = mslhf_series_3, location = locations[3])
mslhf_df_4 <- data.frame(time = time_myt, latent_heat_flux = mslhf_series_4, location = locations[4])
mslhf_df_5 <- data.frame(time = time_myt, latent_heat_flux = mslhf_series_5, location = locations[5])
mslhf_df_6 <- data.frame(time = time_myt, latent_heat_flux = mslhf_series_6, location = locations[6])
mslhf_df_7 <- data.frame(time = time_myt, latent_heat_flux = mslhf_series_7, location = locations[7])
mslhf_df_8 <- data.frame(time = time_myt, latent_heat_flux = mslhf_series_8, location = locations[8])
mslhf_df_9 <- data.frame(time = time_myt, latent_heat_flux = mslhf_series_9, location = locations[9])

# Combine data frames for all locations
mslhf_df <- bind_rows(mslhf_df_1, mslhf_df_2, mslhf_df_3, mslhf_df_4, mslhf_df_5, 
                      mslhf_df_6, mslhf_df_7, mslhf_df_8, mslhf_df_9)

# Calculate daily averages for all locations
daily_avg_mslhf_df <- mslhf_df %>%
  mutate(date = as.Date(time, tz = "Asia/Kuala_Lumpur")) %>%
  group_by(location, date) %>%
  summarize(latent_heat_flux_avg = mean(latent_heat_flux, na.rm = TRUE))

# Load the eddy covariance data
df_merged_day <- read.csv("data/df_merged_day.csv")

# Convert date columns to Date format for merging
df_merged_day$date <- as.Date(df_merged_day$date)

# Merge eddy covariance data with individual LE for comparison
combined_df <- left_join(daily_avg_mslhf_df, df_merged_day %>% select(date, LE.x), by = "date")

# Split combined data by location for individual plots
individual_combined_plots <- list()

for (i in 1:9) {
  # Create the plot
  individual_combined_plots[[i]] <- ggplot(combined_df %>% filter(location == locations[i]), aes(x = date)) +
    geom_line(aes(y = latent_heat_flux_avg), size = 0.8, color = "blue") +  # ERA5 LE with reduced line width
    geom_smooth(aes(y = latent_heat_flux_avg), method = "loess", color = "blue", se = FALSE, linetype = "solid", size = 0.6) +  # LOESS trend line for ERA5 LE
    geom_line(aes(y = LE.x), color = "black", size = 0.8) +  # Eddy covariance LE with solid line
    geom_smooth(aes(y = LE.x), method = "loess", color = "black", se = FALSE, linetype = "solid", size = 0.6) +  # LOESS trend line for eddy covariance LE
    ylim(0, 200) +  # Set y-axis limits to 200
    scale_x_date(date_breaks = "1 month", date_labels = "%b") +  # Show only the month abbreviation on x-axis
    labs(y = expression(LE ~ (W ~ m^-2))) +  # No legend title
    theme_minimal() +
    theme(axis.title.x = element_blank(),  # Remove "Date" from x-axis
          legend.position = "none",  # Remove legend entirely
          panel.border = element_rect(colour = "black", fill = NA)) +  # Add frame around plot
    annotate("text", x = mean(combined_df$date, na.rm = TRUE), y = 195, label = locations[i], hjust = 0.5, size = 4, color = "black")  # Higher, centered, black annotation
  
  # Save the plot to the output folder as a JPEG
  ggsave(filename = paste0(output_folder, "/plot_", gsub("[^0-9]", "", locations[i]), ".jpeg"),
         plot = individual_combined_plots[[i]], width = 7, height = 5, dpi = 300, device = "jpeg")
}

# Combine the 9 individual plots into a 3-by-3 grid
combined_panel <- individual_combined_plots[[1]] + individual_combined_plots[[2]] + individual_combined_plots[[3]] +
  individual_combined_plots[[4]] + individual_combined_plots[[5]] + individual_combined_plots[[6]] +
  individual_combined_plots[[7]] + individual_combined_plots[[8]] + individual_combined_plots[[9]] +
  plot_layout(ncol = 3)

# Save the combined 9-by-9 panel as a JPEG
ggsave(filename = paste0(output_folder, "/combined_panel.jpeg"), plot = combined_panel, width = 14, height = 10, dpi = 300, device = "jpeg")

# Close the NetCDF file
nc_close(nc_file)