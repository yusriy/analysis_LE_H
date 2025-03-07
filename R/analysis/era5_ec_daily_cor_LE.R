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

# Split combined data by location for scatter plots with linear regression
individual_scatter_plots <- list()

for (i in 1:9) {
  
  # Filter data for the current location
  df_location <- combined_df %>% filter(location == locations[i])
  
  # Calculate correlation coefficient (r) and p-value
  cor_test <- cor.test(df_location$LE.x, df_location$latent_heat_flux_avg, use = "complete.obs")
  r_value <- round(cor_test$estimate, 2)
  p_value <- round(cor_test$p.value, 2)  # Round p-value to 2 decimal places
  
  # Determine if x-axis labels should be shown
  show_x_label <- ifelse(i %in% c(1, 2, 3), TRUE, FALSE)
  
  # Create the scatter plot
  individual_scatter_plots[[i]] <- ggplot(df_location, aes(x = LE.x, y = latent_heat_flux_avg)) +
    geom_point(size = 2, color = "blue") +  # Scatter plot for ERA5 LE vs Eddy LE
    geom_smooth(method = "lm", color = "black", se = FALSE, size = 0.8) +  # Linear regression trend line
    labs(x = if (show_x_label) expression(LE[EC] ~ (W ~ m^-2)) else NULL,
         y = expression(LE[ERA5] ~ (W ~ m^-2))) +  # Axis labels
    theme_minimal() +
    theme(panel.border = element_rect(colour = "black", fill = NA),
          axis.title.x = if (show_x_label) element_text() else element_blank(),
          axis.text.x = if (show_x_label) element_text() else element_blank(),
          plot.title = element_text(size = 12, hjust = 0.5),  # Make the "Point" annotation not bold and centered
          plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm")) +  # Reduce margins around the plot
    ggtitle(locations[i]) +  # Title for the "Point" label above the plot
    annotate("text", x = max(df_location$LE.x, na.rm = TRUE) * 0.8, y = max(df_location$latent_heat_flux_avg, na.rm = TRUE) * 0.9, 
             label = paste("r =", r_value, ", p =", p_value), size = 4, color = "black")  # Add r and p-value inside the plot
}

# Rearrange the plots based on the new order (7, 8, 9; 4, 5, 6; 1, 2, 3)
combined_scatter_panel <- individual_scatter_plots[[7]] + individual_scatter_plots[[8]] + individual_scatter_plots[[9]] +
  individual_scatter_plots[[4]] + individual_scatter_plots[[5]] + individual_scatter_plots[[6]] +
  individual_scatter_plots[[1]] + individual_scatter_plots[[2]] + individual_scatter_plots[[3]] +
  plot_layout(ncol = 3) & theme(plot.margin = unit(c(0.2, 0.2, 0.2, 0.2), "cm"))  # Reduce space between panels

# Save the combined scatter panel with reduced white space as a JPEG
ggsave(filename = paste0(output_folder, "/combined_scatter_panel_lm_r_p_rearranged_reduced_white_space.jpeg"), 
       plot = combined_scatter_panel, width = 14, height = 10, dpi = 300, device = "jpeg")

# Close the NetCDF file
nc_close(nc_file)