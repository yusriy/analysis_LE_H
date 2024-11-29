library(ncdf4)
library(dplyr)
library(ggplot2)
library(scales)
library(patchwork)  # For combining plots

# Specify the path to your NetCDF file
file_path <- "/Users/yusriy/OneDrive/USM/Documents/research/Data_analysis/analysis_LE_H/data/era5/era5_nearCEMACS_v2.nc"


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

# Extract the sst variable (sst in Kelvin)
sst <- ncvar_get(nc_file, "sst")

# Handle missing values
sst_fill_value <- -32767
sst[sst == sst_fill_value] <- NA

# Convert air temperature from Kelvin to Celsius
sst <- sst - 273.15

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

# Extract the air temperature time series for the nine locations
sst_series_1 <- sst[lon_idx_1, lat_idx_1, ]
sst_series_2 <- sst[lon_idx_2, lat_idx_2, ]
sst_series_3 <- sst[lon_idx_3, lat_idx_3, ]
sst_series_4 <- sst[lon_idx_4, lat_idx_4, ]
sst_series_5 <- sst[lon_idx_5, lat_idx_5, ]
sst_series_6 <- sst[lon_idx_6, lat_idx_6, ]
sst_series_7 <- sst[lon_idx_7, lat_idx_7, ]
sst_series_8 <- sst[lon_idx_8, lat_idx_8, ]
sst_series_9 <- sst[lon_idx_9, lat_idx_9, ]

# Create data frames for the time series of each location with custom labels
locations <- c("Point 1 (5.25, 100)", "Point 2 (5.25, 100.25)", "Point 3 (5.25, 100.50)", 
               "Point 4 (5.5, 100)", "Point 5 (5.5, 100.25)", "Point 6 (5.5, 100.50)", 
               "Point 7 (5.75, 100)", "Point 8 (5.75, 100.25)", "Point 9 (5.75, 100.50)")

sst_df_1 <- data.frame(time = time_myt, sst = sst_series_1, location = locations[1])
sst_df_2 <- data.frame(time = time_myt, sst = sst_series_2, location = locations[2])
sst_df_3 <- data.frame(time = time_myt, sst = sst_series_3, location = locations[3])
sst_df_4 <- data.frame(time = time_myt, sst = sst_series_4, location = locations[4])
sst_df_5 <- data.frame(time = time_myt, sst = sst_series_5, location = locations[5])
sst_df_6 <- data.frame(time = time_myt, sst = sst_series_6, location = locations[6])
sst_df_7 <- data.frame(time = time_myt, sst = sst_series_7, location = locations[7])
sst_df_8 <- data.frame(time = time_myt, sst = sst_series_8, location = locations[8])
sst_df_9 <- data.frame(time = time_myt, sst = sst_series_9, location = locations[9])

# Combine data frames for all locations
sst_df <- bind_rows(sst_df_1, sst_df_2, sst_df_3, sst_df_4, sst_df_5, 
                    sst_df_6, sst_df_7, sst_df_8, sst_df_9)

# Calculate daily averages for all locations
daily_avg_sst_df <- sst_df %>%
  mutate(date = as.Date(time, tz = "Asia/Kuala_Lumpur")) %>%
  group_by(location, date) %>%
  summarize(sst_avg = mean(sst, na.rm = TRUE))

# Load the eddy covariance data
df_merged_day <- read.csv("/Users/yusriy/OneDrive/USM/Documents/research/Data_analysis/analysis_LE_H/data/df_merged_day.csv")

# Convert date columns to Date format for merging
df_merged_day$date <- as.Date(df_merged_day$date)

# Merge eddy covariance data (TS_1_1_1) with ERA5 air temperature for comparison
combined_df <- left_join(daily_avg_sst_df, df_merged_day %>% select(date, TS_1_1_1), by = "date")

# Split combined data by location for scatter plots with linear regression
individual_scatter_plots <- list()

# Split combined data by location for scatter plots with linear regression
individual_scatter_plots <- list()

# Split combined data by location for scatter plots with linear regression
individual_scatter_plots <- list()

for (i in 1:9) {
  
  # Filter data for the current location
  df_location <- combined_df %>% filter(location == locations[i])
  
  # Calculate correlation coefficient (r) and p-value
  cor_test <- cor.test(df_location$TS_1_1_1, df_location$sst_avg, use = "complete.obs")
  r_value <- round(cor_test$estimate, 2)
  p_value <- round(cor_test$p.value, 2)  # Round p-value to 2 decimal places
  
  # Create the scatter plot
  individual_scatter_plots[[i]] <- ggplot(df_location, aes(x = TS_1_1_1, y = sst_avg)) +
    geom_point(size = 2, color = "blue") +  # Scatter plot for ERA5 air temperature vs Eddy air temperature
    geom_smooth(method = "lm", color = "black", se = FALSE, size = 0.8) +  # Linear regression trend line
    coord_cartesian(xlim = c(26, 31), ylim = c(28.5, 31)) +  # Adjusted y-axis limit
    labs(
      x = expression(SST[EC] ~ (degree * C)),  # Updated x-axis label
      y = expression(SST[ERA5] ~ (degree * C))  # Updated y-axis label
    ) +
    theme_minimal() +
    theme(
      panel.border = element_rect(colour = "black", fill = NA),
      axis.title = element_text(size = 10),
      axis.text = element_text(size = 8),
      plot.title = element_text(size = 12, hjust = 0.5)  # Title centered above the plot
    ) +
    ggtitle(locations[i]) +  # Title for each plot
    annotate(
      "text", 
      x = 26.5, y = 30.5,  # Adjusted position for annotation
      label = paste("r =", r_value, "\np =", p_value), 
      size = 4, color = "black", hjust = 0
    )
}

# Rearrange the plots based on the new order (7, 8, 9; 4, 5, 6; 1, 2, 3)
combined_scatter_panel <- individual_scatter_plots[[7]] + individual_scatter_plots[[8]] + individual_scatter_plots[[9]] +
  individual_scatter_plots[[4]] + individual_scatter_plots[[5]] + individual_scatter_plots[[6]] +
  individual_scatter_plots[[1]] + individual_scatter_plots[[2]] + individual_scatter_plots[[3]] +
  plot_layout(ncol = 3) & theme(plot.margin = unit(c(0.2, 0.2, 0.2, 0.2), "cm"))  # Reduce space between panels

# Save the combined scatter panel with reduced white space as a JPEG
ggsave(filename = "/Users/yusriy/OneDrive/USM/Documents/research/Data_analysis/analysis_LE_H/fig_paper2/combined_scatter_panel_sst_lm_r_p_rearranged.jpeg", 
       plot = combined_scatter_panel, width = 14, height = 10, dpi = 300, device = "jpeg")

# Close the NetCDF file
nc_close(nc_file)