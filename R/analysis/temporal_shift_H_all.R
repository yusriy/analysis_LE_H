library(ggplot2)
library(dplyr)
library(qmap)

# Define the nine locations with file names and corresponding ERA5 sensible heat flux columns
locations <- data.frame(
  name = c("5.25_100", "5.25_100.25", "5.25_100.50", 
           "5.50_100", "5.50_100.25", "5.50_100.50", 
           "5.75_100", "5.75_100.25", "5.75_100.50"),
  file = c("data/merged_day_location_5.25_100.csv", "data/merged_day_location_5.25_100.25.csv", 
           "data/merged_day_location_5.25_100.50.csv", "data/merged_day_location_5.50_100.csv", 
           "data/merged_day_location_5.50_100.25.csv", "data/merged_day_location_5.50_100.50.csv", 
           "data/merged_day_location_5.75_100.csv", "data/merged_day_location_5.75_100.25.csv", 
           "data/merged_day_location_5.75_100.50.csv"),
  era5_col = c("H_era5_location_5.25_100", "H_era5_location_5.25_100.25", "H_era5_location_5.25_100.50",
               "H_era5_location_5.50_100", "H_era5_location_5.50_100.25", "H_era5_location_5.50_100.50",
               "H_era5_location_5.75_100", "H_era5_location_5.75_100.25", "H_era5_location_5.75_100.50")
)

# Initialize a list to store the metrics for each location
metrics_list <- list()

# Loop through all locations
for (i in 1:nrow(locations)) {
  
  # Load the merged data for each location
  merged_data <- read.csv(locations$file[i])
  
  # Ensure the date column is in Date format
  merged_data$date <- as.Date(merged_data$date)
  
  # Separate H from eddy covariance (H_ec) and ERA5 (H_era5)
  H_ec <- merged_data$H.x  # Eddy covariance sensible heat flux
  H_era5 <- merged_data[[locations$era5_col[i]]]  # ERA5 sensible heat flux for the specific location
  
  # ---- Shift ERA5 Data Based on Cross-Correlation Lag ----
  shift_days <- 1  # Shift of 12 days (adjust based on earlier analysis)
  merged_data$H_era5_shifted <- dplyr::lag(H_era5, n = shift_days)
  
  # Remove any rows where either EC or shifted ERA5 values are NA
  merged_data_shifted <- merged_data %>%
    filter(!is.na(H_era5_shifted) & !is.na(H.x))
  
  # Filter the data to remove the extreme negative and positive values
  merged_data_shifted <- merged_data_shifted %>%
    filter(H.x > 0 & H.x < 25)
  
  # ---- Apply Quantile Mapping for Bias Correction ----
  qm_fit <- fitQmap(merged_data_shifted$H.x, merged_data_shifted$H_era5_shifted, method = "RQUANT")
  H_era5_qmapped <- doQmap(merged_data_shifted$H_era5_shifted, qm_fit)
  
  # Add the quantile-mapped data to the dataset
  merged_data_shifted$H_era5_qmapped <- H_era5_qmapped
  
  # ---- Create Scatter Plot for H (EC) vs H (ERA5 Quantile Mapped) ----
  plot_title <- paste("Scatter Plot: H (EC) vs H (ERA5 Quantile Mapped) - Point", locations$name[i])
  
  scatter_plot <- ggplot(merged_data_shifted, aes(x = H.x, y = H_era5_qmapped)) +
    geom_point(alpha = 0.5, color = "blue") +
    geom_smooth(method = "lm", color = "red", se = FALSE) +
    labs(title = plot_title, x = "H (EC)", y = "H (ERA5 Quantile Mapped)") +
    theme_minimal()
  
  print(scatter_plot)
  
  # Save the scatter plot to a file
  ggsave(paste0("fig_paper2/H_Scatter_Plot_", locations$name[i], ".jpeg"), plot = scatter_plot, width = 10, height = 6, dpi = 300)
  
  # ---- Calculate and Store Performance Metrics ----
  
  # Pearson correlation for quantile-mapped data
  correlation_qmapped <- cor(merged_data_shifted$H.x, merged_data_shifted$H_era5_qmapped, use = "complete.obs")
  
  # Calculate RMSE for quantile-mapped ERA5
  rmse_qmapped <- sqrt(mean((merged_data_shifted$H_era5_qmapped - merged_data_shifted$H.x)^2, na.rm = TRUE))
  
  # Calculate MBE (Mean Bias Error)
  mbe_qmapped <- mean(merged_data_shifted$H_era5_qmapped - merged_data_shifted$H.x, na.rm = TRUE)
  
  # Calculate MAPE (Mean Absolute Percentage Error)
  mape_qmapped <- mean(abs((merged_data_shifted$H_era5_qmapped - merged_data_shifted$H.x) / merged_data_shifted$H.x) * 100, na.rm = TRUE)
  
  # Store metrics in a list
  metrics_list[[locations$name[i]]] <- data.frame(
    Location = locations$name[i],
    Correlation = round(correlation_qmapped, 3),
    RMSE = round(rmse_qmapped, 3),
    MBE = round(mbe_qmapped, 3),
    MAPE = round(mape_qmapped, 3)
  )
}

# Combine all metrics into a single data frame
metrics_table <- do.call(rbind, metrics_list)

# Print the final metrics table
print(metrics_table)

# Optionally, save the metrics table to a CSV file
write.csv(metrics_table, "fig_paper2/H_Performance_Metrics.csv", row.names = FALSE)