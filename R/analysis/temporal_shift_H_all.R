library(ggplot2)
library(dplyr)
library(qmap)
library(zoo)  # For na.approx()

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

# Initialize a list to store performance metrics
metrics_list <- list()

# Loop through all locations
for (i in 1:nrow(locations)) {
  
  # Load the merged data for each location
  merged_data <- read.csv(locations$file[i])
  
  # Ensure the date column is in Date format
  merged_data$date <- as.Date(merged_data$date)
  
  # Separate sensible heat flux from eddy covariance (H_ec) and ERA5 (H_era5)
  H_ec <- merged_data$H.x  # Eddy covariance sensible heat flux
  H_era5 <- merged_data[[locations$era5_col[i]]]  # ERA5 sensible heat flux for the specific location
  
  # ---- Handle Missing Values in H_ec and H_era5 ----
  # Interpolate missing values in H_ec and H_era5 using linear interpolation
  merged_data$H_ec <- na.approx(H_ec, na.rm = FALSE)  # Interpolating missing values in H_ec
  merged_data$H_era5 <- na.approx(H_era5, na.rm = FALSE)  # Interpolating missing values in H_era5
  
  # Remove rows where any of the interpolated values are still NA
  merged_data <- merged_data %>%
    filter(!is.na(H_ec) & !is.na(H_era5))
  
  # ---- Plot ACF for Temporal Shift Detection ----
  # Compute ACF between H_ec and H_era5 to identify the optimal lag
  ccf_result <- ccf(merged_data$H_ec, merged_data$H_era5, lag.max = 30, plot = FALSE)
  
  # Save the ACF plot as an image file
  jpeg(paste0("fig_paper2/ACF_H_", locations$name[i], ".jpeg"))
  plot(ccf_result, main = paste("Cross-Correlation of H (EC) and H (ERA5) - Point", locations$name[i]))
  dev.off()
  
  # Determine optimal shift_days based on ACF (adjust according to results)
  shift_days <- 1  # Default shift of 13 days; modify based on ACF result
  
  # ---- Shift ERA5 Data Based on Cross-Correlation Lag ----
  merged_data$H_era5_shifted <- dplyr::lag(merged_data$H_era5, n = shift_days)
  
  # Remove rows with NA after the shift
  merged_data_shifted <- merged_data %>%
    filter(!is.na(H_era5_shifted))
  
  # ---- Apply Quantile Mapping for Bias Correction ----
  qm_fit <- fitQmap(merged_data_shifted$H_ec, merged_data_shifted$H_era5_shifted, method = "RQUANT")
  H_era5_qmapped <- doQmap(merged_data_shifted$H_era5_shifted, qm_fit)
  
  # Add the quantile-mapped data to the dataset
  merged_data_shifted$H_era5_qmapped <- H_era5_qmapped
  
  # ---- Recompute Correlation Metrics ----
  correlation_qmapped <- cor(merged_data_shifted$H_ec, merged_data_shifted$H_era5_qmapped, use = "complete.obs")
  rmse_qmapped <- sqrt(mean((merged_data_shifted$H_era5_qmapped - merged_data_shifted$H_ec)^2, na.rm = TRUE))
  
  # Calculate Mean Bias Error (MBE)
  mbe_qmapped <- mean(merged_data_shifted$H_era5_qmapped - merged_data_shifted$H_ec, na.rm = TRUE)
  
  # Calculate Mean Absolute Percentage Error (MAPE)
  mape_qmapped <- mean(abs((merged_data_shifted$H_era5_qmapped - merged_data_shifted$H_ec) / merged_data_shifted$H_ec) * 100, na.rm = TRUE)
  
  # Store the metrics in the list
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

# Save the metrics table to a CSV file
write.csv(metrics_table, "fig_paper2/H_Performance_Metrics_temporal_QM.csv", row.names = FALSE)