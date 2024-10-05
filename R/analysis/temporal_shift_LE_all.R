library(ggplot2)
library(dplyr)
library(qmap)

# Define the nine locations with file names and corresponding ERA5 latent heat flux columns
locations <- data.frame(
  name = c("5.25_100", "5.25_100.25", "5.25_100.50", 
           "5.50_100", "5.50_100.25", "5.50_100.50", 
           "5.75_100", "5.75_100.25", "5.75_100.50"),
  file = c("data/merged_day_location_5.25_100.csv", "data/merged_day_location_5.25_100.25.csv", 
           "data/merged_day_location_5.25_100.50.csv", "data/merged_day_location_5.50_100.csv", 
           "data/merged_day_location_5.50_100.25.csv", "data/merged_day_location_5.50_100.50.csv", 
           "data/merged_day_location_5.75_100.csv", "data/merged_day_location_5.75_100.25.csv", 
           "data/merged_day_location_5.75_100.50.csv"),
  era5_col = c("LE_era5_location_5.25_100", "LE_era5_location_5.25_100.25", "LE_era5_location_5.25_100.50",
               "LE_era5_location_5.50_100", "LE_era5_location_5.50_100.25", "LE_era5_location_5.50_100.50",
               "LE_era5_location_5.75_100", "LE_era5_location_5.75_100.25", "LE_era5_location_5.75_100.50")
)

# Initialize a list to store performance metrics
metrics_list <- list()

# Loop through all locations
for (i in 1:nrow(locations)) {
  
  # Load the merged data for each location
  merged_data <- read.csv(locations$file[i])
  
  # Ensure the date column is in Date format
  merged_data$date <- as.Date(merged_data$date)
  
  # Separate LE from eddy covariance (LE_ec) and ERA5 (LE_era5)
  LE_ec <- merged_data$LE.x  # Eddy covariance latent heat flux
  LE_era5 <- merged_data[[locations$era5_col[i]]]  # ERA5 latent heat flux for the specific location
  
  # ---- Shift ERA5 Data Based on Cross-Correlation Lag ----
  shift_days <- 13  # Shift of 13 days (adjust as needed based on earlier analysis)
  merged_data$LE_era5_shifted <- dplyr::lag(LE_era5, n = shift_days)
  
  # Remove any rows where either EC or shifted ERA5 values are NA
  merged_data_shifted <- merged_data %>%
    filter(!is.na(LE_era5_shifted) & !is.na(LE.x))
  
  # Ensure that LE_ec and LE_era5_shifted have the same length
  LE_ec <- merged_data_shifted$LE.x
  LE_era5_shifted <- merged_data_shifted$LE_era5_shifted
  
  # ---- Apply Quantile Mapping for Bias Correction ----
  qm_fit <- fitQmap(LE_ec, LE_era5_shifted, method = "RQUANT")
  LE_era5_qmapped <- doQmap(LE_era5_shifted, qm_fit)
  
  # Add the quantile-mapped data to the dataset
  merged_data_shifted$LE_era5_qmapped <- LE_era5_qmapped
  
  # Ensure that the length of LE_ec and LE_era5_qmapped match before calculating metrics
  if (length(LE_ec) == length(LE_era5_qmapped)) {
    
    # ---- Plot the Time Series (Original, Shifted, and Quantile Mapped) ----
    plot_title <- paste("Latent Heat Flux (LE): EC vs ERA5 (Point", locations$name[i], ")")
    ggplot(merged_data_shifted, aes(x = date)) +
      geom_line(aes(y = LE_ec, color = "LE (EC)"), size = 1) +
      geom_line(aes(y = LE_era5_shifted, color = "LE (ERA5 Shifted)"), size = 1, linetype = "dashed") +
      geom_line(aes(y = LE_era5_qmapped, color = "LE (ERA5 Quantile Mapped)"), size = 1, linetype = "dotdash") +
      labs(title = plot_title,
           x = "Date", y = "LE (W m–2)") +
      scale_color_manual(values = c("LE (EC)" = "blue", 
                                    "LE (ERA5 Shifted)" = "red", 
                                    "LE (ERA5 Quantile Mapped)" = "green")) +
      theme_minimal()
    
    # Save the plot to file
    ggsave(paste0("fig_paper2/LE_Comparison_Bias_Corrected_", locations$name[i], ".jpeg"), width = 10, height = 6, dpi = 300)
    
    # ---- Recompute Correlation Metrics ----
    
    # Mean Bias Error (MBE)
    mbe <- mean(LE_era5_qmapped - LE_ec, na.rm = TRUE)
    
    # Root Mean Square Error (RMSE)
    rmse_qmapped <- sqrt(mean((LE_era5_qmapped - LE_ec)^2, na.rm = TRUE))
    
    # Mean Absolute Percentage Error (MAPE)
    mape <- mean(abs((LE_era5_qmapped - LE_ec) / LE_ec) * 100, na.rm = TRUE)
    
    # Pearson Correlation
    correlation_qmapped <- cor(LE_ec, LE_era5_qmapped, use = "complete.obs")
    
    # Store the metrics in a list
    metrics_list[[locations$name[i]]] <- data.frame(
      Location = locations$name[i],
      MBE = round(mbe, 3),
      RMSE = round(rmse_qmapped, 3),
      MAPE = round(mape, 3),
      Pearson_Correlation = round(correlation_qmapped, 3)
    )
    
  } else {
    warning(paste("Lengths of LE_ec and LE_era5_qmapped do not match for location", locations$name[i]))
  }
}

# Combine all metrics into a single data frame
metrics_table <- do.call(rbind, metrics_list)

# Print the final metrics table
print(metrics_table)

# Optionally, save the metrics table to a CSV file
write.csv(metrics_table, "fig_paper2/LE_Performance_Metrics.csv", row.names = FALSE)