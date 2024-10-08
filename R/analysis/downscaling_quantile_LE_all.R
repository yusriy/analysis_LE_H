library(dplyr)
library(ggplot2)
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
  
  # ---- Quantile Mapping ----
  
  # Fit a quantile mapping model (using parametric method by default)
  qm_fit <- fitQmap(LE_ec, LE_era5, method = "QUANT") #QUANT #SSPLIN #PTF
  
  # Apply the quantile mapping transformation
  LE_era5_corrected <- doQmap(LE_era5, qm_fit)
  
  # Add the corrected ERA5 LE to the dataset
  merged_data$LE_era5_corrected <- LE_era5_corrected
  
  # Plot the original and corrected LE from ERA5 vs. Eddy Covariance LE for comparison
  plot_title <- paste("Latent Heat Flux (LE) Comparison: Eddy Covariance vs. ERA5 (Quantile Mapping, Point", locations$name[i], ")")
  ggplot(merged_data, aes(x = date)) +
    geom_line(aes(y = LE_ec, color = "LE (EC)"), size = 1) +
    geom_line(aes(y = LE_era5, color = "LE (ERA5)"), size = 1, linetype = "dashed") +
    geom_line(aes(y = LE_era5_corrected, color = "LE (ERA5 Corrected)"), size = 1, linetype = "dotdash") +
    labs(title = plot_title,
         x = "Date", y = "LE (W m–2)") +
    scale_color_manual(values = c("LE (EC)" = "blue", "LE (ERA5)" = "red", "LE (ERA5 Corrected)" = "green")) +
    theme_minimal() +
    theme(legend.title = element_blank())
  
  ggsave(paste0("fig_paper2/LE_Comparison_QM_Corrected_", locations$name[i], ".jpeg"), width = 10, height = 6, dpi = 300)
  
  # ---- Performance Metrics Calculation ----
  
  # Calculate Mean Bias Error (MBE)
  mbe <- mean(LE_era5_corrected - LE_ec, na.rm = TRUE)
  
  # Calculate Root Mean Square Error (RMSE)
  rmse <- sqrt(mean((LE_era5_corrected - LE_ec)^2, na.rm = TRUE))
  
  # Calculate Mean Absolute Percentage Error (MAPE)
  mape <- mean(abs((LE_era5_corrected - LE_ec) / LE_ec) * 100, na.rm = TRUE)
  
  # Calculate Pearson Correlation Coefficient (r)
  correlation <- cor(LE_era5_corrected, LE_ec, use = "complete.obs")
  
  # Store the metrics in a list
  metrics_list[[locations$name[i]]] <- data.frame(
    Location = locations$name[i],
    MBE = mbe,
    RMSE = rmse,
    MAPE = mape,
    Pearson_Correlation = correlation
  )
}

# Combine all the metrics into a single data frame
metrics_table <- do.call(rbind, metrics_list)

# Print the final metrics table
print(metrics_table)

# Optionally, save the metrics table to a CSV file
write.csv(metrics_table, "fig_paper2/LE_Performance_Metrics_QM.csv", row.names = FALSE)