library(dplyr)
library(ggplot2)

# Load the merged data for the (5.5, 100) location
merged_data <- read.csv("data/merged_day_location_5.50_100.csv")

# Ensure the date column is in Date format
merged_data$date <- as.Date(merged_data$date)

# Separate LE from eddy covariance (LE_ec) and ERA5 (LE_era5)
LE_ec <- merged_data$LE.x  # Eddy covariance latent heat flux
LE_era5 <- merged_data$LE_era5_location_5.50_100  # ERA5 latent heat flux for the specific location

# Calculate the bias (mean difference between LE_ec and LE_era5)
bias <- mean(LE_era5, na.rm = TRUE) - mean(LE_ec, na.rm = TRUE)

# Apply linear scaling to correct LE_era5 by subtracting the bias
LE_era5_corrected <- LE_era5 - bias

# Add the corrected ERA5 LE to the dataset
merged_data$LE_era5_corrected <- LE_era5_corrected

# Plot the original and corrected LE from ERA5 vs. Eddy Covariance LE for comparison
ggplot(merged_data, aes(x = date)) +
  geom_line(aes(y = LE_ec, color = "LE (EC)"), size = 1) +
  geom_line(aes(y = LE_era5, color = "LE (ERA5)"), size = 1, linetype = "dashed") +
  geom_line(aes(y = LE_era5_corrected, color = "LE (ERA5 Corrected)"), size = 1, linetype = "dotdash") +
  labs(title = "Latent Heat Flux (LE) Comparison: Eddy Covariance vs. ERA5 (Point 5.5, 100)",
       x = "Date", y = "LE (W m–2)") +
  scale_color_manual(values = c("LE (EC)" = "blue", "LE (ERA5)" = "red", "LE (ERA5 Corrected)" = "green")) +
  theme_minimal() +
  theme(legend.title = element_blank())

# Save the plot to the fig_paper2 folder
ggsave("fig_paper2/LE_Comparison_Bias_Corrected_5.5_100.jpeg", width = 10, height = 6, dpi = 300)

# ---- Performance Metrics Calculation ----

# Calculate Mean Bias Error (MBE)
mbe <- mean(LE_era5_corrected - LE_ec, na.rm = TRUE)

# Calculate Root Mean Square Error (RMSE)
rmse <- sqrt(mean((LE_era5_corrected - LE_ec)^2, na.rm = TRUE))

# Calculate Mean Absolute Percentage Error (MAPE)
mape <- mean(abs((LE_era5_corrected - LE_ec) / LE_ec) * 100, na.rm = TRUE)

# Calculate Pearson Correlation Coefficient (r)
correlation <- cor(LE_era5_corrected, LE_ec, use = "complete.obs")

# Print the performance metrics
cat("Performance Metrics for Location (5.5, 100):\n")
cat("Mean Bias Error (MBE):", mbe, "\n")
cat("Root Mean Square Error (RMSE):", rmse, "\n")
cat("Mean Absolute Percentage Error (MAPE):", mape, "%\n")
cat("Pearson Correlation Coefficient (r):", correlation, "\n")