library(ggplot2)
library(dplyr)
library(qmap)

# Load the merged data for the location (5.75, 100)
merged_data <- read.csv("data/merged_day_location_5.75_100.csv")

# Ensure the date column is in Date format
merged_data$date <- as.Date(merged_data$date)

# Separate latent heat flux from eddy covariance (LE_ec) and ERA5 (LE_era5)
LE_ec <- merged_data$LE.x   # Eddy covariance latent heat flux
LE_era5 <- merged_data$LE_era5_location_5.75_100  # ERA5 latent heat flux

# ---- Handle Missing Values ----
# Remove any rows where either EC or ERA5 values are NA
complete_data <- merged_data %>%
  filter(!is.na(LE.x) & !is.na(LE_era5_location_5.75_100))

# ---- Plot ACF for Temporal Shift Detection ----
# Compute ACF between LE_ec and LE_era5 to identify the optimal lag
acf_result <- ccf(complete_data$LE.x, complete_data$LE_era5_location_5.75_100, lag.max = 30, 
                  plot = TRUE, main = "Cross-Correlation of LE (EC) and LE (ERA5)")

# Based on the ACF result, you can determine the shift_days value
# For this example, we'll use a shift of 13 days (adjust after analyzing ACF)
shift_days <- 13

# ---- Shift ERA5 Data Based on Cross-Correlation Lag ----
# Shift ERA5 time series by the determined lag
merged_data$LE_era5_shifted <- dplyr::lag(merged_data$LE_era5_location_5.75_100, n = shift_days)

# Remove any rows where either EC or shifted ERA5 values are NA
merged_data_shifted <- merged_data %>%
  filter(!is.na(LE_era5_shifted) & !is.na(LE.x))

# ---- Fit Quantile Mapping Model ----
# Use "QUANT" method for quantile mapping
qm_fit <- fitQmap(merged_data_shifted$LE.x, merged_data_shifted$LE_era5_shifted, method = "RQUANT")

# Apply the quantile mapping transformation to ERA5 shifted data
LE_era5_corrected <- doQmap(merged_data_shifted$LE_era5_shifted, qm_fit)

# Add the quantile-mapped data to the dataset
merged_data_shifted$LE_era5_qmapped <- LE_era5_corrected

# ---- Plot the Time Series (Original, Shifted, and Quantile Mapped) ----
ggplot(merged_data_shifted, aes(x = date)) +
  geom_line(aes(y = LE.x, color = "LE (EC)"), size = 1) +
  geom_line(aes(y = LE_era5_shifted, color = "LE (ERA5 Shifted)"), size = 1, linetype = "dashed") +
  geom_line(aes(y = LE_era5_qmapped, color = "LE (ERA5 Quantile Mapped)"), size = 1, linetype = "dotdash") +
  labs(title = "Latent Heat Flux (LE): Eddy Covariance vs ERA5 (Shifted and Quantile Mapped)",
       x = "Date", y = "LE (W m–2)") +
  scale_color_manual(values = c("LE (EC)" = "blue", 
                                "LE (ERA5 Shifted)" = "red", 
                                "LE (ERA5 Quantile Mapped)" = "green")) +
  theme_minimal()

# ---- Recompute Correlation Metrics After Quantile Mapping ----
# Pearson correlation for quantile-mapped data
correlation_qmapped <- cor(merged_data_shifted$LE.x, merged_data_shifted$LE_era5_qmapped, use = "complete.obs")

# Print the correlation after quantile mapping
print(paste("Pearson Correlation (Quantile Mapped ERA5):", round(correlation_qmapped, 3)))

# Calculate RMSE for quantile-mapped ERA5
rmse_qmapped <- sqrt(mean((merged_data_shifted$LE_era5_qmapped - merged_data_shifted$LE.x)^2, na.rm = TRUE))
print(paste("RMSE (Quantile Mapped ERA5):", round(rmse_qmapped, 3)))

