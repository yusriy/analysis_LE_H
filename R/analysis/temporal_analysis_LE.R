library(ggplot2)
library(dplyr)
library(lubridate)
library(zoo)

# Load the merged data for the ERA5 and eddy covariance for the location (5.75, 100)
merged_data <- read.csv("data/merged_day_location_5.75_100.csv")

# Ensure the date column is in Date format
merged_data$date <- as.Date(merged_data$date)

# Separate latent heat flux from eddy covariance (LE_ec) and ERA5 (LE_era5)
LE_ec <- merged_data$LE.x   # Eddy covariance latent heat flux
LE_era5 <- merged_data$LE_era5_location_5.75_100  # ERA5 latent heat flux

# ---- Handle Missing Values ----
# Option 1: Remove rows where either LE_ec or LE_era5 has missing values
complete_data <- merged_data %>%
  filter(!is.na(LE.x) & !is.na(LE_era5_location_5.75_100))

# Option 2: Interpolate missing values in LE_ec and LE_era5 (if preferred)
merged_data$LE_ec <- na.approx(merged_data$LE.x, rule = 2)
merged_data$LE_era5 <- na.approx(merged_data$LE_era5_location_5.75_100, rule = 2)

# ---- 1. Plot the Time Series ----
ggplot(merged_data, aes(x = date)) +
  geom_line(aes(y = LE_ec, color = "LE (EC)"), size = 1) +
  geom_line(aes(y = LE_era5, color = "LE (ERA5)"), size = 1, linetype = "dashed") +
  labs(title = "Latent Heat Flux (LE) Time Series: Eddy Covariance vs ERA5 (5.75, 100)",
       x = "Date", y = "LE (W m–2)") +
  scale_color_manual(values = c("LE (EC)" = "blue", "LE (ERA5)" = "red")) +
  theme_minimal()

# ---- 2. Calculate and Plot Rolling Correlation Over Time ----
# Calculate rolling correlation between LE_ec and LE_era5
correlation_window <- 30  # Use a rolling window of 30 days
rolling_correlation <- merged_data %>%
  mutate(rolling_corr = zoo::rollapply(LE_ec, correlation_window, 
                                       function(x) cor(x, LE_era5[seq_along(x)], 
                                                       use = "complete.obs"),
                                       fill = NA))

# Plot the rolling correlation
ggplot(rolling_correlation, aes(x = date, y = rolling_corr)) +
  geom_line(color = "darkgreen") +
  labs(title = "Rolling Correlation Between Eddy Covariance and ERA5 (LE)",
       x = "Date", y = "Correlation") +
  theme_minimal()

# ---- 3. Cross-Correlation (Check for Lag) ----
# Calculate cross-correlation between the two time series (using the complete data)
ccf_result <- ccf(complete_data$LE.x, complete_data$LE_era5_location_5.75_100, lag.max = 30, plot = TRUE, 
                  main = "Cross-Correlation of LE (EC) and LE (ERA5) (5.75, 100)")

# ---- 4. Seasonal Correlation Analysis ----
# Create a month-year column for seasonal analysis
merged_data$month_year <- floor_date(merged_data$date, "month")

# Calculate monthly correlation between LE_ec and LE_era5
monthly_correlation <- merged_data %>%
  group_by(month_year) %>%
  summarize(corr = cor(LE_ec, LE_era5, use = "complete.obs"))

# Plot monthly correlation
ggplot(monthly_correlation, aes(x = month_year, y = corr)) +
  geom_line(color = "blue") +
  labs(title = "Monthly Correlation Between Eddy Covariance and ERA5 (LE)",
       x = "Month-Year", y = "Correlation") +
  theme_minimal()
