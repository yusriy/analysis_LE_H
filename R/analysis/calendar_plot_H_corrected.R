library(dplyr)
library(ggplot2)
library(qmap)
library(zoo)  # For na.approx()
library(lubridate)

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

# Combine corrected data for all locations
combined_corrected_data <- data.frame()

# Loop through all locations
for (i in 1:nrow(locations)) {
  # Load the merged data for each location
  merged_data <- read.csv(locations$file[i])
  
  # Ensure the date column is in Date format
  merged_data$date <- as.Date(merged_data$date)
  
  # Separate sensible heat flux from eddy covariance (H_ec) and ERA5 (H_era5)
  H_ec <- merged_data$H.x  # Eddy covariance sensible heat flux
  H_era5 <- merged_data[[locations$era5_col[i]]]  # ERA5 sensible heat flux for the specific location
  
  # Interpolate missing values
  merged_data$H_ec <- na.approx(H_ec, na.rm = FALSE)
  merged_data$H_era5 <- na.approx(H_era5, na.rm = FALSE)
  
  # Remove rows with remaining NAs
  merged_data <- merged_data %>%
    filter(!is.na(H_ec) & !is.na(H_era5))
  
  # Temporal shift
  shift_days <- 1  # Based on prior analysis
  merged_data$H_era5_shifted <- dplyr::lag(merged_data$H_era5, n = shift_days)
  
  # Remove rows with NA after shift
  merged_data <- merged_data %>%
    filter(!is.na(H_era5_shifted))
  
  # Apply quantile mapping
  qm_fit <- fitQmap(merged_data$H_ec, merged_data$H_era5_shifted, method = "RQUANT")
  merged_data$H_era5_qmapped <- doQmap(merged_data$H_era5_shifted, qm_fit)
  
  # Add location info
  merged_data$location <- locations$name[i]
  
  # Append to combined data
  combined_corrected_data <- bind_rows(combined_corrected_data, merged_data)
}

# Prepare data for calendar plot
daily_avg_data <- combined_corrected_data %>%
  group_by(location, date) %>%
  summarize(H_corrected_avg = mean(H_era5_qmapped, na.rm = TRUE), .groups = "drop") %>%
  mutate(
    month = month(date, label = TRUE),
    day = day(date)
  )

# Filter data for Point 7
point7_data <- daily_avg_data %>% filter(location == "5.75_100")

# Generate the calendar plot for Point 7
point7_plot <- ggplot(point7_data, aes(x = day, y = month, fill = H_corrected_avg)) +
  geom_tile(color = "white") +
  scale_fill_gradient2(
    name = expression(H ~ (W ~ m^-2)),
    low = "blue", mid = "white", high = "red", midpoint = mean(point7_data$H_corrected_avg, na.rm = TRUE)
  ) +
  labs(
    title = "Point 7 H (Corrected)",
    x = "Day of the Month",
    y = "Month"
  ) +
  theme_minimal() +
  theme(
    panel.grid = element_blank(),
    plot.title = element_text(hjust = 0.5),
    legend.position = "right"
  )

# Save the plot for Point 7
ggsave("Point7_H_Corrected_Calendar.jpeg", plot = point7_plot, width = 8, height = 6, dpi = 300)

# Display the plot
print(point7_plot)
