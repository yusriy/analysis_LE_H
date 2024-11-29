library(ggplot2)
library(dplyr)
library(lubridate)  # For date manipulations
library(tidyr)      # For pivot_longer

# Prepare data for Point 7
point_name <- "5.75_100"  # Point 7 coordinates
merged_data <- read.csv("data/merged_day_location_5.75_100.csv")

# Ensure date column is in Date format
merged_data$date <- as.Date(merged_data$date)

# Calculate residuals for LE and H
merged_data <- merged_data %>%
  mutate(
    LE_residual = LE_era5_corrected - LE.x,
    H_residual = H_era5_corrected - H.x
  )

# Prepare data for calendar plot
calendar_data <- merged_data %>%
  dplyr::select(date, LE_residual, H_residual) %>%
  pivot_longer(cols = c(LE_residual, H_residual), names_to = "Flux_Type", values_to = "Residual") %>%
  mutate(
    Flux_Type = recode(Flux_Type, LE_residual = "LE Residual", H_residual = "H Residual"),
    month = month(date, label = TRUE),  # Extract month as a factor
    day = day(date)                     # Extract day of the month
  )

# Define divergent color scale
divergent_colors <- scale_fill_gradient2(
  low = "blue", mid = "white", high = "red", midpoint = 0,
  name = expression(Residual ~ (W ~ m^-2))
)

# Create calendar plot for LE Residual
le_calendar <- ggplot(calendar_data %>% filter(Flux_Type == "LE Residual"), aes(x = day, y = month, fill = Residual)) +
  geom_tile(color = "white") +
  divergent_colors +
  labs(
    title = "LE Residual (Point 7)",
    x = "Day of the Month",
    y = "Month"
  ) +
  theme_minimal() +
  theme(
    panel.grid = element_blank(),
    axis.text.x = element_text(size = 8),
    axis.text.y = element_text(size = 10),
    legend.position = "bottom",
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5)
  )

# Save the LE calendar plot
ggsave("calendar_plot_LE_residual_point7.jpeg", plot = le_calendar, width = 10, height = 8, dpi = 300)

# Create calendar plot for H Residual
h_calendar <- ggplot(calendar_data %>% filter(Flux_Type == "H Residual"), aes(x = day, y = month, fill = Residual)) +
  geom_tile(color = "white") +
  divergent_colors +
  labs(
    title = "H Residual (Point 7)",
    x = "Day of the Month",
    y = "Month"
  ) +
  theme_minimal() +
  theme(
    panel.grid = element_blank(),
    axis.text.x = element_text(size = 8),
    axis.text.y = element_text(size = 10),
    legend.position = "bottom",
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5)
  )

# Save the H calendar plot
ggsave("calendar_plot_H_residual_point7.jpeg", plot = h_calendar, width = 10, height = 8, dpi = 300)

# Display the separate calendar plots
print(le_calendar)
print(h_calendar)