
library(ggplot2)
library(dplyr)
library(lubridate)
library(patchwork)  # For combining plots side by side

# Assume df_merged_day is already loaded with Eddy Covariance data
# Ensure the date column is in Date format
df_merged_day <- df_merged_day %>%
  mutate(date = as.Date(date))  # Convert to Date format

# Extract month and day, and calculate daily averages for LE and H
ec_daily_avg <- df_merged_day %>%
  mutate(
    month = month(date, label = TRUE),  # Extract month as a factor with labels (Jan, Feb, ...)
    day = day(date)                     # Extract day of the month
  ) %>%
  group_by(month, day) %>%
  summarize(
    ec_avg_LE = mean(LE.x, na.rm = TRUE),  # Replace `LE.x` with the EC latent heat flux variable
    ec_avg_H = mean(H.x, na.rm = TRUE),    # Replace `H.x` with the EC sensible heat flux variable
    .groups = "drop"
  )

# Create the LE calendar heatmap
le_heatmap <- ggplot(ec_daily_avg, aes(x = day, y = month, fill = ec_avg_LE)) +
  geom_tile(color = "white") +  # Add grid lines between tiles
  scale_fill_distiller(
    name = expression(LE ~ (W ~ m^-2)),  # Add superscript for m^-2
    palette = "RdYlBu",                  # Divergent palette with red-yellow-blue
    direction = -1                       # Negative values in blue, positive in red
  ) +
  labs(x = NULL, y = NULL) +  # Remove axis labels
  theme_minimal() +
  theme(
    panel.grid = element_blank(),         # Remove gridlines
    axis.text.x = element_text(size = 8), # Add x-axis text
    axis.text.y = element_text(size = 10),# Adjust y-axis labels
    legend.position = "bottom",           # Legend at the bottom
    legend.title = element_text(size = 10),# Adjust legend title size
    legend.text = element_text(size = 8), # Adjust legend text size
    plot.margin = margin(r = 20)          # Add spacing for shared x-axis
  )

# Create the H calendar heatmap
h_heatmap <- ggplot(ec_daily_avg, aes(x = day, y = month, fill = ec_avg_H)) +
  geom_tile(color = "white") +  # Add grid lines between tiles
  scale_fill_distiller(
    name = expression(H ~ (W ~ m^-2)),  # Add superscript for m^-2
    palette = "RdYlBu",                # Divergent palette with red-yellow-blue
    direction = -1                     # Negative values in blue, positive in red
  ) +
  labs(x = NULL, y = NULL) +  # Remove axis labels
  theme_minimal() +
  theme(
    panel.grid = element_blank(),         # Remove gridlines
    axis.text.x = element_text(size = 8), # Add x-axis text
    axis.text.y = element_blank(),        # Remove y-axis labels
    legend.position = "none",             # No legend for H
    plot.margin = margin(l = 20)          # Add spacing for shared x-axis
  )

# Combine the two heatmaps and add a shared x-axis label
combined_heatmaps <- le_heatmap + h_heatmap +
  plot_layout(ncol = 2) +  # Side-by-side layout
  plot_annotation(tag_levels = "A") +  # Add subplot labels (A, B)
  plot_layout(guides = "collect") &  # Collect legends
  theme(
    legend.position = "bottom",           # Shared legend at the bottom
    plot.margin = margin(t = 20, b = 20)  # Add spacing for shared x-axis label
  )

# Add the shared x-axis label
combined_heatmaps <- combined_heatmaps + plot_layout(nrow = 2, heights = c(10, 1)) +
  plot_annotation(
    theme = theme(
      plot.margin = margin(r = 0),
      text = element_text(size = 12)
    )
  ) &
  labs(x = "Day of the Month")

# Display the combined heatmaps
print(combined_heatmaps)


# Save the combined heatmaps
ggsave("OneDrive/USM/Documents/research/Data_analysis/analysis_LE_H/EC_calendar_heatmap2.jpeg", plot = combined_heatmaps, width = 14, height = 8, dpi = 300)






