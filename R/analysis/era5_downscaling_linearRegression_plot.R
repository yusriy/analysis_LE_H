library(ggplot2)

# Function to plot linear regression between eddy covariance LE and ERA5 LE, including the p-value and R²
plot_regression <- function(merged_data, location_column, location_name) {
  
  # Ensure that the required columns exist
  if (!("LE.x" %in% colnames(merged_data)) || !(location_column %in% colnames(merged_data))) {
    stop(paste("Missing required columns in the dataset for location:", location_name))
  }
  
  # Perform the linear regression
  model <- lm(LE.x ~ merged_data[[location_column]], data = merged_data)
  
  # Extract the p-value from the model summary
  p_value <- summary(model)$coefficients[2, 4]
  
  # Calculate the correlation coefficient (R²)
  r_squared <- summary(model)$r.squared
  
  # Create the scatter plot with a regression line
  p <- ggplot(merged_data, aes_string(x = location_column, y = "LE.x")) +
    geom_point(alpha = 0.6) +
    geom_smooth(method = "lm", se = FALSE, color = "blue") +
    labs(title = paste("Linear Regression between Eddy Covariance LE and ERA5 LE at", location_name),
         x = paste("ERA5 Latent Heat Flux (", location_column, ")", sep = ""),
         y = "Eddy Covariance Latent Heat Flux (LE.x)") +
    theme_minimal() +
    annotate("text", x = Inf, y = Inf, label = paste("p-value:", format(p_value, digits = 3),
                                                     "\nR²:", format(r_squared, digits = 3)),
             hjust = 1.1, vjust = 1.5, size = 5, color = "red", fontface = "italic")
  
  print(p)
}

# File paths to the merged datasets
merged_files <- list.files(path = "data/", pattern = "merged_day_.*\\.csv", full.names = TRUE)

# Loop through each merged dataset and plot the linear regression with p-value and R²
for (file in merged_files) {
  
  # Load the merged data
  merged_data <- read.csv(file)
  
  # Extract the location name correctly (coordinates only)
  location_name <- gsub("merged_day_location_", "", gsub("\\.csv$", "", basename(file)))
  
  # Identify the correct column name for the ERA5 latent heat in the current file
  location_column <- paste0("LE_era5_location_", location_name)
  
  # Plot the regression for this location
  plot_regression(merged_data, location_column, location_name)
}
