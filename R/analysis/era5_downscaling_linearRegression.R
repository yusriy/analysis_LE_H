library(dplyr)

# Function to perform downscaling using linear regression
downscale_latent_heat <- function(merged_data, location_column, location_name) {
  
  # Ensure that the required columns exist
  if (!("LE.x" %in% colnames(merged_data))) {
    stop(paste("Column 'LE.x' is missing in the dataset for location:", location_name))
  }
  
  if (!(location_column %in% colnames(merged_data))) {
    print(paste("Available columns in", location_name, ":"))
    print(colnames(merged_data))
    stop(paste("Column", location_column, "is missing in the dataset for location:", location_name))
  }
  
  # Perform linear regression
  model <- lm(LE.x ~ merged_data[[location_column]], data = merged_data)
  
  # Predict the downscaled LE using the model
  downscaled_le <- data.frame(
    date = merged_data$date,
    LE_downscaled = predict(model, newdata = merged_data)
  )
  
  return(downscaled_le)
}

# File paths to the merged datasets
merged_files <- list.files(path = "data/", pattern = "merged_day_.*\\.csv", full.names = TRUE)

# Initialize a list to store the downscaled results
downscaled_list <- list()

# Loop through each merged dataset and perform downscaling
for (file in merged_files) {
  
  # Load the merged data
  merged_data <- read.csv(file)
  
  # Extract the location name correctly (coordinates only)
  location_name <- gsub("merged_day_location_", "", gsub("\\.csv$", "", basename(file)))
  
  # Identify the correct column name for the ERA5 latent heat in the current file
  location_column <- paste0("LE_era5_location_", location_name)
  
  # Debugging: Print the location and corresponding column name
  print(paste("Processing location:", location_name))
  print(paste("Using ERA5 column:", location_column))
  
  # Perform downscaling
  downscaled_data <- downscale_latent_heat(merged_data, location_column, location_name)
  
  # Store the downscaled data in the list
  downscaled_list[[location_name]] <- downscaled_data
  
  # Optionally, save the downscaled dataset to a CSV file
  write.csv(downscaled_data, paste0("data/downscaled_latent_heat_", location_name, ".csv"), row.names = FALSE)
}