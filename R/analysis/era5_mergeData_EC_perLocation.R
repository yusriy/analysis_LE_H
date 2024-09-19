library(dplyr)
library(lubridate)

# Load the ERA5 data files
era5_files <- list.files(path = "data/", pattern = "location_.*\\.csv", full.names = TRUE)

# Initialize lists to store the daily and monthly averaged dataframes
daily_avg_list <- list()
monthly_avg_list <- list()

# Loop through each file and calculate daily and monthly averages
for (file in era5_files) {
  
  # Load the ERA5 data
  era5_data <- read.csv(file)
  
  # Ensure the time column is in POSIXct format
  era5_data$time <- as.POSIXct(era5_data$time, tz = "Asia/Kuala_Lumpur")
  
  # Calculate daily averages
  daily_avg <- era5_data %>%
    mutate(date = as.Date(time)) %>%
    group_by(date) %>%
    summarize(across(where(is.numeric), mean, na.rm = TRUE))
  
  # Calculate monthly averages
  monthly_avg <- era5_data %>%
    mutate(month = floor_date(time, "month")) %>%
    group_by(month) %>%
    summarize(across(where(is.numeric), mean, na.rm = TRUE))
  
  # Extract the location coordinates from the file name
  location_name <- gsub("data/location_|\\.csv", "", basename(file))
  
  # Add location coordinates as a prefix to the column names (excluding the date column)
  colnames(daily_avg)[-1] <- paste0(colnames(daily_avg)[-1], "_", location_name)
  colnames(monthly_avg)[-1] <- paste0(colnames(monthly_avg)[-1], "_", location_name)
  
  # Store the results in lists with location names
  daily_avg_list[[location_name]] <- daily_avg
  monthly_avg_list[[location_name]] <- monthly_avg
}

# Load the eddy covariance data
df_merged_day <- read.csv("data/df_merged_day.csv")
df_merged_month <- read.csv("data/df_merged_month.csv")

# Convert date columns to Date format for merging
df_merged_day$date <- as.Date(df_merged_day$date)
df_merged_month$date <- as.Date(df_merged_month$date)

# Initialize lists to store merged dataframes
merged_day_list <- list()
merged_month_list <- list()

# Merge the ERA5 daily and monthly averages with the eddy covariance data for each location
for (location_name in names(daily_avg_list)) {
  
  # Merge daily data
  merged_day <- left_join(df_merged_day, daily_avg_list[[location_name]], by = "date")
  
  # Merge monthly data
  merged_month <- left_join(df_merged_month, monthly_avg_list[[location_name]], by = c("date" = "month"))
  
  # Store merged data in lists with location names
  merged_day_list[[location_name]] <- merged_day
  merged_month_list[[location_name]] <- merged_month
  
  # Optionally, save the merged datasets to CSV
  write.csv(merged_day, paste0("data/merged_day_", location_name, ".csv"), row.names = FALSE)
  write.csv(merged_month, paste0("data/merged_month_", location_name, ".csv"), row.names = FALSE)
}
