# Load necessary libraries
library(dplyr)
library(lubridate)

# Function to download the latest CSV file
download_latest_data <- function() {
  url <- "https://static.dwcdn.net/data/kFsH6.csv"
  destfile <- "/Users/jaredblack/GitHub/ElectionGPT/data/expert/silverbulleton_predictions.csv"
  download.file(url, destfile, method = "curl")  # Use 'curl' method for better compatibility
  cat("File downloaded successfully:", destfile, "\n")
}

# Call the function to download the latest data
download_latest_data()

# File paths
silver_path <- "/Users/jaredblack/GitHub/ElectionGPT/data/expert/silverbulleton_predictions.csv"
output_path <- "/Users/jaredblack/GitHub/ElectionGPT/data/expert/expert_combined_panel.csv"

# Read in the CSV files
silver_data <- read.csv(silver_path, stringsAsFactors = FALSE)
existing_data <- read.csv(output_path, stringsAsFactors = FALSE)

# Process silver data
silver_processed <- silver_data %>%
  filter(state == "National") %>%
  mutate(date = as.Date(modeldate, format = "%m/%d/%y")) %>%
  select(date, harris, trump) %>%
  rename(Harris = harris, Trump = trump) %>%
  mutate(source = "silver") %>%
  filter(!is.na(Harris) & !is.na(Trump)) %>%
  group_by(date) %>%
  slice(1) %>%  # Keep only the first entry for each day to remove duplicates
  ungroup()

# Ensure date formats match between existing_data and silver_processed
existing_data <- existing_data %>%
  mutate(date = as.Date(date, format = "%m/%d/%y"))

silver_processed <- silver_processed %>%
  mutate(date = as.Date(date, format = "%m/%d/%y"))

# Identify the most recent date in silver_processed
latest_silver_date <- max(silver_processed$date, na.rm = TRUE)

# Check if the latest_silver_date already exists in existing_data for the 'silver' source
date_exists <- existing_data %>%
  filter(date == latest_silver_date & source == "silver") %>%
  nrow() > 0

if (!date_exists) {
  # Extract data for the latest_silver_date
  latest_data <- silver_processed %>%
    filter(date == latest_silver_date) %>%
    mutate(date = format(date, "%m/%d/%y"))  # Format date as in the existing data
  
  # Append the latest_data to existing_data
  combined_panel <- bind_rows(existing_data, latest_data)
  
  # Save the updated combined panel to the CSV
  write.csv(combined_panel, output_path, row.names = FALSE, quote = FALSE)
  
  # Print success messages
  cat("Combined panel CSV has been updated successfully!\n")
  cat("Number of rows before update:", nrow(existing_data), "\n")
  cat("Number of new rows appended:", nrow(latest_data), "\n")
  cat("Total number of rows after update:", nrow(combined_panel), "\n")
  cat("New date added:", latest_data$date, "\n")
} else {
  cat("No new data to append. The latest date (", format(latest_silver_date, "%m/%d/%y"), ") already exists in the panel.\n", sep = "")
}