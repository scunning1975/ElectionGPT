# Load necessary libraries
library(dplyr)
library(lubridate)

# Function to download the latest CSV file
download_latest_data <- function() {
  url <- "https://static.dwcdn.net/data/kFsH6.csv"
  destfile <- "/Users/jaredblack/GitHub/ElectionGPT/data/expert/silverbulleton_predictions.csv"
  download.file(url, destfile)
  cat("File downloaded successfully:", destfile, "\n")
}

# Call the function to download the latest data
download_latest_data()

# File paths
silver_path <- "/Users/jaredblack/GitHub/ElectionGPT/data/expert/silverbulleton_predictions.csv"
output_path <- "/Users/jaredblack/GitHub/ElectionGPT/data/expert/expert_combined_panel.csv"

# Read in the CSV files
silver_data <- read.csv(silver_path)
existing_data <- read.csv(output_path, stringsAsFactors = FALSE)

# Process silver data
silver_data <- silver_data %>%
  filter(state == "National") %>%
  mutate(date = as.Date(modeldate, format = "%m/%d/%y")) %>%
  select(date, harris, trump) %>%
  rename(Harris = harris, Trump = trump) %>%
  mutate(source = "silver") %>%
  filter(!is.na(Harris) & !is.na(Trump)) %>%
  group_by(date) %>%
  slice(1) %>%  # Keep only the first entry for each day
  ungroup()

# Find the last date in the existing data
last_date <- as.Date(max(existing_data$date), format = "%m/%d/%y")

# Filter new data to append
new_data <- silver_data %>%
  filter(date > last_date) %>%
  mutate(date = format(date, "%m/%d/%y")) %>%  # Format date as in the existing data
  select(date, Harris, Trump, source)  # Ensure correct column order

# Append the new data to the existing data
combined_panel <- bind_rows(existing_data, new_data)

# Save the updated combined panel to the CSV
write.csv(combined_panel, output_path, row.names = FALSE, quote = FALSE)

# Print messages
cat("Combined panel CSV has been updated successfully!\n")
cat("Number of rows before update:", nrow(existing_data), "\n")
cat("Number of new rows appended:", nrow(new_data), "\n")
cat("Total number of rows after update:", nrow(combined_panel), "\n")
cat("New dates added:", paste(unique(new_data$date), collapse = ", "), "\n")