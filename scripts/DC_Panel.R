# Load required libraries
library(readxl)
library(dplyr)
library(tidyr)
library(lubridate)
library(stringr)
library(purrr)
library(readr)
library(fs)

append_to_panel_dataset <- function(base_path, panel_file) {
  # Get today's date
  today <- Sys.Date()
  
  # Find all DC_election_results files for today
  new_files <- fs::dir_ls(
    base_path,
    recurse = TRUE,
    regexp = paste0("DC_election_results_(direct|Fox|MSNBC|BBC)_", today)
  )
  
  # Function to process a single file
  process_file <- function(file_path) {
    # Extract date and voice from filename
    file_name <- basename(file_path)
    date <- as_date(str_extract(file_name, "\\d{4}-\\d{2}-\\d{2}"))
    voice <- str_extract(file_name, "(direct|Fox|MSNBC|BBC)")
    
    # Read the Excel file
    data <- read_excel(file_path)
    
    # Reshape the data to long format
    long_data <- data %>%
      pivot_longer(cols = -Trial, names_to = "State", values_to = "Result") %>%
      mutate(
        Date = date,
        Voice = voice,
        Date = format(Date, "%m/%d/%y")
      ) %>%
      select(Date, Voice, Trial, State, Result)
    
    return(long_data)
  }
  
  # Process new files and combine into one dataset
  new_panel_data <- map_dfr(new_files, process_file)
  
  # If no new data, exit the function
  if (nrow(new_panel_data) == 0) {
    cat("No new data found for today. Exiting.\n")
    return(NULL)
  }
  
  # Read existing panel data
  existing_panel_data <- read_csv(panel_file)
  
  # Check for duplicates
  new_panel_data <- new_panel_data %>%
    anti_join(existing_panel_data, by = c("Date", "Voice", "Trial", "State"))
  
  # If no new unique data, exit the function
  if (nrow(new_panel_data) == 0) {
    cat("No new unique data found for today. Exiting.\n")
    return(NULL)
  }
  
  # Combine existing and new data
  updated_panel_data <- bind_rows(existing_panel_data, new_panel_data)
  
  # Save the updated panel dataset, overwriting the existing file
  write_csv(updated_panel_data, panel_file)
  
  cat("Panel dataset updated and saved as:", panel_file, "\n")
  cat("Total rows in updated panel:", nrow(updated_panel_data), "\n")
  cat("New unique rows added:", nrow(new_panel_data), "\n")
  
  return(updated_panel_data)
}

# Example usage
base_path <- "data/processed"
panel_file <- "data/panel_election_results_state.csv"
result <- append_to_panel_dataset(base_path, panel_file)
if (is.null(result)) {
  cat("No updates were made to the panel dataset.\n")
} else {
  cat("Panel dataset was successfully updated.\n")
}