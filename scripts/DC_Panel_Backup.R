# Load required libraries
library(readxl)
library(dplyr)
library(tidyr)
library(lubridate)
library(stringr)
library(purrr)
library(readr)
library(fs)

append_to_panel_dataset <- function(panel_file) {
  # Specify the exact files to process
  new_files <- c(
    "/Users/jaredblack/GitHub/ElectionGPT/data/processed/DC_election_results_direct_2024-10-27_14-02-49.xlsx",
    "/Users/jaredblack/GitHub/ElectionGPT/data/processed/DC_election_results_MSNBC_2024-10-27_14-03-10.xlsx",
    "/Users/jaredblack/GitHub/ElectionGPT/data/processed/DC_election_results_Fox_2024-10-27_14-03-00.xlsx",
    "/Users/jaredblack/GitHub/ElectionGPT/data/processed/DC_election_results_BBC_2024-10-27_14-03-21.xlsx"
  )
  
  # Function to process a single file
  process_file <- function(file_path) {
    # Extract voice from filename
    file_name <- basename(file_path)
    voice <- str_extract(file_name, "(direct|Fox|MSNBC|BBC)")
    
    # Read the Excel file
    data <- read_excel(file_path)
    
    # Reshape the data to long format
    long_data <- data %>%
      pivot_longer(cols = -Trial, names_to = "State", values_to = "Result") %>%
      mutate(
        Date = "10/27/24",  # Set fixed date as requested
        Voice = voice
      ) %>%
      select(Date, Voice, Trial, State, Result)
    
    return(long_data)
  }
  
  # Process new files and combine into one dataset
  new_panel_data <- map_dfr(new_files, process_file)
  
  # If no new data, exit the function
  if (nrow(new_panel_data) == 0) {
    cat("No new data found. Exiting.\n")
    return(NULL)
  }
  
  # Read existing panel data
  existing_panel_data <- read_csv(panel_file)
  
  # Check for duplicates
  new_panel_data <- new_panel_data %>%
    anti_join(existing_panel_data, by = c("Date", "Voice", "Trial", "State"))
  
  # If no new unique data, exit the function
  if (nrow(new_panel_data) == 0) {
    cat("No new unique data found. Exiting.\n")
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
panel_file <- "/Users/jaredblack/GitHub/ElectionGPT/data/panel_election_results_state.csv"
result <- append_to_panel_dataset(panel_file)
if (is.null(result)) {
  cat("No updates were made to the panel dataset.\n")
} else {
  cat("Panel dataset was successfully updated.\n")
}