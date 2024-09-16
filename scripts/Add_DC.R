# Load required libraries
library(readxl)
library(writexl)
library(dplyr)
library(fs)
library(lubridate)
library(stringr)
library(tibble)

# Function to process a single file
process_single_file <- function(file_path) {
  # Read the Excel file
  data <- read_excel(file_path, col_types = "text")
  
  # Check if DC column already exists
  if ("DC" %in% names(data)) {
    cat("DC column already exists in:", basename(file_path), "\n")
    return(NULL)
  }
  
  # Convert all columns except the first to numeric
  data <- data %>%
    mutate(across(-1, as.numeric))
  
  # Rename trials sequentially
  data$Trial <- seq_len(nrow(data))
  
  # Add DC column with 0s
  dc_column <- rep(0, nrow(data))
  
  # Find the position to insert DC (between CT and DE)
  insert_position <- which(names(data) == "CT") + 1
  
  # Insert the DC column
  data <- data %>%
    add_column(DC = dc_column, .after = insert_position - 1)
  
  # Create new filename
  new_file_name <- gsub("election_results", "DC_election_results", basename(file_path))
  new_file_path <- file.path("data/processed", new_file_name)
  
  # Save the modified data to a new Excel file
  write_xlsx(data, new_file_path)
  
  cat("Processed file:", basename(file_path), "\n")
  cat("Saved as:", new_file_name, "\n\n")
  
  return(new_file_path)
}

# Function to find and process new daily files
process_new_files <- function(base_path) {
  # Get today's date
  today <- Sys.Date()
  
  # Find all relevant files from today
  today_files <- fs::dir_ls(
    base_path,
    recurse = TRUE,
    regexp = paste0("election_results_(direct|Fox|MSNBC|BBC)_", today, ".*\\.xlsx$")
  )
  
  # Process each file
  processed_files <- sapply(today_files, process_single_file)
  
  # Remove NULL entries (files that were skipped)
  processed_files <- processed_files[!sapply(processed_files, is.null)]
  
  cat("Processed", length(processed_files), "new files.\n")
  
  return(processed_files)
}

# Main execution
base_path <- "data/raw"
processed_files <- process_new_files(base_path)
# If you want to do something with the processed files, you can use the 'processed_files' variable