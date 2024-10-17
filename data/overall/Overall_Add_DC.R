# Load required libraries
library(readxl)
library(writexl)
library(dplyr)
library(fs)
library(lubridate)
library(stringr)
library(tibble)

# Function to process a single file
process_single_file <- function(file_path, output_directory) {
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
  # Assuming 'CT' is a valid column name in your data
  if ("CT" %in% names(data)) {
    insert_position <- which(names(data) == "CT") + 1
  } else {
    stop("Column 'CT' not found in the data.")
  }
  
  # Insert the DC column
  data <- data %>%
    add_column(DC = dc_column, .after = insert_position - 1)
  
  # Create new filename by prefixing with "DC_"
  original_filename <- basename(file_path)
  new_file_name <- paste0("DC_", original_filename)
  new_file_path <- file.path(output_directory, new_file_name)
  
  # Save the modified data to a new Excel file
  write_xlsx(data, new_file_path)
  
  cat("Processed file:", original_filename, "\n")
  cat("Saved as:", new_file_name, "\n\n")
  
  return(new_file_path)
}

# Main execution

# Specify the exact file to process
input_file <- "/Users/jaredblack/GitHub/ElectionGPT/data/overall/election_results_FOX_2024-10-16_12-07-58.xlsx"

# Specify the output directory
output_directory <- "/Users/jaredblack/GitHub/ElectionGPT/data/overall/"

# Check if the input file exists
if (!file.exists(input_file)) {
  stop("Input file does not exist: ", input_file)
}

# Process the single file
processed_file <- process_single_file(input_file, output_directory)

# Optional: If you want to perform additional actions with the processed file, you can do so here
if (!is.null(processed_file)) {
  cat("File successfully processed and saved to:", processed_file, "\n")
} else {
  cat("No processing was done. The DC column may already exist.\n")
}