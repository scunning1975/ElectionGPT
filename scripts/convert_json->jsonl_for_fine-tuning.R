# Load required libraries
library(jsonlite)
library(tidyverse)

# Set the directory path
dir_path <- "/Users/jaredblack/GitHub/ElectionGPT/data/news/"

# Get list of all .json files in the directory
json_files <- "/Users/jaredblack/GitHub/ElectionGPT/data/news/news_data_2024-09-09_12-25-55.json"

# Function to convert a single JSON file to JSONL
convert_json_to_jsonl <- function(file_path) {
  # Read the JSON file
  json_data <- read_json(file_path)
  
  # Create the new file name
  new_file_name <- str_replace(file_path, "\\.json$", ".jsonl")
  
  # Write to JSONL format
  if (is.data.frame(json_data) || is.list(json_data)) {
    write_lines(map_chr(json_data, ~ toJSON(., auto_unbox = TRUE)), new_file_name)
  } else {
    write_lines(toJSON(json_data, auto_unbox = TRUE), new_file_name)
  }
  
  cat("Converted:", file_path, "to", new_file_name, "\n")
}

# Apply the conversion function to all JSON files
walk(json_files, convert_json_to_jsonl)

cat("All conversions complete. Original JSON files have been preserved.\n")

# Combine all JSONL files into a single JSONL file
# Load required libraries
library(jsonlite)
library(purrr)

# Set the directory path
dir_path <- "/Users/jaredblack/GitHub/ElectionGPT/data/news"

# Get list of all .jsonl files in the directory
jsonl_files <- list.files(path = dir_path, pattern = "\\.jsonl$", full.names = TRUE)

# Function to read JSONL file and ensure all lines are character strings
read_jsonl <- function(file_path) {
  lines <- readLines(file_path, warn = FALSE)
  sapply(lines, function(line) {
    tryCatch(
      as.character(line),
      error = function(e) {
        cat("Error processing line in file:", file_path, "\n")
        cat("Error message:", conditionMessage(e), "\n")
        return(NA)
      }
    )
  })
}

# Read and combine all JSONL files
combined_data <- jsonl_files %>%
  map(read_jsonl) %>%
  flatten() %>%
  unlist()

# Remove any NA values that might have been introduced due to errors
combined_data <- combined_data[!is.na(combined_data)]

# Create the output file name
output_file <- file.path(dir_path, "combined_data.jsonl")

# Write the combined data to a single JSONL file
writeLines(combined_data, output_file)

cat("All JSONL files have been combined into:", output_file, "\n")
cat("Total lines in combined file:", length(combined_data), "\n")