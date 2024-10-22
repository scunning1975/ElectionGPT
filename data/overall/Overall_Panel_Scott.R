# Load required libraries
library(readxl)
library(dplyr)
library(tidyr)
library(lubridate)
library(stringr)
library(purrr)
library(readr)
library(writexl)
library(fs)

# Function to process a single file
process_file <- function(file_path) {
  cat("Processing file:", file_path, "\n")
  
  # Extract filename
  file_name <- basename(file_path)
  
  # Extract date and time from filename using regex (case-insensitive for voice)
  date_time_match <- str_match(file_name, "_(\\d{4}-\\d{2}-\\d{2}_\\d{2}-\\d{2}-\\d{2})\\.xlsx$")
  if (is.na(date_time_match[1,2])) {
    cat("Error: Filename does not contain date and time in expected format.\n")
    return(NULL)
  }
  date_time_str <- date_time_match[1,2]
  date <- as_date(str_sub(date_time_str, 1, 10))
  
  # Extract voice from filename using case-insensitive regex
  voice_match <- str_match(file_name, regex("DC_election_results_(Fox|MSNBC|BBC)_\\d{4}-\\d{2}-\\d{2}_\\d{2}-\\d{2}-\\d{2}\\.xlsx$", ignore_case = TRUE))
  if (is.na(voice_match[1,2])) {
    cat("Error: Filename does not contain voice in expected format.\n")
    return(NULL)
  }
  voice <- str_to_title(voice_match[1,2]) # Standardize to Title Case
  
  cat("Extracted voice:", voice, "and date:", date, "\n")
  
  # Read the Excel file
  tryCatch({
    data <- read_excel(file_path, col_types = "text")
    cat("Successfully read Excel file. Dimensions:", dim(data), "\n")
  }, error = function(e) {
    cat("Error reading Excel file:", e$message, "\n")
    return(NULL)
  })
  
  # Check if DC column exists; if not, add it
  if (!"DC" %in% names(data)) {
    cat("Adding DC column\n")
    # Add DC column with 0s
    dc_column <- rep(0, nrow(data))
    
    # Find the position to insert DC (between CT and DE)
    if ("CT" %in% names(data)) {
      insert_position <- which(names(data) == "CT") + 1
    } else {
      cat("Error: Column 'CT' not found in the data.\n")
      return(NULL)
    }
    
    # Insert the DC column
    data <- data %>%
      add_column(DC = dc_column, .after = insert_position - 1)
  }
  
  # Convert all columns except 'Trial' and 'DC' to numeric
  data <- data %>%
    mutate(across(-c(Trial, DC), as.numeric))
  
  # Rename trials sequentially
  data$Trial <- seq_len(nrow(data))
  
  cat("Processed file. Final dimensions:", dim(data), "\n")
  return(list(data = data, voice = voice, date = date))
}

# Function to reshape and clean data
reshape_and_clean <- function(data, voice, date) {
  cat("Reshaping and cleaning data for voice:", voice, "and date:", date, "\n")
  
  # Reshape the data to long format
  long_data <- data %>%
    pivot_longer(cols = -c(Trial, DC), names_to = "State", values_to = "Result") %>%
    mutate(
      Date = format(date, "%m/%d/%y"),
      Voice = voice
    ) %>%
    select(Date, Voice, Trial, State, Result)
  
  cat("Reshaped data. Dimensions:", dim(long_data), "\n")
  
  # Identify Trials with any blanks
  trials_with_blanks <- long_data %>%
    group_by(Trial) %>%
    summarize(any_blank = any(is.na(Result) | Result == "")) %>%
    filter(any_blank) %>%
    pull(Trial)
  
  # Filter out trials with any blanks
  cleaned_long_data <- long_data %>%
    filter(!Trial %in% trials_with_blanks)
  
  cat("Trials with blanks removed:", length(trials_with_blanks), "\n")
  cat("Final cleaned data dimensions:", dim(cleaned_long_data), "\n")
  
  return(cleaned_long_data)
}

# Function to find the most recent file for each voice
find_most_recent_file <- function(directory, voice) {
  files <- fs::dir_ls(
    directory,
    recurse = TRUE,
    regexp = paste0("DC_election_results_", voice, "_.*\\.xlsx$"),
    ignore.case = TRUE
  )
  
  if (length(files) == 0) {
    cat("No matching files found for voice:", voice, "\n")
    return(NULL)
  }
  
  file_info <- file.info(files)
  most_recent_file <- rownames(file_info)[which.max(file_info$mtime)]
  
  return(most_recent_file)
}

# Main execution

# Specify the directory to search for files
base_directory <- "/Users/scott_cunningham/Desktop/ElectionGPT/"

# List of voices to process
voices <- c("FOX", "MSNBC", "BBC")

# Find the most recent file for each voice
files_to_process <- map_chr(voices, ~find_most_recent_file(base_directory, .x))

# Remove any NULL entries (in case a file wasn't found for a voice)
files_to_process <- files_to_process[!sapply(files_to_process, is.null)]

# Verify that files were found
if (length(files_to_process) == 0) {
  stop("No files found to process.")
}

# Process each file individually
processed_data <- list()
for (file in files_to_process) {
  cat("\nProcessing file:", file, "\n")
  result <- process_file(file)
  if (!is.null(result)) {
    cleaned_data <- reshape_and_clean(result$data, result$voice, result$date)
    processed_data[[length(processed_data) + 1]] <- cleaned_data
    cat("Processed data dimensions:", dim(cleaned_data), "\n")
  } else {
    cat("Failed to process file:", file, "\n")
  }
}

# Combine the datasets into one panel
panel_data <- bind_rows(processed_data)

# Save the panel dataset
panel_file <- file.path(base_directory, "panel_election_results_state.csv")
write_csv(panel_data, panel_file)

cat("\nPanel dataset created and saved as:", panel_file, "\n")
cat("Total rows in panel dataset:", nrow(panel_data), "\n")