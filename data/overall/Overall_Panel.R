# Load required libraries
library(readxl)
library(dplyr)
library(tidyr)
library(lubridate)
library(stringr)
library(purrr)
library(readr)
library(writexl)

# Function to process a single file
process_file <- function(file_path) {
  # Extract filename
  file_name <- basename(file_path)
  
  # Extract date and time from filename using regex (case-insensitive for voice)
  date_time_match <- str_match(file_name, "_(\\d{4}-\\d{2}-\\d{2}_\\d{2}-\\d{2}-\\d{2})\\.xlsx$")
  if (is.na(date_time_match[1,2])) {
    stop("Filename does not contain date and time in expected format.")
  }
  date_time_str <- date_time_match[1,2]
  date <- as_date(str_sub(date_time_str, 1, 10))
  
  # Extract voice from filename using case-insensitive regex
  voice_match <- str_match(file_name, regex("DC_election_results_(direct|Fox|MSNBC|BBC)_\\d{4}-\\d{2}-\\d{2}_\\d{2}-\\d{2}-\\d{2}\\.xlsx$", ignore_case = TRUE))
  if (is.na(voice_match[1,2])) {
    stop("Filename does not contain voice in expected format.")
  }
  voice <- str_to_title(voice_match[1,2]) # Standardize to Title Case
  
  # Read the Excel file
  data <- read_excel(file_path, col_types = "text")
  
  # Check if DC column exists; if not, add it
  if (!"DC" %in% names(data)) {
    # Add DC column with 0s
    dc_column <- rep(0, nrow(data))
    
    # Find the position to insert DC (between CT and DE)
    if ("CT" %in% names(data)) {
      insert_position <- which(names(data) == "CT") + 1
    } else {
      stop("Column 'CT' not found in the data.")
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
  
  return(data)
}

# Function to reshape and clean data
reshape_and_clean <- function(data, voice, date) {
  # Reshape the data to long format
  long_data <- data %>%
    pivot_longer(cols = -c(Trial, DC), names_to = "State", values_to = "Result") %>%
    mutate(
      Date = format(date, "%m/%d/%y"),
      Voice = voice
    ) %>%
    select(Date, Voice, Trial, State, Result)
  
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
  
  return(cleaned_long_data)
}

# Main execution

# Specify the two files to process
file1 <- "/Users/jaredblack/GitHub/ElectionGPT/data/overall/DC_election_results_FOX_2024-10-16_12-07-58.xlsx"
file2 <- "/Users/jaredblack/GitHub/ElectionGPT/data/overall/DC_election_results_MSNBC_2024-10-16_12-08-03.xlsx"

# List of files to process
files_to_process <- c(file1, file2)

# Verify that the files exist
missing_files <- files_to_process[!file.exists(files_to_process)]
if (length(missing_files) > 0) {
  stop("The following files do not exist:\n", paste(missing_files, collapse = "\n"))
}

# Process each file
processed_data <- map(files_to_process, function(file) {
  data <- process_file(file)
  
  # Extract voice and date from filename
  file_name <- basename(file)
  voice <- str_to_title(str_match(file_name, regex("DC_election_results_(direct|Fox|MSNBC|BBC)_", ignore_case = TRUE))[1,2])
  date_str <- str_match(file_name, "_(\\d{4}-\\d{2}-\\d{2})_")[1,2]
  date <- as_date(date_str)
  
  # Reshape and clean the data
  cleaned_data <- reshape_and_clean(data, voice, date)
  
  return(cleaned_data)
})

# Combine the two datasets into one panel
panel_data <- bind_rows(processed_data)

# Save the panel dataset
panel_file <- "/Users/jaredblack/GitHub/ElectionGPT/data/overall/panel_election_results_state.csv"
write_csv(panel_data, panel_file)

cat("Panel dataset created and saved as:", panel_file, "\n")
cat("Total rows in panel dataset:", nrow(panel_data), "\n")