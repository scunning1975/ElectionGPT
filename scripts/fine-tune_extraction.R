# Load required libraries
library(httr)
library(jsonlite)
library(readr)
library(stringr)
library(dplyr)
library(purrr)
library(tidyr)  # Added for pivot_wider
library(writexl)

# Set API key for OpenAI
openai_api_key <- "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"

# Function to extract winners using GPT API
extract_winners_gpt <- function(story) {
  prompt <- paste0(
    "Extract the winner for each state in the 2024 US presidential election from the following story. ",
    "Output should be in CSV format with columns: State, Winner (either 'Trump' or 'Harris'). ",
    "Include all 50 states plus District of Columbia if mentioned. If a state is not mentioned, exclude it from the output.\n\n",
    story
  )
  
  body <- list(
    model = "gpt-3.5-turbo",
    messages = list(
      list(role = "system", content = "You are a helpful assistant that extracts election results."),
      list(role = "user", content = prompt)
    ),
    temperature = 0.0,
    max_tokens = 1000
  )
  
  response <- POST(
    url = "https://api.openai.com/v1/chat/completions",
    body = toJSON(body, auto_unbox = TRUE),
    add_headers(
      "Content-Type" = "application/json",
      "Authorization" = paste("Bearer", openai_api_key)
    ),
    encode = "json"
  )
  
  if (status_code(response) == 200) {
    content <- content(response, "parsed")
    csv_content <- content$choices[[1]]$message$content
    return(read.csv(text = csv_content, stringsAsFactors = FALSE))
  } else {
    cat("Error in API call:", content(response, "text"), "\n")
    return(NULL)
  }
}

process_results <- function(results) {
  all_states <- c(state.abb, "DC")
  
  # Convert results to 1s and 0s
  results$Winner <- ifelse(results$Winner == "Trump", 1, 0)
  
  # Add DC with 0 if it's missing
  if (!"District of Columbia" %in% results$State) {
    results <- rbind(results, data.frame(State = "District of Columbia", Winner = 0))
  }
  
  # Create a mapping of state names to abbreviations
  state_mapping <- setNames(c(state.abb, "DC"), c(state.name, "District of Columbia"))
  
  # Convert state names to abbreviations
  results$State <- state_mapping[results$State]
  
  # Ensure all states are present, fill missing with NA
  missing_states <- setdiff(all_states, results$State)
  if (length(missing_states) > 0) {
    results <- rbind(results, data.frame(State = missing_states, Winner = NA))
  }
  
  # Convert to wide format
  results_wide <- results %>%
    tidyr::pivot_wider(names_from = State, values_from = Winner) %>%
    select(all_of(all_states))
  
  return(results_wide)
}

# Read the file
file_path <- "/Users/jaredblack/GitHub/ElectionGPT/data/stories/fine-tuned_story_direct_2024-09-09_20-00-13.txt"
content <- read_file(file_path)

# Split content into individual stories
stories <- str_split(content, "Story \\d+:")[[1]][-1]  # Remove the first empty element

# Extract winners for each story using GPT API and process results
results <- map_dfr(stories, function(story) {
  winners <- extract_winners_gpt(story)
  if (!is.null(winners)) {
    process_results(winners)
  } else {
    NULL
  }
}, .id = "Trial")

# Extract winners for each story using GPT API and process results
results <- map_dfr(stories, function(story) {
  winners <- extract_winners_gpt(story)
  if (!is.null(winners)) {
    process_results(winners)
  } else {
    NULL
  }
}, .id = "Trial")

# Convert Trial to numeric and sort
results <- results %>%
  mutate(Trial = as.numeric(Trial)) %>%
  arrange(Trial)

# Ensure all values are 0, 1, or NA
results <- results %>%
  mutate(across(-Trial, ~if_else(. %in% c(0, 1), ., NA_real_)))

# Reorder columns to match state.abb order, and add DC at the end
state_order <- c(state.abb, "DC")
results <- results %>%
  select(Trial, all_of(state_order))

# Set the directory path
dir_path <- "/Users/jaredblack/GitHub/ElectionGPT/data"

# Create the directory if it doesn't exist
if (!dir.exists(dir_path)) {
  dir.create(dir_path, recursive = TRUE)
}

# Save as CSV
write.csv(results, file.path(dir_path, "fine-tuned_direct_election_results.csv"), row.names = FALSE)

# Save as Excel
write_xlsx(results, file.path(dir_path, "fine-tuned_direct_election_results.xlsx"))

cat("Results extracted and saved in", dir_path, "as '_election_results.csv' and '_election_results.xlsx'\n")