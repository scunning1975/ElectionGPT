# Load required libraries
library(httr)
library(jsonlite)

# Set API key
news_api_key <- "7200c2f7-76db-4899-9328-ef6ac9c62b6d"

# Base URL for Perigon API
news_api_url <- "https://api.goperigon.com/v1/all"

# Parameters for the news API request
news_params <- list(
  q = "2024 US presidential election",
  language = "en",
  sortBy = "date",
  limit = 100,
  articleBodyLen = 50, 
  showReprints = FALSE
)

# Directory to save the JSON files
save_directory <- "/Users/jaredblack/GitHub/ElectionGPT/data/news"

# Create the directory if it doesn't exist
if (!dir.exists(save_directory)) {
  dir.create(save_directory, recursive = TRUE)
}

# Function to fetch news data
fetch_news_data <- function(url, params, api_key) {
  response <- GET(url, 
                  query = params, 
                  add_headers("x-api-key" = api_key,
                              "x-include-sentiment" = "true",
                              "x-include-entity-sentiment" = "true",
                              "x-include-summary" = "true",
                              "x-return-relevance-score" = "true"))
  
  if (status_code(response) == 200) {
    news_data <- content(response, "text")
    parsed_data <- fromJSON(news_data)
    return(parsed_data$articles)
  } else {
    cat("Failed to fetch news data. Status code:", status_code(response), "\n")
    cat("Response content:", content(response, "text"), "\n")
    return(NULL)
  }
}

# Fetch news data
news_data <- fetch_news_data(news_api_url, news_params, news_api_key)

# Check if news data was successfully fetched
if (!is.null(news_data)) {
  # Generate a timestamp for the filename
  timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
  
  # Save the news data as JSON
  filename <- file.path(save_directory, paste0("perigon_news_data_", timestamp, ".json"))
  write_json(news_data, filename)
  cat("News data saved to:", filename, "\n")
} else {
  cat("No news data to save.\n")
}