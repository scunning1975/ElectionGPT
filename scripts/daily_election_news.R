# Load required libraries
library(httr)
library(jsonlite)
library(dotenv) # access environment variables

# Load the .env file
dotenv::load_dot_env(file = "/Users/jaredblack/GitHub/ElectionGPT/keys.env")

# Set API key
news_api_key <- Sys.getenv("EVENT_REGISTRY_API_KEY")

# Base URL for Event Registry API
news_api_url <- "https://eventregistry.org/api/v1/article/getArticles"

# Parameters for the news API request
news_params <- list(
  apiKey = news_api_key,
  keyword = "2024 US presidential election",
  lang = "eng",
  sortBy = "date",
  maxArticles = 100,
  articleBodyLen = 200  # This parameter sets the length of the article body to a shorter summary
)

# Directory to save the JSON files
save_directory <- "/Users/jaredblack/GitHub/ElectionGPT/data/news"  # New save location

# Create the directory if it doesn't exist
if (!dir.exists(save_directory)) {
  dir.create(save_directory, recursive = TRUE)
}

# Function to fetch news data
fetch_news_data <- function(url, params) {
  response <- GET(url, query = params)
  if (status_code(response) == 200) {
    news_data <- content(response, "parsed")
    return(news_data$articles)
  } else {
    cat("Failed to fetch news data:", status_code(response), "\n")
    return(NULL)
  }
}

# Fetch news data
news_data <- fetch_news_data(news_api_url, news_params)

# Check if news data is fetched successfully
if (!is.null(news_data)) {
  # Print the structure of the fetched data
  str(news_data)
  
  # Save the news data as a JSON file with a date-stamped filename
  timestamp <- format(Sys.time(), "%Y-%m-%d_%H-%M-%S")
  json_filename <- paste0("news_data_", timestamp, ".json")
  
  # Save to the directory
  json_filepath <- file.path(save_directory, json_filename)
  write_json(news_data, json_filepath)
  cat("News data fetched and saved as:", json_filepath, "\n")
} else {
  cat("Failed to fetch news data.\n")
}