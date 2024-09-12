# Load required libraries
library(httr)
library(jsonlite)

# Set API key
news_api_key <- "be2a99fb-1359-494c-a526-1d45a1ade146"

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

# Directories to save the JSON files
save_directory_1 <- "/Users/jaredblack/KamalaGPT/news_data/election_news"
save_directory_2 <- "/Users/jaredblack/GitHub/ElectionGPT/data/news"  # New save location

# Create the directories if they don't exist
for (dir in c(save_directory_1, save_directory_2)) {
  if (!dir.exists(dir)) {
    dir.create(dir, recursive = TRUE)
  }
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
  
  # Save to both locations
  for (dir in c(save_directory_1, save_directory_2)) {
    json_filepath <- file.path(dir, json_filename)
    write_json(news_data, json_filepath)
    cat("News data fetched and saved as:", json_filepath, "\n")
  }
} else {
  cat("Failed to fetch news data.\n")
}