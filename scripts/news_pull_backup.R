# Load required libraries
library(httr)
library(jsonlite)
library(dotenv)

# Load the .env file
dotenv::load_dot_env(file = "/Users/jaredblack/GitHub/ElectionGPT/keys.env")

# Set API key
news_api_key <- Sys.getenv("EVENT_REGISTRY_API_KEY")

# Base URL for Event Registry API
news_api_url <- "https://eventregistry.org/api/v1/article/getArticles"

# Function to fetch news data with size control
fetch_news_data <- function(url, params) {
  response <- GET(url, query = params)
  if (status_code(response) == 200) {
    news_data <- content(response, "parsed")
    
    # Debug print to see structure
    cat("Response structure:\n")
    str(news_data)
    
    # Check if we're getting the articles in the expected format
    if (!is.null(news_data$articles$results)) {
      articles <- news_data$articles$results
    } else if (!is.null(news_data$articles)) {
      articles <- news_data$articles
    } else {
      cat("Unexpected response structure\n")
      return(NULL)
    }
    
    # Ensure we only take the first 100 articles
    if (length(articles) > 100) {
      articles <- articles[1:100]
    }
    
    # Verify the size of the data
    json_size <- length(toJSON(articles))
    cat("JSON size before save:", json_size, "bytes\n")
    
    return(articles)
  } else {
    cat("Failed to fetch news data:", status_code(response), "\n")
    return(NULL)
  }
}

# Parameters for the news API request
news_params <- list(
  apiKey = news_api_key,
  keyword = "2024 US presidential election",
  lang = "eng",
  sortBy = "date",
  sortByAsc = FALSE,
  maxArticles = 100,
  articleBodyLen = 200,
  returnInfo = "id,title,body,date,url",
  dateEnd = "2024-10-26",     # Simple date format
  dateStart = "2024-09-26"    # Optional, if we want just last day
)

# Directory to save the JSON files
save_directory <- "/Users/jaredblack/GitHub/ElectionGPT/data/news"

# Create the directory if it doesn't exist
if (!dir.exists(save_directory)) {
  dir.create(save_directory, recursive = TRUE)
}

# Fetch news data
cat("Fetching news data...\n")
news_data <- fetch_news_data(news_api_url, news_params)

# Function to trim fields from articles to reduce size
trim_articles <- function(articles) {
  lapply(articles, function(article) {
    list(
      title = article$title,
      body = substr(article$body, 1, 200),  # Ensure body is limited to 200 chars
      date = article$date,
      url = article$url
    )
  })
}

# Check if news data is fetched successfully
if (!is.null(news_data)) {
  # Trim the articles to reduce size
  trimmed_data <- trim_articles(news_data)
  
  # Save the news data as a JSON file with a date-stamped filename
  timestamp <- format(Sys.time(), "%Y-%m-%d_%H-%M-%S")
  json_filename <- paste0("news_data_", timestamp, ".json")
  
  # Save to the directory
  json_filepath <- file.path(save_directory, json_filename)
  
  # Convert to JSON with minimal whitespace
  json_data <- toJSON(trimmed_data, auto_unbox = TRUE)
  write(json_data, json_filepath)
  
  # Check final file size
  file_size <- file.size(json_filepath)
  cat("File saved as:", json_filepath, "\n")
  cat("File size:", file_size, "bytes\n")
} else {
  cat("Failed to fetch news data.\n")
}