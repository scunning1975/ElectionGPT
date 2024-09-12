# Load required libraries
library(httr)
library(jsonlite)
library(lubridate)

# Set API key
news_api_key <- "be2a99fb-1359-494c-a526-1d45a1ade146"

# Base URL for Event Registry API
news_api_url <- "https://eventregistry.org/api/v1/article/getArticles"

# Calculate date range for the last 30 days
end_date <- Sys.Date()
start_date <- end_date - days(30)

# Parameters for the news API request
news_params <- list(
  apiKey = news_api_key,
  keyword = "2024 US presidential election",
  lang = "eng",
  sortBy = "date",
  dateStart = format(start_date, "%Y-%m-%d"),
  dateEnd = format(end_date, "%Y-%m-%d"),
  maxArticles = 100,  # We'll use this as our batch size
  articleBodyLen = 200
)

# Directories to save the JSON files
save_directory_1 <- "/Users/jaredblack/KamalaGPT/news_data/election_news"
save_directory_2 <- "/Users/jaredblack/GitHub/ElectionGPT/data/news"

# Create the directories if they don't exist
for (dir in c(save_directory_1, save_directory_2)) {
  if (!dir.exists(dir)) {
    dir.create(dir, recursive = TRUE)
  }
}

# Function to fetch news data with pagination
fetch_news_data <- function(url, params, max_articles = 10000) {
  all_articles <- list()
  total_articles <- 0
  
  while (total_articles < max_articles) {
    response <- GET(url, query = params)
    
    if (status_code(response) == 200) {
      news_data <- content(response, "parsed")
      batch_articles <- news_data$articles$results
      
      if (length(batch_articles) == 0) {
        break  # No more articles to fetch
      }
      
      all_articles <- c(all_articles, batch_articles)
      total_articles <- total_articles + length(batch_articles)
      
      # Update the lastArticleId for the next batch
      params$lastArticleId <- batch_articles[[length(batch_articles)]]$id
      
      cat("Fetched", length(batch_articles), "articles. Total:", total_articles, "\n")
      
      # Add a small delay to avoid hitting rate limits
      Sys.sleep(1)
    } else {
      cat("Failed to fetch news data:", status_code(response), "\n")
      break
    }
  }
  
  return(all_articles)
}

# Fetch news data
news_data <- fetch_news_data(news_api_url, news_params)

# Check if news data is fetched successfully
if (length(news_data) > 0) {
  # Save the news data as a JSON file with a date-stamped filename
  timestamp <- format(Sys.time(), "%Y-%m-%d_%H-%M-%S")
  json_filename <- paste0("news_data_", timestamp, ".json")
  
  # Save to both locations
  for (dir in c(save_directory_1, save_directory_2)) {
    json_filepath <- file.path(dir, json_filename)
    write_json(news_data, json_filepath)
    cat("News data fetched and saved as:", json_filepath, "\n")
  }
  
  cat("Total articles fetched:", length(news_data), "\n")
} else {
  cat("Failed to fetch news data or no articles found.\n")
}