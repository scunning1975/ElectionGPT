# Load required libraries
library(httr)
library(jsonlite)
library(dotenv) # access environment variables

# Load the .env file
dotenv::load_dot_env(file = "keys.env")

# Set API key from environment variable
news_api_key <- Sys.getenv("PERIGON_API_KEY")

# Base URL for Perigon API
news_api_url <- "https://api.goperigon.com/v1/all"

# Directory to save the JSON files (relative path)
save_directory <- "data/news"

# Create the directory if it doesn't exist
if (!dir.exists(save_directory)) {
  dir.create(save_directory, recursive = TRUE)
}

# Function to fetch news data with pagination
fetch_news_data <- function(url, params, api_key, total_articles = 100) {
  all_articles <- list()
  page_number <- 1
  articles_fetched <- 0
  per_page <- 20  # Maximum allowed per page by the API
  
  while (articles_fetched < total_articles) {
    params$page <- page_number
    params$perPage <- per_page
    
    response <- GET(url, 
                    query = params, 
                    add_headers("x-api-key" = api_key,
                                "x-include-sentiment" = "true",
                                "x-include-entity-sentiment" = "true",
                                "x-include-summary" = "true",
                                "x-return-relevance-score" = "true",
                                "x-include-nlp" = "true",
                                "x-include-source-rankings" = "true",
                                "x-include-similar-articles" = "true",
                                "x-include-source-domain-rankings" = "true"))
    
    if (status_code(response) == 200) {
      news_data <- content(response, "text", encoding = "UTF-8")
      parsed_data <- fromJSON(news_data)
      
      articles <- parsed_data$articles
      
      if (length(articles) == 0) {
        break  # No more articles to fetch
      }
      
      # Convert articles data frame to list of lists if necessary
      if (is.data.frame(articles)) {
        articles <- split(articles, seq(nrow(articles)))
      }
      
      # Append articles to all_articles
      all_articles <- c(all_articles, articles)
      articles_fetched <- length(all_articles)
      page_number <- page_number + 1
    } else {
      cat("Failed to fetch news data. Status code:", status_code(response), "\n")
      cat("Response content:", content(response, "text", encoding = "UTF-8"), "\n")
      break
    }
  }
  
  # Trim the list to the desired number of articles
  all_articles <- all_articles[1:min(total_articles, length(all_articles))]
  return(all_articles)
}

# Parameters for the news API request
news_params <- list(
  q = "2024 US presidential election",
  language = "en",
  sort = "date",
  showReprints = "false"
)

# Fetch news data
news_data <- fetch_news_data(news_api_url, news_params, news_api_key, total_articles = 100)

# Check if news data was successfully fetched
if (!is.null(news_data) && length(news_data) > 0) {
  # Remove duplicates based on 'url' (assuming 'url' is unique for each article)
  urls <- sapply(news_data, function(article) article$url)
  unique_indices <- !duplicated(urls)
  news_data_unique <- news_data[unique_indices]
  
  # Extract only the required fields from each article, including the title
  processed_articles <- lapply(news_data_unique, function(article) {
    list(
      title = ifelse(is.null(article$title), "", article$title),
      summary = ifelse(is.null(article$summary), "", article$summary),
      source = ifelse(is.null(article$source$name), "", article$source$name),
      date = ifelse(is.null(article$pubDate), "", article$pubDate),
      sentiment = ifelse(is.null(article$sentiment$score), NA, article$sentiment$score)
    )
  })
  
  # Print the number of distinct articles
  cat("Number of distinct articles fetched:", length(processed_articles), "\n")
  
  # Generate a timestamp for the filename
  timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
  
  # Save the processed articles as JSON
  filename <- file.path(save_directory, paste0("perigon_news_data_", timestamp, ".json"))
  write_json(processed_articles, filename, auto_unbox = TRUE)
  cat("Processed news data saved to:", filename, "\n")
} else {
  cat("No news data to save.\n")
}