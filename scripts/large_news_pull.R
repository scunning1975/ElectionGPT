# Load required libraries
library(httr)
library(jsonlite)
library(dotenv)
library(dplyr)
library(stringr)

# Load the .env file
dotenv::load_dot_env(file = "/Users/jaredblack/GitHub/ElectionGPT/keys.env")

# Set API key
news_api_key <- Sys.getenv("EVENT_REGISTRY_API_KEY")

# Base URL for Event Registry API
news_api_url <- "https://eventregistry.org/api/v1/article/getArticles"

# Directory to save the JSON files
save_directory <- "/Users/jaredblack/GitHub/ElectionGPT/data/news"

# Create the directory if it doesn't exist
if (!dir.exists(save_directory)) {
  dir.create(save_directory, recursive = TRUE)
}

# Define the %||% operator if not already defined
`%||%` <- function(a, b) if (!is.null(a)) a else b

# Function to fetch news data with pagination
fetch_news_data <- function(url, params, totalArticles) {
  all_articles <- list()
  articles_fetched <- 0
  page <- 1
  articlesPerPage <- 100  # Number of articles per page (adjust as per API limits)
  
  while (articles_fetched < totalArticles) {
    params$articlesPage <- page
    params$articlesCount <- min(articlesPerPage, totalArticles - articles_fetched)
    
    response <- GET(url, query = params)
    if (status_code(response) == 200) {
      news_data <- content(response, "parsed")
      articles <- news_data$articles$results
      
      if (length(articles) == 0) {
        break  # No more articles to fetch
      }
      
      all_articles <- c(all_articles, articles)
      articles_fetched <- length(all_articles)
      cat("Fetched", articles_fetched, "articles so far...\n")
      
      page <- page + 1
    } else {
      cat("Failed to fetch news data:", status_code(response), "\n")
      break
    }
    
    Sys.sleep(1)  # Pause to avoid hitting rate limits
  }
  
  return(all_articles)
}

# Parameters for the news API request
news_params <- list(
  apiKey = news_api_key,
  keyword = "2024 US presidential election",
  lang = "eng",
  sortBy = "date",
  articleBodyLen = -1,               # -1 to fetch the full article body
  articleIncludeArticleBody = 'true' # Include the full article body
)

# Total number of articles you want to fetch
totalArticlesToFetch <- 500  # Adjust this number as needed

# Fetch news data
news_data <- fetch_news_data(news_api_url, news_params, totalArticlesToFetch)

# Function to remove irrelevant content from an article body
remove_irrelevant_content <- function(text) {
  if (is.na(text) || text == "") {
    return(text)
  }
  
  # Define patterns to remove
  patterns <- c(
    "By clicking.*?Privacy Policy\\.",          # Terms and conditions disclaimers
    "Read more:.*?(\\n|$)",                     # "Read more" sections
    "More from .*?(\\n|$)",                     # "More from" sections
    "Original article source:.*?(\\n|$)",       # "Original article source" lines
    "Read On The.*?(\\n|$)",                    # "Read On The..." lines
    ".*?\\bApp\\b.*?(\\n|$)",                   # Lines containing the word "App"
    ".*?Credit:.*?(\\n|$)",                     # Lines starting with "Credit:"
    ".*?Image:.*?(\\n|$)",                      # Lines starting with "Image:"
    "Follow us on.*?(\\n|$)",                   # Social media prompts
    "Sign up for.*?(\\n|$)",                    # Sign-up prompts
    "Get the latest.*?(\\n|$)",                 # Promotional lines
    "Copyright.*",                              # Copyright notices
    "All rights reserved.*",                    # "All rights reserved" and after
    ".*?advertisement.*?",                      # Lines containing "advertisement"
    ".*?ADVERTISEMENT.*?",                      # Lines containing "ADVERTISEMENT"
    ".*?Sponsored Content.*?",                  # Lines containing "Sponsored Content"
    ".*?This article originally appeared on.*?",# Lines indicating original publication
    "Related Articles.*",                       # "Related Articles" and everything after
    ".*?Click here to read more.*?(\\n|$)",     # "Click here to read more" lines
    "\\[.*?\\]",                                # Text within brackets
    "<[^>]+>",                                  # HTML tags
    "\\n\\s*\\n",                               # Multiple empty lines
    "^\\s*$"                                    # Empty lines
  )
  
  # Combine patterns into one regular expression
  combined_pattern <- paste(patterns, collapse = "|")
  
  # Remove the patterns
  cleaned_text <- gsub(combined_pattern, "", text, perl = TRUE, ignore.case = TRUE)
  
  # Remove multiple consecutive newlines
  cleaned_text <- gsub("\\n{2,}", "\n", cleaned_text)
  
  # Trim whitespace
  cleaned_text <- str_trim(cleaned_text)
  
  return(cleaned_text)
}

# Flatten and clean articles
flatten_and_clean_articles <- function(articles) {
  # Function to flatten and clean a single article
  process_article <- function(article) {
    # Extract relevant fields
    title <- article$title %||% NA
    body <- article$body %||% NA
    sentiment <- article$sentiment %||% NA
    wgt <- article$wgt %||% NA  # Extract the weight field
    source_name <- article$source$title %||% NA  # Extract the source name
    
    # Remove irrelevant content from the body
    body <- remove_irrelevant_content(body)
    
    # Return a data frame row
    data.frame(
      title = title,
      content = body,
      sentiment = sentiment,
      weight = wgt,
      source = source_name,
      stringsAsFactors = FALSE
    )
  }
  
  # Process all articles
  articles_df <- bind_rows(lapply(articles, process_article))
  
  # Remove duplicates based on title and content
  articles_df <- articles_df %>% distinct(title, content, .keep_all = TRUE)
  
  # Remove articles with empty content
  articles_df <- articles_df %>% filter(!is.na(content) & content != "")
  
  return(articles_df)
}

# Flatten and clean the articles
news_df <- flatten_and_clean_articles(news_data)

# Remove articles with content length less than a threshold (optional)
min_content_length <- 200  # Set a threshold for minimum content length
news_df <- news_df %>%
  mutate(content_length = nchar(content)) %>%
  filter(content_length >= min_content_length)

# Remove the content_length column as it's no longer needed
news_df <- news_df %>% select(-content_length)

# Save the news data as a JSON file with a date-stamped filename
timestamp <- format(Sys.time(), "%Y-%m-%d_%H-%M-%S")
json_filename <- paste0("news_data_cleaned_", timestamp, ".json")

# Save to the directory
json_filepath <- file.path(save_directory, json_filename)

# Save the cleaned data with pretty formatting
write_json(news_df, json_filepath, pretty = TRUE, auto_unbox = TRUE)
cat("Cleaned news data saved as:", json_filepath, "\n")

# Assess the output file size
file_size <- file.info(json_filepath)$size
cat("Cleaned output file size:", format(file_size, units = "auto"), "\n")