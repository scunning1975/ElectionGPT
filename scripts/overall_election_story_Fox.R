# Load required libraries
library(httr)
library(jsonlite)
library(stringr)
library(dplyr)
library(openxlsx)
library(dotenv) # Access environment variables

# Load the .env file (update the path to your keys.env file)
dotenv::load_dot_env(file = "/Users/jaredblack/GitHub/ElectionGPT/keys.env")

# Set API key for OpenAI from environment variable
openai_api_key <- Sys.getenv("OVERALL_FOX_KEY")

# Function to load the specified news file
load_news_file <- function(filepath) {
  news_content <- fromJSON(filepath)
  return(news_content)
}

# Load the news file (update the path to your news data file)
news_filepath <- "/Users/jaredblack/GitHub/ElectionGPT/data/news/news_data_cleaned_2024-10-16_11-39-32.json"
news_content <- load_news_file(news_filepath)

# Total number of news stories loaded
total_news_stories <- nrow(news_content)
cat("Total number of news stories loaded:", total_news_stories, "\n")

# Function to estimate tokens based on characters
estimate_tokens <- function(text) {
  num_chars <- nchar(text)
  num_tokens <- num_chars / 4  # Approximate 4 characters per token
  return(ceiling(num_tokens))  # Round up to be safe
}

# Calculate the maximum characters for news content
context_window <- 128000  # Total context window in tokens
max_response_tokens <- 3000  # Tokens reserved for the response
prompt_tokens <- 100  # Estimated tokens used by the prompt messages

available_tokens_for_news <- context_window - prompt_tokens - max_response_tokens
max_chars_for_news <- available_tokens_for_news * 4  # Approximate characters per token

# Combine news content until it reaches the maximum allowed characters
accumulated_chars <- 0
news_content_used <- c()
used_indices <- c()  # Keep track of indices of articles used

for (i in 1:nrow(news_content)) {
  article_content <- news_content$content[i]
  article_length <- nchar(article_content)
  
  if ((accumulated_chars + article_length) <= max_chars_for_news) {
    news_content_used <- c(news_content_used, article_content)
    accumulated_chars <- accumulated_chars + article_length
    used_indices <- c(used_indices, i)  # Record the index of the article used
  } else {
    break  # Stop adding more articles when limit is reached
  }
}

# Number of news stories actually used
num_news_stories_used <- length(news_content_used)
cat("Number of news stories used:", num_news_stories_used, "\n")

# Combine the used news content into one string
news_content_text <- paste(news_content_used, collapse = " ")

# Total characters and estimated tokens in news content used
total_chars_used <- nchar(news_content_text)
estimated_tokens_used <- estimate_tokens(news_content_text)

cat("Total characters in news content used:", total_chars_used, "\n")
cat("Estimated tokens in news content used:", estimated_tokens_used, "\n")

# Extract the exact news articles used
news_articles_used <- news_content[used_indices, ]

# Save the exact news articles used to a JSON file
timestamp <- format(Sys.time(), "%Y-%m-%d_%H-%M-%S")
news_used_filename <- paste0("news_used_", timestamp, ".json")
news_used_filepath <- file.path("/Users/jaredblack/GitHub/ElectionGPT/data/news", news_used_filename)
write_json(news_articles_used, news_used_filepath, pretty = TRUE, auto_unbox = TRUE)
cat("News articles used saved as:", news_used_filepath, "\n")

# Define the prompt
prompt <- "Given the recent news articles provided, generate a short news story from the perspective of Fox News reporter Bret Baier about the outcome of the 2024 US presidential election between Donald Trump and Kamala Harris. The story must explicitly state the winner in each of the 50 states. Include a specific detail or quote pulled directly from the news articles."

# Function to generate the story using GPT-4 with exponential backoff
generate_story <- function(api_key, prompt, news_content) {
  max_retries <- 5
  retry_delay <- 1  # Initial delay in seconds
  for (attempt in 1:max_retries) {
    gpt_api_url <- "https://api.openai.com/v1/chat/completions"
    body <- list(
      model = "gpt-4o-mini",
      messages = list(
        list(role = "system", content = "You are Bret Baier, a Fox News reporter."),
        list(role = "user", content = paste(prompt, "\n\n", news_content))
      ),
      temperature = 0.7,
      max_tokens = 3000
    )
    response <- POST(
      gpt_api_url,
      body = toJSON(body, auto_unbox = TRUE),
      encode = "json",
      add_headers(
        `Authorization` = paste("Bearer", api_key),
        `Content-Type` = "application/json"
      )
    )
    if (status_code(response) == 200) {
      return(content(response, "parsed")$choices[[1]]$message$content)
    } else if (status_code(response) %in% c(429, 500, 502, 503, 504)) {
      # Handle rate limit and server errors with exponential backoff
      cat("API error (status code:", status_code(response), ") on attempt", attempt, "\n")
      if (attempt < max_retries) {
        Sys.sleep(retry_delay)
        retry_delay <- retry_delay * 2  # Exponential backoff
        next
      } else {
        cat("Max retries reached. Failed to generate story.\n")
        return(NULL)
      }
    } else {
      cat("Failed to generate story:", status_code(response), "\n")
      return(NULL)
    }
  }
}

# Function to extract the winner for each state from the story and convert to 1 or 0 with exponential backoff
extract_winners <- function(api_key, story) {
  max_retries <- 5
  retry_delay <- 1  # Initial delay in seconds
  for (attempt in 1:max_retries) {
    extract_prompt <- paste(
      "Extract the winner for each state in the 2024 US presidential election from the following story. ",
      "The output should be in CSV format with columns: State, Winner (either 'Trump' or 'Harris').\n\n",
      story
    )
    
    gpt_api_url <- "https://api.openai.com/v1/chat/completions"
    body <- list(
      model = "gpt-3.5-turbo",
      messages = list(
        list(role = "system", content = "You are a trustworthy independent reporter."),
        list(role = "user", content = extract_prompt)
      ),
      temperature = 0.0,
      max_tokens = 1500
    )
    response <- POST(gpt_api_url, body = toJSON(body, auto_unbox = TRUE), encode = "json",
                     add_headers(`Authorization` = paste("Bearer", api_key), `Content-Type` = "application/json"))
    if (status_code(response) == 200) {
      csv_content <- content(response, "parsed")$choices[[1]]$message$content
      cleaned_csv <- gsub("```", "", csv_content)
      
      # Safely attempt to read the CSV content
      tryCatch({
        trial_results <- read.csv(text = cleaned_csv, stringsAsFactors = FALSE)
        
        # Ensure the results are in the correct format
        if (ncol(trial_results) == 2 && all(c("State", "Winner") %in% colnames(trial_results))) {
          # Convert 'Trump' to 1 and 'Harris' to 0
          trial_results$Winner <- ifelse(trial_results$Winner == "Trump", 1, 0)
          return(trial_results)
        } else {
          cat("Warning: Unexpected format in CSV content\n")
          return(NULL)
        }
      }, error = function(e) {
        cat("Error reading CSV content: ", e$message, "\n")
        return(NULL)
      })
    } else if (status_code(response) %in% c(429, 500, 502, 503, 504)) {
      # Handle rate limit and server errors with exponential backoff
      cat("API error (status code:", status_code(response), ") on attempt", attempt, "\n")
      if (attempt < max_retries) {
        Sys.sleep(retry_delay)
        retry_delay <- retry_delay * 2  # Exponential backoff
        next
      } else {
        cat("Max retries reached. Failed to extract winners.\n")
        return(NULL)
      }
    } else {
      cat("Failed to extract winners:", status_code(response), "\n")
      return(NULL)
    }
  }
}

# Prepare to save the stories
test_story_filename <- paste0("story_overall_FOX_", timestamp, ".txt")  # Added underscore after 'FOX' for consistency
test_story_filepath <- file.path("/Users/jaredblack/GitHub/ElectionGPT/data/stories", test_story_filename)
fileConn <- file(test_story_filepath, "w")

# Write the prompt at the beginning of the file
writeLines(paste0("prompt <- \"", prompt, "\"\n\n-----------------------\n\n"), fileConn)

# Initialize the results data frame with columns: Trial, and state abbreviations
results <- data.frame(matrix(ncol = length(state.abb) + 1, nrow = 0))
colnames(results) <- c("Trial", state.abb)

# Number of trials
num_trials <- 1000  # Set the number of stories to generate

# Start the loop
for (i in 1:num_trials) {
  cat("Generating story", i, "\n")
  story <- generate_story(openai_api_key, prompt, news_content_text)
  
  if (!is.null(story)) {
    cat("Story", i, "generated successfully.\n")
    # Save the story to the file
    writeLines(paste0("Story ", i, ":\n", story, "\n\n----------------------------------\n\n"), fileConn)
    
    # Extract the state winners from the story
    winners_csv <- extract_winners(openai_api_key, story)
    
    if (!is.null(winners_csv)) {
      # Attempt to transpose and bind the results, with error handling
      tryCatch({
        # Clean and standardize state names
        winners_csv$State <- str_trim(winners_csv$State)
        winners_csv$State <- str_to_title(winners_csv$State)
        
        # Ensure state names in winners_csv match state.abb
        winners_csv$Abbreviation <- state.abb[match(winners_csv$State, state.name)]
        
        # Initialize a vector for this trial's results with NA
        trial_vector <- rep(NA, length(state.abb))
        names(trial_vector) <- state.abb
        
        # Fill in the results for the states present
        trial_vector[winners_csv$Abbreviation] <- winners_csv$Winner
        
        # Create a data frame for this trial
        trial_results <- data.frame(t(trial_vector))
        trial_results$Trial <- paste("Trial", i)
        
        # Reorder columns to have 'Trial' first
        trial_results <- trial_results[, c("Trial", state.abb)]
        
        # Bind the results
        results <- rbind(results, trial_results)
      }, error = function(e) {
        cat("Error processing trial results for story", i, ":", e$message, "\n")
        print(winners_csv)
      })
    } else {
      cat("Failed to extract winners for story", i, "\n")
    }
  } else {
    cat("Failed to generate story", i, "\n")
  }
  
  # Add a delay to avoid hitting rate limits
  Sys.sleep(5)
}

# Close the file connection after the loop
close(fileConn)

# Save the results to an Excel file
results_filename <- paste0("election_results_FOX_", timestamp, ".xlsx")
results_filepath <- file.path("/Users/jaredblack/GitHub/ElectionGPT/data/raw", results_filename)
write.xlsx(results, results_filepath, rowNames = FALSE)
cat("Election results saved as:", results_filepath, "\n")
cat("All stories saved as:", test_story_filepath, "\n")