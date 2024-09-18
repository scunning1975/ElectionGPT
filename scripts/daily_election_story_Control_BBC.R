# Load required libraries
library(httr)
library(jsonlite)
library(stringr)
library(dplyr)
library(openxlsx)
library(dotenv) # Load environment variables

# Load the .env file
dotenv::load_dot_env(file = "/Users/jaredblack/GitHub/ElectionGPT/keys.env")

# Set API key for OpenAI from environment variable
openai_api_key <- Sys.getenv("CONTROL_BBC")

# Define the prompt
prompt <- "Generate a short news story from the perspective of BBC reporter Laura Kuenssberg about the outcome of the 2024 US presidential election between Donald Trump and Kamala Harris. The story must explicitly state the winner in each of the 50 states."

# Function to generate the story using GPT-4
generate_story <- function(api_key, prompt) {
  gpt_api_url <- "https://api.openai.com/v1/chat/completions"
  body <- list(
    model = "gpt-4o-mini",
    messages = list(
      list(role = "system", content = "You are Laura Kuenssberg, a BBC reporter."),
      list(role = "user", content = prompt)
    ),
    temperature = 0.75,
    max_tokens = 3000
  )
  
  tryCatch({
    response <- POST(gpt_api_url, 
                     body = toJSON(body, auto_unbox = TRUE), 
                     encode = "json",
                     add_headers(`Authorization` = paste("Bearer", api_key), 
                                 `Content-Type` = "application/json"))
    
    # Print response status and content for debugging
    cat("Response status:", status_code(response), "\n")
    cat("Response content:", content(response, "text"), "\n")
    
    if (status_code(response) == 200) {
      parsed_content <- content(response, "parsed")
      if (!is.null(parsed_content$choices) && length(parsed_content$choices) > 0) {
        return(parsed_content$choices[[1]]$message$content)
      } else {
        cat("Error: Unexpected response structure\n")
        return(NULL)
      }
    } else {
      cat("Failed to generate story. Status code:", status_code(response), "\n")
      return(NULL)
    }
  }, error = function(e) {
    cat("Error in API call:", e$message, "\n")
    return(NULL)
  })
}

# Function to extract the winner for each state from the story and convert to 1 or 0
extract_winners <- function(api_key, story) {
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
  } else {
    cat("Failed to extract winners:", status_code(response), "\n")
    return(NULL)
  }
}

# Prepare the combined story text file
timestamp <- format(Sys.time(), "%Y-%m-%d_%H-%M-%S")
combined_story_filename <- paste0("story_Control_BBC_", timestamp, ".txt")
combined_story_filepath <- file.path("/Users/jaredblack/GitHub/ElectionGPT/data/stories", combined_story_filename)
fileConn <- file(combined_story_filepath, "w")  # Initialize the file connection

# Write the prompt at the beginning of the file
writeLines(paste0("prompt <- \"", prompt, "\"\n\n-----------------------\n\n"), fileConn)

# Initialize the results data frame
results <- data.frame(matrix(ncol = 51, nrow = 0))
colnames(results) <- c("Trial", state.abb)

# Start the loop
for (i in 1:100) {
  cat("Generating story", i, "\n")
  story <- generate_story(openai_api_key, prompt)
  
  if (is.null(story)) {
    cat("Failed to generate story for trial", i, ". Skipping to next iteration.\n")
    next
  }
  
  # Append the story to the combined text file
  writeLines(paste0("Story ", i, ":\n", story, "\n\n----------------------------------\n\n"), fileConn)
  
  # Extract the state winners from the story
  winners_csv <- extract_winners(openai_api_key, story)
  
  if (!is.null(winners_csv)) {
    # Attempt to transpose and bind the results, with error handling
    tryCatch({
      trial_results <- t(winners_csv$Winner)
      trial_results <- cbind(Trial = paste("Trial", i), trial_results)
      colnames(trial_results) <- c("Trial", state.abb)
      results <- rbind(results, trial_results)
    }, error = function(e) {
      cat("Error processing trial results: ", e$message, "\n")
    })
  }
  
  # Add delays to avoid hitting rate limits
  Sys.sleep(5)
}

# Close the file connection after the loop
close(fileConn)

# Save the results to an Excel file
results_filename <- paste0("election_results_Control_BBC_", timestamp, ".xlsx")
results_filepath <- file.path("/Users/jaredblack/GitHub/ElectionGPT/data/raw", results_filename)
write.xlsx(results, results_filepath)
cat("Election results saved as:", results_filepath, "\n")
cat("All stories saved as:", combined_story_filepath, "\n")