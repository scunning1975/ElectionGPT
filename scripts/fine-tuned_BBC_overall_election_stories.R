# Load required libraries
library(httr)
library(jsonlite)
library(stringr)
library(dplyr)

# Set API key for OpenAI
openai_api_key <- "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"
model_id <- "ft:gpt-4o-mini-2024-07-18:personal::A5g8HA2H" 

# Directory where stories will be saved
save_directory <- "/Users/jaredblack/GitHub/ElectionGPT/data"

# Define the prompt
prompt <- "Generate a short news story from the perspective of BBC reporter Laura Kuenssberg about the outcome of the 2024 US presidential election between Donald Trump and Kamala Harris. The story must explicitly state the winner in each of the 50 states. Include a specific detail or quote pulled directly from the news articles. After the story, provide a clear list of all 50 states and their winners in the format 'State: Winner'."

# Function to generate the story using the fine-tuned model
generate_story <- function(api_key, model_id, prompt) {
  url <- "https://api.openai.com/v1/chat/completions"
  body <- list(
    model = model_id,
    messages = list(
      list(role = "system", content = "You are Laura Kuenssberg, a BBC reporter."),
      list(role = "user", content = prompt)
    ),
    temperature = 0.75,
    max_tokens = 3000
  )
  response <- POST(url, body = toJSON(body, auto_unbox = TRUE), encode = "json",
                   add_headers(`Authorization` = paste("Bearer", api_key),
                               `Content-Type` = "application/json"))
  if (status_code(response) == 200) {
    return(content(response, "parsed")$choices[[1]]$message$content)
  } else {
    cat("Failed to generate story:", status_code(response), "\n")
    cat("Error message:", content(response, "text"), "\n")
    return(NULL)
  }
}

# Prepare the combined story text file
timestamp <- format(Sys.time(), "%Y-%m-%d_%H-%M-%S")
combined_story_filename <- paste0("fine-tuned_story_BBC_", timestamp, ".txt")
combined_story_filepath <- file.path(save_directory, "stories", combined_story_filename)
fileConn <- file(combined_story_filepath, "w")  # Initialize the file connection

# Write the prompt at the beginning of the file
writeLines(paste0("prompt <- \"", prompt, "\"\n\n-----------------------\n\n"), fileConn)

# Start the loop
for (i in 1:100) {
  cat("Generating story", i, "\n")
  story <- generate_story(openai_api_key, model_id, prompt)
  
  if (!is.null(story)) {
    # Append the story to the combined text file
    writeLines(paste0("Story ", i, ":\n", story, "\n\n----------------------------------\n\n"), fileConn)
  }
  
  # Add delays to avoid hitting rate limits
  Sys.sleep(5)
}

# Close the file connection after the loop
close(fileConn)

cat("All stories saved as:", combined_story_filepath, "\n")