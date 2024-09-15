library(jsonlite)
library(glue)

# Function to process a single JSON file and append it to the combined output in chat format
process_file_for_finetuning_chat <- function(input_file, con) {
  # Read the JSON data from the input file
  news_data <- fromJSON(input_file, simplifyVector = TRUE)
  
  # Check if 'results' is a list and has entries
  if (is.list(news_data$results) && length(news_data$results) > 0) {
    # Iterate through each entry in the 'results'
    for (i in seq_along(news_data$results)) {
      # Extract the title and body for each news article
      title <- news_data$results$title[[i]]
      body <- news_data$results$body[[i]]
      
      # Only process entries where both title and body are non-null
      if (!is.null(title) && !is.null(body)) {
        # Create the chat-format data with system, user, and assistant roles
        chat_data <- list(
          messages = list(
            list(role = "system", content = "You are a helpful assistant."),
            list(role = "user", content = glue("Create a fictional story about the election based on this true recent news story: {title}")),
            list(role = "assistant", content = body)
          )
        )
        
        # Write the formatted data to the output file in JSONL format
        json_line <- toJSON(chat_data, auto_unbox = TRUE)
        writeLines(json_line, con)
        writeLines("\n", con)  # Add a new line after each record
      } else {
        cat(glue("Skipped article {i}: Missing title or body in {input_file}\n"))
      }
    }
  } else {
    cat(glue("No results found in {input_file}\n"))
  }
}

# Main directory containing all the JSON files
input_directory <- "/Users/jaredblack/GitHub/ElectionGPT/data/news/news for fine-tuning"
output_file <- "/Users/jaredblack/GitHub/ElectionGPT/data/news/combined_finetuning_chat3.jsonl"  # Combined output file

# List all JSON files in the input directory
json_files <- list.files(input_directory, pattern = "\\.json$", full.names = TRUE)

# Ensure connection closes correctly using tryCatch
tryCatch({
  # Open the output file for writing
  con <- file(output_file, "w")
  
  # Loop through each JSON file and process it, appending to the combined output file
  for (json_file in json_files) {
    cat(glue("Processing file: {json_file}\n"))
    process_file_for_finetuning_chat(json_file, con)
  }
}, finally = {
  # Close the output file connection when done, even if there's an error
  close(con)
})