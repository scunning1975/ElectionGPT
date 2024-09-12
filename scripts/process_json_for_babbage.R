library(jsonlite)
library(glue)

# Function to process a single JSON file and append it to the combined output
process_file_for_finetuning <- function(input_file, con) {
  # Read the JSON data from the input file
  news_data <- fromJSON(input_file, simplifyVector = TRUE)
  
  # Check if the results field exists and if it has the right structure
  if (!"results" %in% names(news_data) || !is.data.frame(news_data$results)) {
    cat(glue("Skipped file {input_file}: 'results' field missing or not a data frame.\n"))
    return()
  }
  
  # Iterate through each row in the results
  for (i in seq_len(nrow(news_data$results))) {
    # Extract the title and body for each news article
    title <- news_data$results$title[[i]]
    body <- news_data$results$body[[i]]
    
    # Only process entries where both title and body are non-null
    if (!is.null(title) && !is.null(body)) {
      # Create the prompt-completion pair for fine-tuning with the new prompt
      finetuning_data <- list(
        prompt = glue("Create a fictional story about the election winner based on this true recent news story: {title}"),
        completion = body
      )
      
      # Write the formatted data to the output file in JSONL format
      json_line <- toJSON(finetuning_data, auto_unbox = TRUE)
      writeLines(json_line, con)
    } else {
      cat(glue("Skipped article {i}: Missing title or body in {input_file}\n"))
    }
  }
}

# Main directory containing all the JSON files
input_directory <- "/Users/jaredblack/GitHub/ElectionGPT/data/news/news for fine-tuning"
output_file <- "/Users/jaredblack/GitHub/ElectionGPT/data/news/combined_finetuning2.jsonl"  # Combined output file

# List all JSON files in the input directory
json_files <- list.files(input_directory, pattern = "\\.json$", full.names = TRUE)

# Open the output file for writing
con <- file(output_file, "w")

# Loop through each JSON file and process it, appending to the combined output file
for (json_file in json_files) {
  cat(glue("Processing file: {json_file}\n"))
  process_file_for_finetuning(json_file, con)
}

# Close the output file connection
close(con)

cat("All files processed and saved to", output_file, "\n")