library(jsonlite)
library(glue)
library(purrr)

# Set the file path
file_path <- "/Users/jaredblack/GitHub/ElectionGPT/data/news/combined_news_data.jsonl"

# Function to safely parse JSON
safe_fromJSON <- function(json_string) {
  tryCatch(
    fromJSON(json_string),
    error = function(e) NULL
  )
}

# Function to process the file
process_file <- function(file_path) {
  # Read the file line by line
  lines <- readLines(file_path)
  
  # Process each line
  formatted_data <- map(lines, function(line) {
    article <- safe_fromJSON(line)
    if (is.null(article) || !is.list(article) || is.null(article$title) || is.null(article$body)) {
      return(NULL)
    }
    
    list(
      messages = list(
        list(
          role = "system",
          content = "You are a helpful assistant that provides information about elections and political news."
        ),
        list(
          role = "human",
          content = glue("Summarize this news article: {article$title}")
        ),
        list(
          role = "assistant",
          content = article$body
        )
      )
    )
  })
  
  # Remove NULL entries
  formatted_data <- compact(formatted_data)
  
  # Create the new file name
  new_file_name <- gsub("\\.jsonl$", "_formatted.jsonl", file_path)
  
  # Write the formatted data to the new file
  con <- file(new_file_name, "w")
  for (item in formatted_data) {
    writeLines(toJSON(item, auto_unbox = TRUE), con)
  }
  close(con)
  
  cat(glue("Processed: {basename(file_path)} -> {basename(new_file_name)}\n"))
  cat(glue("Number of examples created: {length(formatted_data)}\n"))
  
  # Print first formatted item
  if (length(formatted_data) > 0) {
    cat("First formatted item:\n")
    cat(toJSON(formatted_data[[1]], auto_unbox = TRUE, pretty = TRUE), "\n")
  }
}

# Process the file
process_file(file_path)
cat("File has been processed.\n")