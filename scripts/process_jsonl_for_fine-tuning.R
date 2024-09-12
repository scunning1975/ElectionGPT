library(jsonlite)
library(glue)
library(purrr)

# Set the directory path
dir_path <- "/Users/jaredblack/GitHub/ElectionGPT/data/news"

# Get all .jsonl files in the directory
jsonl_files <- list.files(dir_path, pattern = "\\.jsonl$", full.names = TRUE)

# Function to safely parse JSON
safe_fromJSON <- function(json_string) {
  tryCatch(
    fromJSON(json_string),
    error = function(e) NULL
  )
}

# Function to process each file
process_file <- function(file_path) {
  # Read the file line by line
  con <- file(file_path, "r")
  lines <- readLines(con)
  close(con)
  
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
          role = "user",
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
  
  # Convert to JSON
  json_data <- sapply(formatted_data, toJSON, auto_unbox = TRUE)
  
  # Create the new file name
  new_file_name <- file.path(dir_path, glue("formatted_{basename(file_path)}"))
  
  # Write the formatted data to the new file
  writeLines(json_data, con = new_file_name)
  
  cat(glue("Processed: {basename(file_path)} -> {basename(new_file_name)}\n"))
  cat(glue("Number of examples created: {length(json_data)}\n"))
  
  # Print first formatted item
  if (length(json_data) > 0) {
    cat("First formatted item:\n")
    cat(json_data[1], "\n")
  }
}

# Process all files
lapply(jsonl_files, process_file)

cat("All files have been processed.\n")