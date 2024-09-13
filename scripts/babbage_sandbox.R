# Install and load required packages if you haven't already
# install.packages(c("httr", "jsonlite"))
library(httr)
library(jsonlite)

# Set your OpenAI API key
api_key <- "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"

# Function to make a request to the OpenAI API
get_babbage_completion <- function(prompt, max_tokens = 500, temperature = 0.75) {
  url <- "https://api.openai.com/v1/completions"
  
  body <- list(
    model = "ft:babbage-002:personal:sea-of-stories:A6Q6RY9a",
    prompt = prompt,
    max_tokens = max_tokens,
    temperature = temperature
  )
  
  response <- POST(
    url,
    add_headers(
      "Content-Type" = "application/json",
      "Authorization" = paste("Bearer", api_key)
    ),
    body = body,
    encode = "json"
  )
  
  if (status_code(response) == 200) {
    content <- content(response, "parsed")
    return(content$choices[[1]]$text)
  } else {
    stop("Error: ", content(response, "text"))
  }
}

# Function to generate multiple completions with a delay
get_multiple_completions <- function(prompt, num_completions = 100, delay_seconds = 5) {
  completions <- list()
  
  for (i in 1:num_completions) {
    cat(sprintf("Generating completion %d of %d\n", i, num_completions))
    
    result <- get_babbage_completion(prompt)
    completions[[i]] <- result
    
    if (i < num_completions) {
      cat(sprintf("Waiting for %d seconds...\n", delay_seconds))
      Sys.sleep(delay_seconds)
    }
  }
  
  return(completions)
}

# Example usage
prompt <- "A specific quote from the news about the 2024 presidential election:"
results <- get_multiple_completions(prompt)

# Print the results
for (i in 1:length(results)) {
  cat(sprintf("\nCompletion %d:\n%s\n", i, results[[i]]))
}

# Save results to a text file
output_file <- "babbage_completions_results.txt"

# Write the prompt and results to the file
writeLines(c(paste("Prompt:", prompt), "", unlist(lapply(seq_along(results), function(i) c(paste("Completion", i, ":", sep=""), results[[i]], "")))), output_file)

cat(sprintf("\nResults have been saved to: %s\n", output_file))
