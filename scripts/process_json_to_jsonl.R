library(jsonlite)
library(purrr)
library(fs)

# Set the path to the folder containing JSON files
input_folder <- "/Users/jaredblack/GitHub/ElectionGPT/data/news/"

# Set the output JSONL file path
output_file <- "/Users/jaredblack/GitHub/ElectionGPT/data/news/combined_news_data.jsonl"

# Check current working directory
print(paste("Current working directory:", getwd()))

# List all files in the input folder
print("Files in the input folder:")
print(dir_ls(input_folder))

# Get a list of JSON files in the folder that contain "news_data_2024" and end with ".json"
json_files <- dir_ls(input_folder, glob = "*news_data_2024*.json")

# Print the files that match our pattern
print("Matching JSON files:")
print(json_files)

# Function to read JSON file and convert to a single line
read_and_convert <- function(file) {
  data <- read_json(file)
  toJSON(data, auto_unbox = TRUE)
}

# Read all matching JSON files, convert to JSONL format, and write to the output file
json_files %>%
  map(read_and_convert) %>%
  walk(write_lines, output_file, append = TRUE)

cat("All matching JSON files have been combined into", output_file, "\n")
print(paste("Number of files processed:", length(json_files)))

# Check the content of the output file
if (file.exists(output_file)) {
  print(paste("Output file size:", file_size(output_file), "bytes"))
  print("First few lines of the output file:")
  print(read_lines(output_file, n = 5))
} else {
  print("Output file was not created.")
}