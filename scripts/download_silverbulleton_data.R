# R script to download the latest CSV file
download_latest_data <- function() {
  url <- "https://static.dwcdn.net/data/OmaRQ.csv"
  destfile <- "/Users/jaredblack/GitHub/ElectionGPT/data/expert/silverbulleton_predictions.csv"  # Specify the correct file path
  download.file(url, destfile)
  cat("File downloaded successfully:", destfile, "\n")
}

# Call the function to download the latest data
download_latest_data()