
pull_and_save_to_shiny <- function() {
  # Navigate to your local GitHub repository
  setwd("/Users/sunmingrun/Documents/GitHub/ElectionGPT/data")
  
  # Pull the latest changes from GitHub
  system("git pull origin main")
  
  message("Pulled latest changes from GitHub.")
  
  # Path to the updated file after pulling from GitHub
  source_file <- "/Users/sunmingrun/Documents/GitHub/ElectionGPT/data/panel_election_results_state.csv"
  
  # Destination path where the file will be copied to (Shiny folder)
  destination_file <- "/Users/sunmingrun/Documents/GitHub/ElectionGPT/ShinyApp/panel_election_results_state.csv"
  
  # Check if the file exists, then copy it to the Shiny folder
  if (file.exists(source_file)) {
    file.copy(from = source_file, to = destination_file, overwrite = TRUE)
    message("File copied to ShinyApp folder.")
  } else {
    message("Source file does not exist. Please check the file path.")
  }
}

# Run the function
pull_and_save_to_shiny()

#Step 2: Use Git to add, commit, and push changes to GitHub
push_to_github <- function() {
  # Navigate to your local GitHub repository
  setwd("/Users/sunmingrun/Documents/GitHub/ElectionGPT/ShinyApp")
  
  # Add the updated files to the staging area
  system("git add .")  # Add all changed files, or you can specify the specific file: system('git add path_to_file')
  
  # Commit the changes with a message
  system("git commit -m 'Automated update of Shiny'")
  
  # Push the changes to GitHub
  system("git push origin main")
  
  message("Changes pushed to GitHub successfully.")
}


# Run the entire process
push_to_github()
