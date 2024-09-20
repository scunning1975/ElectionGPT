
pull_and_save_to_shiny <- function() {
  # Navigate to your local GitHub repository
  setwd("/Users/sunmingrun/Documents/GitHub/ElectionGPT")
  
  # Pull the latest changes from GitHub
  system("git commit -m 'update of Shiny'")
  system("git pull")
  system("git merge")
  system("git add .")
  system("git pull origin main")
  message("Pulled latest changes from GitHub.")
  
}
pull_and_save_to_shiny()

save_to_shiny <- function() {
  # Path to the updated file after pulling from GitHub
  source_file_panel <- "/Users/sunmingrun/Documents/GitHub/ElectionGPT/data/panel_election_results_state.csv"
  
  # Destination path where the file will be copied to (Shiny folder)
  destination_file_panel <- "/Users/sunmingrun/Documents/GitHub/ElectionGPT/ShinyApp/panel_election_results_state.csv"
  
  # Check if the file exists, then copy it to the Shiny folder
  if (file.exists(source_file_panel)) {
    file.copy(from = source_file_panel, to = destination_file_panel, overwrite = TRUE)
    message("panel_election_results_state.csv file copied to ShinyApp folder.")
  } else {
    message("panel_election_results_state.csv does not exist. Please check the file path.")
  }
  
  # Path to the expert_combined_panel.csv file after pulling from GitHub
  source_file_expert <- "/Users/sunmingrun/Documents/GitHub/ElectionGPT/data/expert/expert_combined_panel.csv"
  
  # Destination path for the expert_combined_panel.csv (same folder)
  destination_file_expert <- "/Users/sunmingrun/Documents/GitHub/ElectionGPT/ShinyApp/expert_combined_panel.csv"
  
  # Check if the file exists, then copy it to the Shiny folder
  if (file.exists(source_file_expert)) {
    file.copy(from = source_file_expert, to = destination_file_expert, overwrite = TRUE)
    message("expert_combined_panel.csv file copied to ShinyApp folder.")
  } else {
    message("expert_combined_panel.csv does not exist. Please check the file path.")
  }
  
  source_file_panel <- "/Users/sunmingrun/Documents/GitHub/ElectionGPT/data/panel_control_election_results_state.csv"
  
  # Destination path where the file will be copied to (Shiny folder)
  destination_file_panel <- "/Users/sunmingrun/Documents/GitHub/ElectionGPT/ShinyApp/panel_control_election_results_state.csv"
  
  # Check if the file exists, then copy it to the Shiny folder
  if (file.exists(source_file_panel)) {
    file.copy(from = source_file_panel, to = destination_file_panel, overwrite = TRUE)
    message("panel_election_results_state.csv file copied to ShinyApp folder.")
  } else {
    message("panel_election_results_state.csv does not exist. Please check the file path.")
  }
  
}

save_to_shiny()
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
