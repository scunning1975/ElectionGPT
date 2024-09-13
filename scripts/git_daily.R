#!/usr/bin/env Rscript

# File: /Users/jaredblack/GitHub/ElectionGPT/scripts/git_daily.R

# Load required library
library(git2r)

# Set the path to your local repository
repo_path <- "/Users/jaredblack/GitHub/ElectionGPT"

# Function to run system commands and handle errors
run_command <- function(command) {
  result <- system(command, intern = TRUE)
  if (attr(result, "status") != 0) {
    stop(paste("Error executing command:", command, "\n", paste(result, collapse = "\n")))
  }
  cat(paste(result, collapse = "\n"), "\n")
}

# Change to the repository directory
setwd(repo_path)

# Open the repository
repo <- repository(repo_path)

# 1. Check for new stuff and 2. Pull anything new
cat("Checking for updates and pulling changes...\n")
run_command("git fetch origin")
run_command("git pull origin main")

# 3. Stage, commit, and push everything
cat("Staging changes...\n")
run_command("git add -A")

# Check if there are changes to commit
status <- status(repo)
if (length(status$staged) > 0 || length(status$unstaged) > 0 || length(status$untracked) > 0) {
  cat("Committing changes...\n")
  commit_message <- paste("Auto-update:", format(Sys.time(), "%Y-%m-%d %H:%M:%S"))
  run_command(paste0("git commit -m \"", commit_message, "\""))
  
  cat("Pushing changes...\n")
  run_command("git push origin main")
} else {
  cat("No changes to commit.\n")
}

cat("Git daily update completed successfully.\n")