#!/bin/bash

# File: /Users/jaredblack/GitHub/ElectionGPT/scripts/git_daily.sh

# Set the path to your local repository
REPO_PATH="/Users/jaredblack/GitHub/ElectionGPT"

# Change to the repository directory
cd "$REPO_PATH"

# Function to run commands and log output
run_command() {
    echo "Running: $1"
    eval "$1"
    echo ""
}

# Your Git workflow
run_command "git status"
run_command "git pull origin main"
run_command "git add ."

# Check if there are changes to commit
if [[ -n $(git status -s) ]]; then
    commit_message="Auto-update: $(date +'%Y-%m-%d %H:%M:%S')"
    run_command "git commit -m \"$commit_message\""
    run_command "git push origin main"
else
    echo "No changes to commit."
fi

echo "Git daily update completed successfully."