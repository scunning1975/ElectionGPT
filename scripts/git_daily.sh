#!/bin/bash

# File: /Users/jaredblack/GitHub/ElectionGPT/scripts/git_daily.sh

# Set the path to your local repository and data directory
REPO_PATH="/Users/jaredblack/GitHub/ElectionGPT"
DATA_PATH="$REPO_PATH/data"

# Change to the repository directory
cd "$REPO_PATH"

# Function to run commands and log output
run_command() {
    echo "Running: $1"
    eval "$1"
    echo ""
}

# Your Git workflow (only working in the /data subfolder)
run_command "git status $DATA_PATH"

# Stage changes only in the /data subfolder
run_command "git add $DATA_PATH"

# Check if there are changes to commit
if [[ -n $(git status -s $DATA_PATH) ]]; then
    commit_message="Auto-update: $(date +'%Y-%m-%d %H:%M:%S')"
    run_command "git commit -m \"$commit_message\""
    # Force push to ensure local changes are pushed regardless of remote
    run_command "git push --force origin main"
else
    echo "No changes to commit in /data."
fi

echo "Git daily update completed successfully."