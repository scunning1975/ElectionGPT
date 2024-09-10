# Load necessary libraries
library(ggplot2)
library(dplyr)
library(readr)

# Load the data from the CSV file
file_path <- "/Users/jaredblack/GitHub/ElectionGPT/data/cost-2024-08-01-2024-09-10.csv"
data <- read_csv(file_path)

# Convert the 'date' column to Date type
data$date <- as.Date(data$date)

# Group data by date and model name, and calculate the sum of costs
grouped_data <- data %>%
  group_by(date, name) %>%
  summarise(total_cost = sum(cost_in_major))

# Calculate the total spend
total_spend <- sum(grouped_data$total_cost)

# Define specific colors for each model
model_colors <- c(
  "GPT-4 Turbo" = "darkgreen",
  "GPT-3.5 Turbo" = "lightgreen",
  "GPT-4o" = "purple",
  "GPT-4o mini" = "lightblue",
  "Fine-tuning" = "red"
)

# Create the stacked bar chart with the requested modifications
ggplot(grouped_data, aes(x = date, y = total_cost, fill = name)) +
  geom_bar(stat = "identity", width = 0.8) +  # Adjust the width for wider bars
  labs(title = "Daily Model Spend from 2024-08-01 to 2024-09-10",
       subtitle = paste("Total Spend: $", round(total_spend, 2)),
       x = "Date", y = "Cost (USD)") +
  theme_minimal(base_size = 15) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),  # Rotate date labels for better readability
    panel.grid = element_blank()  # Remove the background grid
  ) +
  scale_fill_manual(values = model_colors) +  # Apply specific colors to models
  scale_y_continuous(limits = c(0, 60))  # Set y-axis limit to 100