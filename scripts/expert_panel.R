# Load necessary library
library(dplyr)

# File paths
economist_path <- "data/expert/Economist_timeseries_poll_data.csv"
silver_path <- "data/expert/silverbulleton_predictions.csv"
times_siena_path <- "data/expert/Times-Siena_timeseries_poll_data.csv"

# Read in the CSV files
economist_data <- read.csv(economist_path)
silver_data <- read.csv(silver_path)
times_siena_data <- read.csv(times_siena_path)

# Add source columns
economist_data <- economist_data %>% mutate(source = "economist")
silver_data <- silver_data %>% mutate(source = "silver")
times_siena_data <- times_siena_data %>% mutate(source = "times-siena")

# Combine the data
combined_panel <- bind_rows(economist_data, silver_data, times_siena_data)

# Save the combined panel to a new CSV
write.csv(combined_panel, "data/expert/combined_panel.csv", row.names = FALSE)

# Print a message
cat("Combined panel CSV has been created successfully!\n")

# Load necessary libraries
library(ggplot2)
library(dplyr)
library(lubridate)

# File path of combined panel
panel_file <- "data/expert/expert_combined_panel.csv"

# Read the combined panel data
poll_data <- read.csv(panel_file)

# Convert the date column to Date type for proper plotting
poll_data$date <- mdy(poll_data$date)

# Filter data to start from 13 August 2024 and remove rows with NAs
poll_data <- poll_data %>%
  filter(date >= as.Date("2024-08-13")) %>%
  filter(!is.na(Harris) & !is.na(Trump))

# Reshape the data to a longer format for easy plotting
poll_data_long <- poll_data %>%
  gather(key = "candidate", value = "percentage", Harris, Trump)

# Create the plot with cleaner visuals
ggplot(poll_data_long, aes(x = date, y = percentage, color = candidate, linetype = source)) +
  geom_line(size = 1.5) +  # Thicker lines for clarity
  labs(title = "Comparison of Harris and Trump Polling Data",
       x = "Date",
       y = "Percentage",
       color = "Candidate",
       linetype = "Source") +
  theme_minimal(base_size = 15) +  # Clean minimalistic theme
  scale_color_manual(values = c("Harris" = "#1f78b4", "Trump" = "#e31a1c")) +  # Clearer colors for candidates
  scale_linetype_manual(values = c("economist" = "solid", "silver" = "dashed", "times-siena" = "dotted")) +  # Distinguishable line types for sources
  theme(legend.position = "bottom",  # Position legend at the bottom
        plot.title = element_text(hjust = 0.5),  # Center the title
        legend.title = element_text(face = "bold"),  # Bold legend title for emphasis
        legend.text = element_text(size = 12))  # Larger legend text for readability