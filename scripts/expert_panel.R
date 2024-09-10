# Load necessary library
library(dplyr)

# File paths
economist_path <- "/Users/jaredblack/GitHub/ElectionGPT/data/expert/Economist_timeseries_poll_data.csv"
silver_path <- "/Users/jaredblack/GitHub/ElectionGPT/data/expert/silverbulleton_predictions.csv"
times_siena_path <- "/Users/jaredblack/GitHub/ElectionGPT/data/expert/Times-Siena_timeseries_poll_data.csv"

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
write.csv(combined_panel, "/Users/jaredblack/GitHub/ElectionGPT/data/expert/combined_panel.csv", row.names = FALSE)

# Print a message
cat("Combined panel CSV has been created successfully!\n")