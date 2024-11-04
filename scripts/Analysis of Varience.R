# Load necessary libraries
library(ggplot2)
library(dplyr)
library(readr)
library(ggthemes)
library(stringr)
library(scales)
library(lubridate)

# Read the panel data from the CSV files
panel_data <- read_csv("/Users/jaredblack/GitHub/ElectionGPT/data/panel_election_results_state.csv")
control_panel_data <- read_csv("/Users/jaredblack/GitHub/ElectionGPT/data/panel_control_election_results_state.csv")

# Ensure that the data does not contain git conflict markers
# If you haven't already, please clean your CSV files by removing any git conflict markers.

# Add a 'Group' column to each dataframe
panel_data <- panel_data %>%
  mutate(Group = 'Experimental')

control_panel_data <- control_panel_data %>%
  mutate(Group = 'Control')

# Combine the two datasets
combined_data <- bind_rows(panel_data, control_panel_data)

# Parse the Date column correctly
combined_data <- combined_data %>%
  mutate(
    Date = parse_date_time(Date, orders = c("mdy", "mdY")),
    Voice = tolower(Voice)
  )

# Ensure that the voices are correctly labeled
combined_data <- combined_data %>%
  mutate(
    Voice = case_when(
      Voice == "msnbc" ~ "MSNBC",
      Voice == "fox" ~ "Fox",
      Voice == "direct" ~ "Direct",
      Voice == "bbc" ~ "BBC",
      TRUE ~ Voice  # Keep other voices as they are
    ),
    Group = factor(Group, levels = c("Experimental", "Control"))
  )

# Filter for the correct voices
combined_data <- combined_data %>%
  filter(
    Voice %in% c("MSNBC", "Fox", "Direct", "BBC")
  )

# Prepare the data
state_variability_data <- combined_data %>%
  group_by(State, Voice, Date, Group) %>%
  summarize(
    Win_Proportion = mean(Result == 0, na.rm = TRUE),
    .groups = 'drop'
  )

# Step 1: Calculate variability (standard deviation) for each state across all dates and groups
state_variability <- state_variability_data %>%
  group_by(State) %>%
  summarize(
    Variability = sd(Win_Proportion, na.rm = TRUE),
    .groups = 'drop'
  )

# Identify the top N states with the highest variability (swing states)
top_swing_states <- state_variability %>%
  arrange(desc(Variability)) %>%
  top_n(10, Variability)

# Display the top swing states
print("Top Swing States Based on Variability:")
print(top_swing_states)

# Step 2: For these swing states, find the days with the largest changes in Win_Proportion
# First, filter the data for the top swing states
swing_states_data <- state_variability_data %>%
  filter(State %in% top_swing_states$State)

# Calculate day-to-day changes in Win_Proportion for each state and group
swing_states_daily_changes <- swing_states_data %>%
  arrange(State, Voice, Group, Date) %>%
  group_by(State, Voice, Group) %>%
  mutate(
    Win_Proportion_Change = Win_Proportion - lag(Win_Proportion)
  ) %>%
  ungroup()

# Remove NA values resulting from lag
swing_states_daily_changes <- swing_states_daily_changes %>%
  filter(!is.na(Win_Proportion_Change))

# Find the top 10 days with the largest absolute changes in Win_Proportion
top_changes <- swing_states_daily_changes %>%
  mutate(Abs_Change = abs(Win_Proportion_Change)) %>%
  arrange(desc(Abs_Change)) %>%
  top_n(10, Abs_Change)

# Display the top 10 days with the most variability in swing states
print("Top 10 Days with the Largest Changes in Win Proportion for Swing States:")
print(top_changes %>%
        select(State, Voice, Group, Date, Win_Proportion_Change))

# Optional: Visualize the variability for the top swing states
ggplot(swing_states_data, aes(x = Date, y = Win_Proportion * 100, color = Group)) +
  geom_line() +
  facet_wrap(~ State, scales = "free_y") +
  labs(
    title = "Win Proportion Over Time for Top Swing States",
    y = "Win Proportion (%)",
    x = "Date",
    color = "Group"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5, size = 14),
    strip.text = element_text(face = "bold", size = 10)
  )