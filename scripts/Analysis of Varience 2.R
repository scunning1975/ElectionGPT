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
# Drop IA and add CA and TX
top_swing_states <- state_variability %>%
  arrange(desc(Variability)) %>%
  filter(State != "IA") %>%                  # Drop IA
  bind_rows(state_variability %>%            # Add CA and TX
              filter(State %in% c("CA", "TX"))) %>%
  distinct(State, .keep_all = TRUE) %>%
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

# Prepare data for plotting
# Add a dummy state for the legend
swing_states_data <- swing_states_data %>%
  mutate(
    State = as.character(State)
  )

# Define the order of states for consistent facet placement
state_order <- c(setdiff(unique(swing_states_data$State), c("Legend")), "Legend")

# Create a dummy data point for the 'Legend' facet
legend_data <- data.frame(
  State = "Legend",
  Voice = "MSNBC",
  Date = as.Date(min(swing_states_data$Date)),
  Group = factor("Experimental", levels = c("Experimental", "Control")),
  Win_Proportion = NA
)

# Combine with the plotting data
swing_states_data <- bind_rows(swing_states_data, legend_data)

# Convert State to a factor with specified levels
swing_states_data$State <- factor(swing_states_data$State, levels = state_order)

# Plot the data with the legend facet
ggplot(swing_states_data, aes(x = Date, y = Win_Proportion * 100, color = Group)) +
  geom_line() +
  # Add explanatory text in the 'Legend' facet
  geom_text(
    data = subset(swing_states_data, State == "Legend"),
    aes(x = min(swing_states_data$Date) + 5, y = 50),
    label = "Lines represent Harris's win proportion over time.\nDifferent colors indicate Experimental and Control groups.",
    hjust = 0,
    vjust = 0.5,
    size = 4,
    inherit.aes = FALSE
  ) +
  facet_wrap(~ State, scales = "free_y", ncol = 3) +
  labs(
    title = "Win Proportion Over Time for Top Swing States",
    y = "Win Proportion (%)",
    x = "Date",
    color = "Group"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5, size = 14),
    strip.text = element_text(face = "bold", size = 10),
    legend.position = "bottom"
  )