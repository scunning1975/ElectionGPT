# Load necessary libraries
library(ggplot2)
library(dplyr)
library(readr)
library(ggthemes)
library(grid)
library(gridExtra)
library(stringr)

# Define electoral votes per state
electoral_votes <- data.frame(
  State = c("AL", "AK", "AZ", "AR", "CA", "CO", "CT", "DE", "FL", "GA", "HI", "ID", 
            "IL", "IN", "IA", "KS", "KY", "LA", "ME", "MD", "MA", "MI", "MN", 
            "MS", "MO", "MT", "NE", "NV", "NH", "NJ", "NM", "NY", "NC", "ND", 
            "OH", "OK", "OR", "PA", "RI", "SC", "SD", "TN", "TX", "UT", "VT", 
            "VA", "WA", "WV", "WI", "WY", "DC"),
  Electoral_Votes = c(9, 3, 11, 6, 54, 10, 7, 3, 30, 16, 4, 4, 19, 11, 6, 6, 
                      8, 8, 4, 10, 11, 15, 10, 6, 10, 4, 5, 6, 14, 5, 14, 16, 
                      3, 17, 7, 8, 19, 4, 9, 3, 11, 40, 6, 3, 13, 12, 4, 10, 
                      3, 3, 3)
)

# Read the panel data from the CSV files
panel_data <- read_csv("/Users/jaredblack/GitHub/ElectionGPT/data/panel_election_results_state.csv")
control_panel_data <- read_csv("/Users/jaredblack/GitHub/ElectionGPT/data/panel_control_election_results_state.csv")

# Define states of interest
key_states <- c("MI", "NC", "NV", "AZ", "PA", "GA", "WI", "CA", "TX")

# Add a 'Group' column to each dataframe
panel_data <- panel_data %>%
  mutate(Group = 'Experimental')

control_panel_data <- control_panel_data %>%
  mutate(Group = 'Control')

# Combine the two datasets
combined_data <- bind_rows(panel_data, control_panel_data)

# Prepare the plotting data
state_plots_data <- combined_data %>%
  filter(
    tolower(Voice) %in% c("msnbc", "fox"),
    State %in% key_states
  ) %>%
  mutate(
    Voice = tolower(Voice),
    Voice = case_when(
      Voice == "msnbc" ~ "MSNBC",
      Voice == "fox" ~ "Fox"
    ),
    Group = factor(Group, levels = c("Experimental", "Control"))
  ) %>%
  # Join with electoral votes by 'State'
  left_join(electoral_votes, by = "State") %>%
  # Calculate probability Harris wins per day per state
  group_by(Voice, Date, State, Group, Electoral_Votes) %>%
  summarize(
    Win_Probability = mean(Result == 0, na.rm = TRUE),
    .groups = 'drop'
  ) %>%
  # Calculate average electoral votes won per day per state
  mutate(
    Avg_EV_Won = Win_Probability * Electoral_Votes
  ) %>%
  # Add state electoral votes to labels
  mutate(
    State = factor(State, levels = key_states),
    State_Label = paste0(State, "\n(", Electoral_Votes, " EV)")
  )

# Update the plot to have 'Avg_EV_Won' on y-axis and facet by 'State_Label'
p <- ggplot(state_plots_data, 
            aes(x = Voice, 
                y = Avg_EV_Won, 
                fill = Group)) +
  geom_boxplot(
    alpha = 0.7,
    width = 0.6,
    position = position_dodge(width = 0.7),
    outlier.alpha = 0.5,
    outlier.size = 1
  ) +
  facet_wrap(
    ~State_Label,
    ncol = 3,
    scales = "free_y"
  ) +
  scale_fill_manual(
    values = c("Experimental" = "#4C72B0", "Control" = "#DD8452")
  ) +
  labs(
    title = "Average Electoral Votes Won per Day by Voice and Group",
    subtitle = "Comparing Experimental and Control Groups within MSNBC and Fox Predictions",
    y = "Average Electoral Votes Won",
    x = NULL,
    fill = "Group"
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    plot.title = element_text(face = "bold", hjust = 0.5, size = 14),
    plot.subtitle = element_text(hjust = 0.5, size = 12),
    strip.text = element_text(face = "bold", size = 12),
    axis.text.x = element_text(angle = 0, size = 10, face = "bold"),
    axis.text.y = element_text(size = 10),
    axis.title = element_text(size = 12),
    panel.grid.minor = element_blank(),
    panel.spacing = unit(1, "cm"),
    plot.margin = margin(20, 20, 20, 20)
  ) +
  # Adjust y-axis limits based on maximum electoral votes per state
  scale_y_continuous(
    limits = c(0, NA),  # Let ggplot adjust the upper limit automatically
    breaks = scales::pretty_breaks()
  )

# Save the plot
ggsave("state_average_electoral_votes.pdf", p, width = 8.5, height = 11, units = "in")

# Display plot
print(p)