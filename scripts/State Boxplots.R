# Load necessary libraries
library(ggplot2)
library(dplyr)
library(readr)
library(ggthemes)
library(grid)
library(gridExtra)
library(stringr)
library(scales)

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
  group_by(Voice, Date, State, Group) %>%
  summarize(
    Win_Proportion = mean(Result == 0, na.rm = TRUE),
    .groups = 'drop'
  ) %>%
  # Join with electoral votes for reference
  left_join(electoral_votes, by = "State") %>%
  # Add state electoral votes to labels
  mutate(
    State = factor(State, levels = key_states),
    State_Label = paste0(State, "\n(", Electoral_Votes, " EV)")
  )

# Remove outliers (e.g., top and bottom 1%) within each State, Voice, and Group
state_plots_filtered <- state_plots_data %>%
  group_by(State_Label, Voice, Group) %>%
  mutate(
    lower_bound = quantile(Win_Proportion, 0.01, na.rm = TRUE),
    upper_bound = quantile(Win_Proportion, 0.99, na.rm = TRUE)
  ) %>%
  filter(
    Win_Proportion >= lower_bound,
    Win_Proportion <= upper_bound
  ) %>%
  ungroup()

# Update the plot with 538 style
p <- ggplot(state_plots_filtered, 
            aes(x = Voice, 
                y = Win_Proportion * 100, 
                fill = Group)) +
  geom_boxplot(
    position = position_dodge(width = 0.8), 
    width = 0.7, 
    alpha = 0.7,
    outlier.shape = 21,
    outlier.alpha = 0.5
  ) +
  facet_wrap(
    ~State_Label,
    ncol = 3,
    scales = "free_y"
  ) +
  # Customize colors
  scale_fill_manual(
    values = c("Experimental" = "#1f78b4", "Control" = "#33a02c"),
    labels = c("Experimental", "Control")
  ) +
  # Customize labels and theme
  labs(
    title = "Probability of Harris Victory by State and Group",
    subtitle = "Comparing Experimental and Control Groups within MSNBC and Fox Predictions",
    y = "Win Proportion (%)",
    x = NULL,
    fill = "Group"
  ) +
  theme_fivethirtyeight() +
  theme(
    legend.position = "bottom",
    legend.title = element_blank(),
    plot.title = element_text(face = "bold", hjust = 0.5, size = 16),
    plot.subtitle = element_text(hjust = 0.5, size = 12),
    axis.title = element_text(face = "bold", size = 12),
    axis.text = element_text(size = 10),
    strip.text = element_text(face = "bold", size = 12),
    panel.spacing = unit(1, "lines"),
    plot.margin = margin(10, 10, 10, 10)
  ) +
  # Adjust y-axis limits and labels
  scale_y_continuous(
    limits = c(0, 100),
    breaks = seq(0, 100, 25),
    labels = scales::percent_format(scale = 1)
  ) +
  # Adjust x-axis labels
  scale_x_discrete(labels = toupper)

# Add explanatory note
note <- textGrob(
  paste(
    "Note: Each boxplot represents the distribution of Harris's win proportion in the state,",
    "calculated as the percentage of trials where Harris won.",
    "Outliers (top and bottom 1% per group) have been removed for clarity.",
    sep = "\n"
  ),
  x = 0, hjust = 0, gp = gpar(fontsize = 10)
)

# Combine plot and note
final_plot <- grid.arrange(
  p, 
  bottom = note
)

# Save the plot
ggsave("state_win_proportions_538_style.pdf", final_plot, width = 11, height = 14, units = "in")

# Display plot
print(final_plot)