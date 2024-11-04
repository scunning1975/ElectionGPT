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

# Combine panel data and control panel data with proper voice handling
combined_data <- bind_rows(
  panel_data %>% 
    mutate(
      Group = "Regular",
      Voice = tolower(Voice)
    ),
  control_panel_data %>% 
    mutate(
      Group = "Control",
      Voice = tolower(Voice)
    )
) %>%
  filter(!is.na(Voice)) %>%
  mutate(FullVoice = ifelse(Group == "Control", 
                            paste("control", Voice), 
                            Voice))

# Calculate daily electoral votes
daily_votes <- combined_data %>%
  left_join(electoral_votes, by = "State") %>%
  group_by(FullVoice, Date, State) %>%
  summarize(
    Win_Proportion = mean(Result == 0, na.rm = TRUE),
    Electoral_Votes = first(Electoral_Votes),
    .groups = 'drop'
  ) %>%
  mutate(Weighted_Votes = Win_Proportion * Electoral_Votes) %>%
  group_by(Date, FullVoice) %>%
  summarize(
    Daily_Electoral_Votes = sum(Weighted_Votes, na.rm = TRUE),
    .groups = 'drop'
  )

# Remove outliers within each Voice group and prepare for plotting
daily_votes_filtered <- daily_votes %>%
  group_by(FullVoice) %>%
  mutate(
    percentile_low = quantile(Daily_Electoral_Votes, 0.01, na.rm = TRUE),
    percentile_high = quantile(Daily_Electoral_Votes, 0.99, na.rm = TRUE)
  ) %>%
  filter(
    Daily_Electoral_Votes >= percentile_low,
    Daily_Electoral_Votes <= percentile_high
  ) %>%
  select(-percentile_low, -percentile_high) %>%
  ungroup() %>%
  mutate(
    # Properly extract voice base for both control and non-control groups
    VoiceBase = case_when(
      grepl("control", FullVoice) ~ gsub("control ", "", FullVoice),
      TRUE ~ FullVoice
    ),
    IsControl = grepl("control", FullVoice),
    # Create proper ordering for the voice groups
    VoiceBase = factor(VoiceBase, levels = c("msnbc", "fox", "direct", "bbc"))
  )

# Create the main plot - now with standard boxplots
p <- ggplot(daily_votes_filtered, aes(x = VoiceBase, y = Daily_Electoral_Votes, fill = IsControl)) +
  # Add boxplot with some transparency and wider boxes
  geom_boxplot(position = position_dodge(width = 0.8), 
               width = 0.7, 
               alpha = 0.7,
               outlier.shape = 21,            # Outline circles for outliers
               outlier.alpha = 0.5) +         # Transparent outliers
  # Add horizontal line for 270 votes
  geom_hline(yintercept = 270, linetype = "dashed", color = "darkred", size = 1) +
  # Customize colors
  scale_fill_manual(values = c("TRUE" = "#7FB6D5", "FALSE" = "#5AAE61"),
                    labels = c("TRUE" = "Control", "FALSE" = "Experimental")) +
  # Customize labels and theme
  labs(title = "Daily Electoral Votes for Kamala Harris by News Reader",
       subtitle = "Distribution of Daily Results (Excluding Top and Bottom 1%)",
       y = "Daily Electoral Votes for Harris",
       x = NULL,
       fill = "Group") +
  theme_fivethirtyeight() +
  theme(
    legend.position = "bottom",
    legend.title = element_blank(),
    plot.title = element_text(face = "bold", hjust = 0.5, size = 16),
    plot.subtitle = element_text(hjust = 0.5, size = 12),
    axis.title = element_text(face = "bold", size = 12),
    axis.text = element_text(size = 10),
    axis.text.x = element_text(angle = 0, hjust = 0.5),
    plot.margin = margin(10, 10, 10, 10)
  ) +
  # Customize axis labels
  scale_x_discrete(labels = toupper) +
  # Add annotation for 270 votes
  annotate("text", x = 0.75, y = 272, label = "270 Votes to Win", 
           color = "darkred", hjust = -0.1, size = 4)

# Add explanatory note
note <- paste0(
  "Note: This chart shows the distribution of daily electoral votes for Kamala Harris based on different news sources.\n",
  "Each boxplot represents the distribution of daily outcomes, calculated by aggregating state-level win proportions weighted by electoral votes.\n",
  "The boxes show the quartiles and median, while whiskers extend to the most extreme non-outlier values.\n",
  "Outliers (top and bottom 1% per group) have been removed. The dashed red line marks the 270 votes needed to win."
)

# Combine plot and note
final_plot <- grid.arrange(p, 
                           bottom = textGrob(note, x = 0, hjust = 0, vjust = 1, 
                                             gp = gpar(fontsize = 11, lineheight = 1.2)))