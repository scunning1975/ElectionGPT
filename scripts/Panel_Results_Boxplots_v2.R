# Load necessary libraries
library(ggplot2)
library(dplyr)
library(readr)
library(ggthemes)
library(grid)
library(gridExtra)

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

# Standardize the case of the Voice column and remove any NA values
panel_data <- panel_data %>%
  mutate(Voice = tolower(Voice)) %>%
  filter(!is.na(Voice))

control_panel_data <- control_panel_data %>%
  mutate(Voice = tolower(Voice)) %>%
  filter(!is.na(Voice))

# Add a "Control" suffix to the Voice column for control panel data
control_panel_data$Voice <- paste(control_panel_data$Voice, "control")

# Combine the two datasets
combined_data <- rbind(panel_data, control_panel_data)

# Merge combined data with electoral votes and calculate average electoral votes for Harris per day
merged_data <- combined_data %>%
  left_join(electoral_votes, by = "State") %>%
  filter(Result == 0) %>% # Only votes for Harris
  group_by(Voice, Trial) %>%
  summarize(
    Total_Electoral_Votes = sum(Electoral_Votes, na.rm = TRUE),
    Days = n_distinct(Date),
    Harris_Electoral_Votes = Total_Electoral_Votes / Days,
    .groups = 'drop'
  )

# Remove outliers within each Voice group
merged_data <- merged_data %>%
  group_by(Voice) %>%
  mutate(
    percentile_low = quantile(Harris_Electoral_Votes, 0.01),
    percentile_high = quantile(Harris_Electoral_Votes, 0.99)
  ) %>%
  filter(
    Harris_Electoral_Votes >= percentile_low,
    Harris_Electoral_Votes <= percentile_high
  ) %>%
  select(-percentile_low, -percentile_high) %>%
  ungroup()

# Create a new column for grouping and ordering
merged_data <- merged_data %>%
  mutate(
    VoiceBase = gsub(" control", "", Voice),
    IsControl = grepl("control", Voice),
    # Create proper ordering for the voice groups
    VoiceBase = factor(VoiceBase, levels = c("msnbc", "fox", "direct", "bbc"))
  )

# Custom capitalization function
custom_capitalize <- function(x) {
  x <- tolower(x)
  x <- gsub("\\b(bbc|msnbc)\\b", "\\U\\1", x, perl = TRUE)
  tools::toTitleCase(x)
}

# Create the main plot
p <- ggplot(merged_data, aes(x = VoiceBase, y = Harris_Electoral_Votes, fill = IsControl)) +
  # Add violin plot
  geom_violin(position = position_dodge(width = 0.7), alpha = 0.7) +
  # Add boxplot inside violin
  geom_boxplot(position = position_dodge(width = 0.7), width = 0.1, alpha = 0.7) +
  # Add individual points
  geom_jitter(position = position_jitterdodge(dodge.width = 0.7, jitter.width = 0.1), 
              size = 1, alpha = 0.5) +
  # Add horizontal line for 270 votes
  geom_hline(yintercept = 270, linetype = "dashed", color = "darkred", size = 1) +
  # Customize colors
  scale_fill_manual(values = c("TRUE" = "#7FB6D5", "FALSE" = "#5AAE61"),
                    labels = c("TRUE" = "Control", "FALSE" = "Experimental")) +
  # Customize labels and theme
  labs(title = "Average Daily Electoral Votes for Kamala Harris by News Reader",
       subtitle = "Experimental vs Control Groups (Excluding Top and Bottom 1%)",
       y = "Average Daily Electoral Votes for Harris",
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
  scale_x_discrete(labels = custom_capitalize) +
  # Add annotation for 270 votes
  annotate("text", x = 4.5, y = 270, label = "270 Votes to Win", 
           color = "darkred", hjust = -0.1, size = 4)

# Add explanatory note
note <- "Note: This chart shows the distribution of average daily electoral votes for Kamala Harris based on different news sources.\nThe violin plots show the full distribution shape, with boxplots indicating quartiles and median.\nPoints represent individual trials, excluding the top and bottom 1% of observations for each group.\nThe dashed red line marks the 270 votes needed to win."

# Combine plot and note
final_plot <- grid.arrange(p, 
                           bottom = textGrob(note, x = 0, hjust = 0, vjust = 1, 
                                             gp = gpar(fontsize = 11, lineheight = 1.2)))