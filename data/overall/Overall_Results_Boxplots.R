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

# Read the panel data from the new CSV file
panel_data <- read_csv("/Users/jaredblack/GitHub/ElectionGPT/data/overall/overall_election_results_17Oct.csv")

# Merge panel data with electoral votes
merged_data <- panel_data %>%
  left_join(electoral_votes, by = "State") %>%
  filter(Result == 0) %>% # Only votes for Harris
  group_by(Voice, Trial) %>%
  summarize(Harris_Electoral_Votes = sum(Electoral_Votes, na.rm = TRUE), .groups = 'drop')

# Calculate mean and median for each voice
voice_stats <- merged_data %>%
  group_by(Voice) %>%
  summarize(
    Mean = mean(Harris_Electoral_Votes),
    Median = median(Harris_Electoral_Votes),
    .groups = 'drop'
  )

# Create the main plot
p <- ggplot(merged_data, aes(x = Harris_Electoral_Votes, y = Voice, fill = Voice)) +
  geom_boxplot(outlier.shape = NA, alpha = 0.7, width = 0.5) +
  geom_jitter(aes(color = Voice), width = 0, height = 0.2, size = 2, alpha = 0.5) +
  geom_vline(xintercept = 270, linetype = "dashed", color = "darkred", size = 1) +
  geom_point(data = voice_stats, aes(x = Mean, y = Voice), shape = 23, size = 3, fill = "blue") +
  geom_point(data = voice_stats, aes(x = Median, y = Voice), shape = 22, size = 3, fill = "green") +
  scale_fill_manual(values = c("Fox" = "#1f78b4", "Msnbc" = "#33a02c", "Bbc" = "#e31a1c")) +
  scale_color_manual(values = c("Fox" = "#1f78b4", "Msnbc" = "#33a02c", "Bbc" = "#e31a1c")) +
  labs(title = "Electoral Votes for Kamala Harris by News Reader",
       subtitle = "Based on multiple trials per news reader voice",
       x = "Electoral Votes for Harris",
       y = "News Voice") +
  theme_fivethirtyeight() +
  theme(
    legend.position = "none",
    plot.title = element_text(face = "bold", hjust = 0.5, size = 16),
    plot.subtitle = element_text(hjust = 0.5, size = 12),
    axis.title = element_text(face = "bold", size = 12),
    axis.text = element_text(size = 10),
    plot.margin = margin(10, 10, 60, 10)  # Increase bottom margin for larger note
  ) +
  annotate("text", x = 270, y = 2, label = "270 Votes to Win", color = "darkred", angle = 90, vjust = -0.5, size = 4)

# Create a key/legend
key_data <- data.frame(
  x = c(1, 2, 3, 4, 5),
  y = c(1, 1, 1, 1, 1),
  label = c("Fox News", "MSNBC", "BBC", "Mean", "Median"),
  color = c("#1f78b4", "#33a02c", "#e31a1c", "blue", "green"),
  shape = c(21, 21, 21, 23, 22)
)

key <- ggplot(key_data, aes(x = x, y = y, fill = color, shape = shape)) +
  geom_point(size = 5) +
  geom_text(aes(label = label), hjust = -0.2, size = 4) +
  scale_fill_identity() +
  scale_shape_identity() +
  theme_void() +
  xlim(0.5, 6) + ylim(0.5, 1.5)

# Combine the main plot and key
combined_plot <- grid.arrange(p, key, ncol = 1, heights = c(10, 1))

# Add explanatory note with larger font size
note <- "Note: This chart shows the distribution of electoral votes for Kamala Harris based on different news sources.\nThe boxes represent the interquartile range, with the middle line showing the median.\nThe points represent individual trials. Blue triangles show the mean and green squares show the median for each voice.\nThe dashed red line marks the 270 votes needed to win."

grid.arrange(combined_plot, 
             bottom = textGrob(note, x = 0, hjust = 0, vjust = 1, 
                               gp = gpar(fontsize = 11, lineheight = 1.2)))