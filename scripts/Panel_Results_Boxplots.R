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

# Modify the Voice factor levels to group pairs together
merged_data$Voice <- factor(merged_data$Voice, levels = c(
  "msnbc", "msnbc control", 
  "fox", "fox control", 
  "direct", "direct control", 
  "bbc", "bbc control"
))

# Create a new column for coloring
merged_data$VoiceGroup <- gsub(" control", "", merged_data$Voice)

# Create the main plot
# Custom capitalization function
custom_capitalize <- function(x) {
  x <- tolower(x)
  x <- gsub("\\b(bbc|msnbc)\\b", "\\U\\1", x, perl = TRUE)
  tools::toTitleCase(x)
}

# Create the main plot
p <- ggplot(merged_data, aes(x = Harris_Electoral_Votes, y = Voice, fill = VoiceGroup)) +
  geom_boxplot(outlier.shape = NA, alpha = 0.7, width = 0.5) +
  geom_jitter(aes(color = VoiceGroup), width = 0, height = 0.2, size = 1, alpha = 0.5) +
  geom_vline(xintercept = 270, linetype = "dashed", color = "darkred", size = 1) +
  scale_fill_brewer(palette = "Set2") +
  scale_color_brewer(palette = "Set2") +
  labs(title = "Average Daily Electoral Votes for Kamala Harris by News Reader",
       subtitle = "Experimental vs Control for each news source",
       x = "Average Daily Electoral Votes for Harris",
       y = NULL) +
  theme_fivethirtyeight() +
  theme(
    legend.position = "none",
    plot.title = element_text(face = "bold", hjust = 0.5, size = 16),
    plot.subtitle = element_text(hjust = 0.5, size = 12),
    axis.title = element_text(face = "bold", size = 12),
    axis.text = element_text(size = 10),
    axis.text.y = element_text(hjust = 0),
    plot.margin = margin(10, 10, 60, 10),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank()
  ) +
  scale_y_discrete(labels = custom_capitalize) +
  annotate("text", x = 270, y = 0.3, label = "270 Votes to Win", color = "darkred", angle = 90, vjust = -0.5, size = 4)

# Calculate x-axis limits to include all data points
x_min <- min(merged_data$Harris_Electoral_Votes, na.rm = TRUE)
x_max <- max(merged_data$Harris_Electoral_Votes, na.rm = TRUE)
x_range <- x_max - x_min
x_padding <- x_range * 0.05  # 5% padding on each side

# Apply the calculated limits and zoom in
p <- p + coord_cartesian(xlim = c(max(0, x_min - x_padding), x_max + x_padding),
                         ylim = c(-0.5, 8.5),  # Extend y-axis to show full "270 Votes to Win" text
                         clip = "off")  # Allow drawing outside the plot area

# Create a key/legend
key_data <- data.frame(
  x = c(1, 2, 3, 4),
  y = c(1, 1, 1, 1),
  VoiceGroup = c("MSNBC", "Fox", "Direct", "BBC")
)

key <- ggplot(key_data, aes(x = x, y = y, fill = VoiceGroup)) +
  geom_point(shape = 21, size = 5) +
  geom_text(aes(label = VoiceGroup), hjust = -0.5, size = 4) +
  scale_fill_brewer(palette = "Set2") +
  theme_void() +
  theme(legend.position = "none") +
  xlim(0.5, 5.5) + ylim(0.5, 1.5)

# Combine the main plot and key
combined_plot <- grid.arrange(p, key, ncol = 1, heights = c(10, 1))

# Add explanatory note with larger font size
note <- "Note: This chart shows the distribution of average daily electoral votes for Kamala Harris based on different news sources.\nExperimental and control groups for each source are shown in pairs. The boxes represent the interquartile range,\nwith the middle line showing the median. The points represent individual trials.\nThe dashed red line marks the 270 votes needed to win."

grid.arrange(combined_plot, 
             bottom = textGrob(note, x = 0, hjust = 0, vjust = 1, 
                               gp = gpar(fontsize = 11, lineheight = 1.2)))