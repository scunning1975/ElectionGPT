# Load the necessary libraries
library(ggplot2)
library(dplyr)
library(shiny)
library(usmap)
library(readr)
library(tidyr)
library(scales)
library(bslib)

# Read the panel data from the CSV file
panel_data <- read.csv("data/panel_election_results_state.csv")

# Convert the Date column to Date format
panel_data$Date <- as.Date(panel_data$Date, format = "%m/%d/%y")

# Ensure Trial column is numeric
panel_data$Trial <- as.numeric(panel_data$Trial)

# Read the Expert Opinions data
expert_opinions <- read.csv("data/expert/Expert_Opinions.csv")
expert_opinions$date <- as.Date(expert_opinions$date, format = "%m/%d/%y")

# Add electoral votes information for each state (including DC)
electoral_votes <- data.frame(
  state = c("AL", "AK", "AZ", "AR", "CA", "CO", "CT", "DE", "FL", "GA", "HI", "ID", 
            "IL", "IN", "IA", "KS", "KY", "LA", "ME", "MD", "MA", "MI", "MN", 
            "MS", "MO", "MT", "NE", "NV", "NH", "NJ", "NM", "NY", "NC", "ND", 
            "OH", "OK", "OR", "PA", "RI", "SC", "SD", "TN", "TX", "UT", "VT", 
            "VA", "WA", "WV", "WI", "WY", "DC"),
  Electoral_Votes = c(9, 3, 11, 6, 54, 10, 7, 3, 30, 16, 4, 4, 19, 11, 6, 6, 
                      8, 8, 4, 10, 11, 15, 10, 6, 10, 4, 5, 6, 14, 5, 14, 16, 
                      3, 17, 7, 8, 19, 4, 9, 3, 11, 40, 6, 3, 13, 12, 4, 10, 
                      3, 3, 3)
)

# Define all states including DC
all_states <- c("AL", "AK", "AZ", "AR", "CA", "CO", "CT", "DE", "FL", "GA", "HI", "ID", 
                "IL", "IN", "IA", "KS", "KY", "LA", "ME", "MD", "MA", "MI", "MN", 
                "MS", "MO", "MT", "NE", "NV", "NH", "NJ", "NM", "NY", "NC", "ND", 
                "OH", "OK", "OR", "PA", "RI", "SC", "SD", "TN", "TX", "UT", "VT", 
                "VA", "WA", "WV", "WI", "WY", "DC")

# Define the Shiny UI
ui <- fluidPage(
  theme = bs_theme(bootswatch = "flatly"),
  titlePanel("ElectionGPT"),
  
  sidebarLayout(
    sidebarPanel(
      sliderInput("date",
                  "Select Date:",
                  min = min(panel_data$Date, na.rm = TRUE),
                  max = max(panel_data$Date, na.rm = TRUE),
                  value = max(panel_data$Date, na.rm = TRUE),
                  timeFormat = "%Y-%m-%d",
                  animate = animationOptions(interval = 300, loop = TRUE)),
      selectInput("voice",
                  "Select Voice:",
                  choices = c("Direct", "Fox", "MSNBC", "BBC"),
                  selected = "Direct")
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Map", 
                 plotOutput("hexMap"),
                 textOutput("demVotes"),
                 textOutput("repVotes")
        ),
        tabPanel("Trends", 
                 plotOutput("trendLine"),
                 plotOutput("comparisonPlot")
        ),
        tabPanel("State Outcomes", 
                 sidebarLayout(
                   sidebarPanel(
                     selectInput("voice_trend",
                                 "Select Voices:",
                                 choices = c("Direct", "Fox", "MSNBC", "BBC"),
                                 selected = c("Direct", "Fox"),
                                 multiple = TRUE),
                     selectInput("state_trend",
                                 "Select States:",
                                 choices = sort(unique(panel_data$State)),
                                 selected = "CA",
                                 multiple = TRUE)
                   ),
                   mainPanel(
                     plotOutput("stateVoiceTrend")
                   )
                 )
        )
      )
    )
  )
)

# Define the Shiny server
server <- function(input, output) {
  
  # Reactive function to filter data based on selected date and voice for the map
  filtered_data <- reactive({
    panel_data %>%
      mutate(Voice = tolower(Voice)) %>%
      filter(Voice == tolower(input$voice), Date == input$date) %>%
      left_join(electoral_votes, by = c("State" = "state"))
  })
  
  # Calculate electoral votes per trial based on the number of wins for the map
  electoral_vote_results <- reactive({
    filtered_data() %>%
      group_by(State) %>%
      summarize(
        Dem_Electoral_Votes = sum((Result == 0) * Electoral_Votes),
        Rep_Electoral_Votes = sum((Result == 1) * Electoral_Votes),
        .groups = 'drop'
      ) %>%
      mutate(
        Total_Electoral_Votes = Dem_Electoral_Votes + Rep_Electoral_Votes,
        Dem_Vote_Share = (Dem_Electoral_Votes / Total_Electoral_Votes) * 100,
        Rep_Vote_Share = (Rep_Electoral_Votes / Total_Electoral_Votes) * 100,
        state = State
      )
  })
  
  # Normalize electoral votes to sum to 538 for the map
  normalized_votes <- reactive({
    electoral_data <- electoral_vote_results()
    
    total_dem_votes <- sum(electoral_data$Dem_Electoral_Votes)
    total_rep_votes <- sum(electoral_data$Rep_Electoral_Votes)
    total_votes <- total_dem_votes + total_rep_votes
    
    dem_scaling_factor <- 538 * (total_dem_votes / total_votes)
    rep_scaling_factor <- 538 * (total_rep_votes / total_votes)
    
    electoral_data %>%
      mutate(
        Scaled_Dem_Electoral_Votes = (Dem_Electoral_Votes / total_dem_votes) * dem_scaling_factor,
        Scaled_Rep_Electoral_Votes = (Rep_Electoral_Votes / total_rep_votes) * rep_scaling_factor
      )
  })
  
  # Render the hex map with weighted electoral votes
  output$hexMap <- renderPlot({
    plot_usmap(data = normalized_votes(), values = "Dem_Vote_Share", regions = "states", include = all_states) +
      scale_fill_gradient(low = "red", high = "blue", name = "Democrat Vote Share (% of Trials)") +
      labs(title = "Electoral Vote Outcomes by State",
           subtitle = paste("Voice:", input$voice, "- Date:", input$date),
           caption = "538 electoral votes") +
      theme_void() +
      theme(plot.title = element_text(size = 20),
            plot.subtitle = element_text(size = 14))
  })
  
  # Calculate the total normalized electoral votes won by Democrats
  output$demVotes <- renderText({
    dem_votes <- sum(normalized_votes()$Scaled_Dem_Electoral_Votes)
    paste("Democrat Electoral Votes: ", round(dem_votes))
  })
  
  # Calculate the total normalized electoral votes won by Republicans
  output$repVotes <- renderText({
    rep_votes <- sum(normalized_votes()$Scaled_Rep_Electoral_Votes)
    paste("Republican Electoral Votes: ", round(rep_votes))
  })
  
  # Reactive function to calculate overall trend data for all voices
  trend_data <- reactive({
    panel_data %>%
      mutate(Voice = tolower(Voice)) %>%
      filter(Voice %in% tolower(c("Direct", "Fox", "MSNBC", "BBC"))) %>%
      group_by(Date, Voice, State) %>%
      summarize(
        Percent_Dem_Wins = mean(Result == 0) * 100,
        .groups = 'drop'
      ) %>%
      left_join(electoral_votes, by = c("State" = "state")) %>%
      group_by(Date, Voice) %>%
      summarize(
        Democrat_Votes = sum(Electoral_Votes * Percent_Dem_Wins / 100),
        .groups = 'drop'
      ) %>%
      mutate(Voice = factor(Voice, levels = c("direct", "fox", "msnbc", "bbc")))
  })
  
  output$trendLine <- renderPlot({
    ggplot(data = trend_data(), aes(x = Date, y = Democrat_Votes, color = Voice)) +
      geom_rect(aes(xmin = min(Date), xmax = max(Date), ymin = 270, ymax = Inf), 
                fill = "lightblue", alpha = 0.3) +
      geom_rect(aes(xmin = min(Date), xmax = max(Date), ymin = -Inf, ymax = 270), 
                fill = "pink", alpha = 0.3) +
      geom_line(size = 1) +
      scale_color_manual(
        values = c("direct" = "blue", "fox" = "red", "msnbc" = "purple", "bbc" = "green"),
        labels = c("GPT-Direct Voice", "Fox News", "MSNBC", "BBC"),
        guide = guide_legend(override.aes = list(linetype = 1, size = 1.5))
      ) +
      geom_hline(yintercept = 270, linetype = "dashed", color = "black", size = 1) +
      scale_y_continuous(limits = c(200, 400)) +
      scale_x_date(
        date_breaks = "1 week",
        date_labels = "%b %d"
      ) +
      labs(title = "Vote Trends by Voice",
           y = "Democrat Electoral Votes",
           x = "Date",
           color = "News Voice") +
      theme_minimal() +
      theme(
        plot.title = element_text(size = 16, face = "bold"),
        axis.title = element_text(size = 12),
        axis.text = element_text(size = 10),
        legend.title = element_blank(),
        legend.position = "bottom",
        panel.grid.minor = element_blank()
      )
  })
  
  # Reactive function to calculate state outcomes by voice
  state_voice_trend <- reactive({
    panel_data %>%
      mutate(Voice = tolower(Voice)) %>%
      filter(Voice %in% tolower(input$voice_trend), State %in% input$state_trend) %>%
      group_by(Date, Voice, State) %>%
      summarize(
        Percent_Dem_Wins = mean(Result == 0) * 100,
        .groups = 'drop'
      )
  })
  
  # Render the state outcomes by voice plot
  output$stateVoiceTrend <- renderPlot({
    ggplot(data = state_voice_trend(), aes(x = Date, y = Percent_Dem_Wins, color = State, linetype = Voice)) +
      geom_line(size = 1) +
      scale_color_manual(values = scales::hue_pal()(length(unique(state_voice_trend()$State)))) +
      scale_y_continuous(limits = c(0, 100), labels = scales::percent_format(scale = 1)) +
      scale_x_date(
        date_breaks = "3 day",
        date_labels = "%b %d"
      ) +
      labs(title = "State Outcomes by Voice",
           y = "Percentage of Trials Won by Democrats",
           x = "Date",
           color = "State",
           linetype = "Voice") +
      theme_minimal() +
      theme(plot.title = element_text(size = 20))
  })
  
  # Render the comparisonPlot with expert opinions
  comparison_data <- reactive({
    direct_data <- panel_data %>%
      filter(Voice == "Direct") %>%
      group_by(Date) %>%
      summarize(Harris = mean(Result == 0) * 100) %>%
      rename(date = Date) %>%
      mutate(Source = "GPT-Direct Voice")
    
    expert_data <- expert_opinions %>%
      pivot_longer(cols = -date, names_to = "Source", values_to = "Harris")
    
    bind_rows(
      direct_data,
      expert_data
    ) %>%
      filter(!is.na(date)) %>%
      arrange(date)
  })
  
  # The comparisonPlot rendering remains the same
  output$comparisonPlot <- renderPlot({
    ggplot(comparison_data(), aes(x = date, y = Harris, color = Source)) +
      geom_rect(aes(xmin = min(date), xmax = max(date), ymin = 50, ymax = Inf), 
                fill = "lightblue", alpha = 0.3) +
      geom_rect(aes(xmin = min(date), xmax = max(date), ymin = -Inf, ymax = 50), 
                fill = "pink", alpha = 0.3) +
      geom_line(size = 1) +
      geom_hline(yintercept = 50, linetype = "dashed", color = "black", size = 1) +
      scale_color_manual(values = c("GPT-Direct Voice" = "blue", "Silver" = "gray50", "Times" = "purple"),
                         breaks = c("GPT-Direct Voice", "Silver", "Times")) +
      scale_y_continuous(limits = c(40, 60), labels = scales::percent_format(scale = 1)) +
      scale_x_date(date_breaks = "1 week", date_labels = "%b %d") +
      labs(title = "Expert Opinion Comparison",
           y = "Support for Harris (%)",
           x = "Date") +
      theme_minimal() +
      theme(
        plot.title = element_text(size = 16, face = "bold"),
        axis.title = element_text(size = 12),
        axis.text = element_text(size = 10),
        legend.title = element_blank(),
        legend.position = "bottom",
        panel.grid.minor = element_blank()
      )
  })
}

# Run the Shiny app
shinyApp(ui, server)