# Load the necessary libraries
library(ggplot2)
library(dplyr)
library(shiny)
library(usmap)
library(readr)
library(tidyr)
library(scales)
library(bslib)
library(stringr)

# Read the panel data from the CSV files
panel_data <- read.csv("data/panel_election_results_state.csv")
control_panel_data <- read.csv("data/panel_control_election_results_state.csv")

# Combine panel data and control panel data
# Combine panel data and control panel data
combined_panel_data <- bind_rows(
  panel_data %>% mutate(Group = "Regular"),
  control_panel_data %>% mutate(Group = "Control")
) %>%
  mutate(Voice = case_when(
    Voice %in% c("MSNBC", "BBC") ~ Voice,
    TRUE ~ str_to_title(Voice)
  ))

# Convert the Date column to Date format
combined_panel_data$Date <- as.Date(combined_panel_data$Date, format = "%m/%d/%y")

# Ensure Trial column is numeric
combined_panel_data$Trial <- as.numeric(combined_panel_data$Trial)

# Read the new Expert Opinions data
expert_opinions <- read.csv("/Users/jaredblack/GitHub/ElectionGPT/data/expert/expert_combined_panel.csv")
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
                  min = min(combined_panel_data$Date, na.rm = TRUE),
                  max = max(combined_panel_data$Date, na.rm = TRUE),
                  value = max(combined_panel_data$Date, na.rm = TRUE),
                  timeFormat = "%Y-%m-%d",
                  animate = animationOptions(interval = 300, loop = TRUE)),
      selectInput("voice",
                  "Select Voice:",
                  choices = c("Direct", "Fox", "MSNBC", "BBC",
                              "Control Direct", "Control Fox", "Control MSNBC", "Control BBC"),
                  selected = "Direct"),
      selectInput("expert_comparison_voice",
                  "Select Voice for Expert Comparison:",
                  choices = c("Direct", "Fox", "MSNBC", "BBC",
                              "Control Direct", "Control Fox", "Control MSNBC", "Control BBC"),
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
                                 choices = c("Direct", "Fox", "MSNBC", "BBC",
                                             "Control Direct", "Control Fox", "Control MSNBC", "Control BBC"),
                                 selected = c("Direct", "Fox"),
                                 multiple = TRUE),
                     selectInput("state_trend",
                                 "Select States:",
                                 choices = sort(unique(combined_panel_data$State)),
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
    combined_panel_data %>%
      mutate(Voice = tolower(Voice)) %>%
      filter(
        case_when(
          grepl("^control", tolower(input$voice)) ~ Group == "Control" & Voice == tolower(gsub("^Control ", "", input$voice)),
          TRUE ~ Group == "Regular" & Voice == tolower(input$voice)
        ),
        Date == input$date
      ) %>%
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
  
  trend_data <- reactive({
    print("Step 1: Initial data")
    print(table(combined_panel_data$Voice, combined_panel_data$Group))
    
    step1 <- combined_panel_data %>%
      mutate(FullVoice = case_when(
        Group == "Control" ~ paste("Control", Voice),
        TRUE ~ Voice
      ))
    
    print("Step 2: After FullVoice creation")
    print(table(step1$FullVoice))
    
    step2 <- step1 %>%
      filter(FullVoice %in% c("Direct", "Fox", "MSNBC", "BBC",
                              "Control Direct", "Control Fox", "Control MSNBC", "Control BBC"))
    
    print("Step 3: After filtering")
    print(table(step2$FullVoice))
    
    step3 <- step2 %>%
      group_by(Date, FullVoice, State) %>%
      summarize(
        Percent_Dem_Wins = mean(Result == 0) * 100,
        .groups = 'drop'
      ) %>%
      left_join(electoral_votes, by = c("State" = "state"))
    
    print("Step 4: After summarizing by state")
    print(table(step3$FullVoice))
    
    final_data <- step3 %>%
      group_by(Date, FullVoice) %>%
      summarize(
        Democrat_Votes = sum(Electoral_Votes * Percent_Dem_Wins / 100),
        .groups = 'drop'
      ) %>%
      mutate(FullVoice = factor(FullVoice, levels = c("Direct", "Fox", "MSNBC", "BBC",
                                                      "Control Direct", "Control Fox", "Control MSNBC", "Control BBC")))
    
    print("Final step: After calculating Democrat_Votes")
    print(table(final_data$FullVoice))
    
    return(final_data)
  })
  
  output$trendLine <- renderPlot({
    data <- trend_data()
    print("Data passed to ggplot:")
    print(table(data$FullVoice))
    
    ggplot(data = data, aes(x = Date, y = Democrat_Votes, color = FullVoice)) +
      geom_rect(aes(xmin = min(Date), xmax = max(Date), ymin = 270, ymax = Inf), 
                fill = "#4169E1", alpha = 0.1) +
      geom_rect(aes(xmin = min(Date), xmax = max(Date), ymin = -Inf, ymax = 270), 
                fill = "#FF6347", alpha = 0.1) +
      geom_line(size = 1) +
      scale_color_manual(
        values = c("Direct" = "blue", "Fox" = "red", "MSNBC" = "purple", "BBC" = "green",
                   "Control Direct" = "lightblue", "Control Fox" = "pink", "Control MSNBC" = "violet", "Control BBC" = "lightgreen"),
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
    data <- combined_panel_data %>%
      mutate(FullVoice = paste(Group, Voice)) %>%
      filter(
        FullVoice %in% c(
          paste("Regular", input$voice_trend),
          paste("Control", gsub("^Control ", "", input$voice_trend))
        ),
        State %in% input$state_trend
      ) %>%
      group_by(Date, FullVoice, State) %>%
      summarize(
        Percent_Dem_Wins = mean(Result == 0) * 100,
        n = n(),
        .groups = 'drop'
      )
    
    # Print debugging information
    print(paste("Unique FullVoices:", paste(unique(data$FullVoice), collapse = ", ")))
    print(paste("Date range:", min(data$Date), "to", max(data$Date)))
    print(paste("Total rows:", nrow(data)))
    
    return(data)
  })
  
  # Render the state outcomes by voice plot
  output$stateVoiceTrend <- renderPlot({
    data <- state_voice_trend()
    
    p <- ggplot(data, aes(x = Date, y = Percent_Dem_Wins, color = State, linetype = FullVoice)) +
      scale_color_manual(values = scales::hue_pal()(length(unique(data$State)))) +
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
    
    # Add lines for series with more than one day of data
    p <- p + geom_line(data = subset(data, n > 1), size = 1)
    
    # Add points for all data
    p <- p + geom_point(size = 2)
    
    p
  })
  
  # Render the comparisonPlot with expert opinions
  comparison_data <- reactive({
    selected_voice_data <- combined_panel_data %>%
      filter(case_when(
        grepl("^Control", input$expert_comparison_voice) ~ 
          Group == "Control" & Voice == gsub("^Control ", "", input$expert_comparison_voice),
        TRUE ~ Group == "Regular" & Voice == input$expert_comparison_voice
      )) %>%
      group_by(Date) %>%
      summarize(Harris = mean(Result == 0) * 100) %>%
      rename(date = Date) %>%
      mutate(source = paste("GPT-", input$expert_comparison_voice))
    
    expert_data <- expert_opinions %>%
      select(date, Harris, source) %>%
      filter(!is.na(Harris))
    
    bind_rows(
      selected_voice_data,
      expert_data
    ) %>%
      filter(!is.na(date)) %>%
      arrange(date)
  })
  
  # The comparisonPlot rendering
  output$comparisonPlot <- renderPlot({
    data <- comparison_data()
    gpt_voice <- paste("GPT-", input$expert_comparison_voice)
    
    ggplot(data, aes(x = date, y = Harris, color = source)) +
      geom_line(size = 1) +
      geom_hline(yintercept = 50, linetype = "dashed", color = "black", size = 1) +
      scale_color_manual(values = c(setNames("blue", gpt_voice),
                                    "economist" = "red", 
                                    "silver" = "purple", 
                                    "times-siena" = "green"),
                         breaks = c(gpt_voice, "economist", "silver", "times-siena"),
                         labels = c(gpt_voice, "Economist", "FiveThirtyEight", "NYT/Siena")) +
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
