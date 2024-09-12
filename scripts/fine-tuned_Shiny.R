library(shiny)
library(tidyverse)
library(usmap)
library(scales)

# Read the data
read_election_data <- function(file_path) {
  read_csv(file_path) %>%
    pivot_longer(cols = -Trial, names_to = "state", values_to = "Result") %>%
    group_by(state) %>%
    summarize(Dem_Win_Prob = mean(Result == 0, na.rm = TRUE))
}

# Electoral votes data
electoral_votes <- tibble(
  state = state.abb,
  Votes = c(9,3,11,6,55,9,7,3,29,16,4,4,20,11,6,6,8,8,4,10,11,16,10,6,10,3,5,6,4,14,5,29,15,3,18,7,7,20,4,9,3,11,38,6,3,13,12,5,10,3)
)

# Add DC
electoral_votes <- bind_rows(electoral_votes, tibble(state = "DC", Votes = 3))

# UI
ui <- fluidPage(
  titlePanel("2024 US Presidential Election Projection"),
  sidebarLayout(
    sidebarPanel(
      selectInput("data_source", "Select Data Source:",
                  choices = c("Direct", "Fox", "MSNBC", "BBC"),
                  selected = "Direct"),
      hr(),
      textOutput("dem_votes"),
      textOutput("rep_votes")
    ),
    mainPanel(
      plotOutput("election_map", height = "600px"),
      tableOutput("state_summary")  # Add a table for the state-by-state summary
    )
  )
)

server <- function(input, output, session) {
  
  election_data <- reactive({
    file_path <- switch(input$data_source,
                        "Direct" = "/Users/jaredblack/GitHub/ElectionGPT/data/fine-tuned_direct_election_results.csv",
                        "Fox" = "/Users/jaredblack/GitHub/ElectionGPT/data/fine-tuned_Fox_election_results.csv",
                        "MSNBC" = "/Users/jaredblack/GitHub/ElectionGPT/data/fine-tuned_MSNBC_election_results.csv",
                        "BBC" = "/Users/jaredblack/GitHub/ElectionGPT/data/fine-tuned_BBC_election_results.csv")
    
    read_election_data(file_path) %>%
      left_join(electoral_votes, by = "state")
  })
  
  output$election_map <- renderPlot({
    data <- election_data()
    
    plot_usmap(data = data, values = "Dem_Win_Prob") +
      scale_fill_gradient2(
        low = "red", mid = "white", high = "blue", 
        midpoint = 0.5, 
        labels = scales::percent_format(),
        name = "Probability of\nDemocratic Win"
      ) +
      labs(title = paste("2024 US Presidential Election Projection -", input$data_source, "Data"),
           subtitle = "Based on simulated election trials") +
      theme(legend.position = "right")
  })
  
  output$state_summary <- renderTable({
    data <- election_data()
    data %>%
      mutate(
        Dem_Win_Percent = scales::percent(Dem_Win_Prob, accuracy = 0.1)
      ) %>%
      select(state, Votes, Dem_Win_Percent)
  })
  
  output$dem_votes <- renderText({
    data <- election_data()
    dem_votes <- sum(data$Votes[data$Dem_Win_Prob > 0.5], na.rm = TRUE)
    paste("Projected Democratic Electoral Votes:", dem_votes)
  })
  
  output$rep_votes <- renderText({
    data <- election_data()
    rep_votes <- sum(data$Votes[data$Dem_Win_Prob <= 0.5], na.rm = TRUE)
    paste("Projected Republican Electoral Votes:", rep_votes)
  })
}

# Run the app
shinyApp(ui, server)