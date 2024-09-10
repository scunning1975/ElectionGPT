library(shiny)
library(tidyverse)
library(scales)
library(sf)

# Create a simplified hexagonal map data
us_hex <- tibble(
  state = c(state.abb, "DC"),
  row = c(7, 2, 5, 5, 1, 4, 3, 3, 7, 6, 1, 2, 4, 5, 4, 4, 5, 6, 1, 3, 2, 5, 4, 6, 4, 3, 4, 3, 1, 2, 5, 2, 5, 2, 4, 5, 2, 3, 1, 6, 3, 6, 7, 3, 1, 4, 1, 5, 4, 2, 3),
  col = c(9, 1, 4, 7, 1, 3, 10, 10, 8, 7, 1, 2, 5, 7, 5, 4, 6, 5, 10, 9, 11, 7, 4, 5, 6, 2, 3, 2, 11, 10, 3, 9, 7, 1, 7, 4, 1, 8, 11, 7, 2, 6, 5, 3, 10, 7, 2, 9, 5, 1, 10)
) %>%
  mutate(
    x = col * 1.05 + (row %% 2) * 0.525,  # Adjust stagger effect for closer fit
    y = -row * 0.9  # Tighter vertical spacing
  )

# Convert to sf object
us_hex_sf <- st_as_sf(us_hex, coords = c("x", "y"), crs = 4326)

# Create hexagon polygons manually with smaller size to avoid overlap
hex_radius <- 0.45  # Control the size of hexagons

create_hexagon <- function(center_x, center_y, radius = hex_radius) {
  angles <- seq(0, 2 * pi, length.out = 7)
  x_hex <- center_x + radius * cos(angles)
  y_hex <- center_y + radius * sin(angles)
  
  # Ensure the polygon is closed by adding the first point as the last point
  x_hex <- c(x_hex, x_hex[1])
  y_hex <- c(y_hex, y_hex[1])
  
  st_polygon(list(cbind(x_hex, y_hex)))
}

# Create hexagonal geometry for each state
us_hex_sf$geometry <- mapply(create_hexagon, us_hex$x, us_hex$y, SIMPLIFY = FALSE) %>%
  st_sfc(crs = 4326)

# Electoral votes data
electoral_votes <- tibble(
  state = c(state.abb, "DC"),
  Votes = c(9,3,11,6,55,9,7,3,29,16,4,4,20,11,6,6,8,8,4,10,11,16,10,6,10,3,5,6,4,14,5,29,15,3,18,7,7,20,4,9,3,11,38,6,3,13,12,5,10,3,3)
)

# Read the data
read_election_data <- function(file_path) {
  read_csv(file_path) %>%
    pivot_longer(cols = -Trial, names_to = "state", values_to = "Result") %>%
    group_by(state) %>%
    summarize(Dem_Win_Prob = mean(Result == 0, na.rm = TRUE))
}

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
      tableOutput("vote_summary")  # Add the table for vote summary
    )
  )
)

# Server
server <- function(input, output, session) {
  
  election_data <- reactive({
    file_path <- switch(input$data_source,
                        "Direct" = "/Users/jaredblack/GitHub/ElectionGPT/data/fine-tuned_direct_election_results.csv",
                        "Fox" = "/Users/jaredblack/GitHub/ElectionGPT/data/fine-tuned_Fox_election_results.csv",
                        "MSNBC" = "/Users/jaredblack/GitHub/ElectionGPT/data/fine-tuned_MSNBC_election_results.csv",
                        "BBC" = "/Users/jaredblack/GitHub/ElectionGPT/data/fine-tuned_BBC_election_results.csv")  # Added BBC option
    
    read_election_data(file_path) %>%
      left_join(electoral_votes, by = "state")
  })
  
  output$election_map <- renderPlot({
    data <- election_data()
    
    # Join the election data with the hexagonal map data
    map_data <- us_hex_sf %>%
      left_join(data, by = "state")
    
    ggplot(data = map_data) +
      geom_sf(aes(fill = Dem_Win_Prob), color = "white") +
      geom_sf_text(aes(label = state), size = 5) +  # Larger text for state names
      scale_fill_gradient2(
        low = "red", mid = "white", high = "blue", 
        midpoint = 0.5, 
        labels = scales::percent_format(),
        name = "Probability of\nDemocratic Win"
      ) +
      labs(title = paste("2024 US Presidential Election Projection -", input$data_source, "Data"),
           subtitle = "Based on simulated election trials") +
      theme_void() +
      theme(legend.position = "right",
            plot.title = element_text(hjust = 0.5, size = 16),
            plot.subtitle = element_text(hjust = 0.5, size = 12))
  })
  
  # Render vote summary table
  output$vote_summary <- renderTable({
    data <- election_data()
    data %>%
      mutate(Winner = ifelse(Dem_Win_Prob > 0.5, "Democratic", "Republican"),
             Winning_Votes = ifelse(Dem_Win_Prob > 0.5, Votes, 0)) %>%
      select(state, Votes, Winner, Winning_Votes)
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