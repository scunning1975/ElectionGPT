library(usmap) #import the package
library(shiny)
library(ggplot2) #use ggplot2 to add layer for visualization
library(maps)
library(openxlsx)
library(readxl)
library(dplyr)
library(tidyr)

library(tidytuesdayR) # to get tidytuesday data
library(tidyverse) # for ggplot
library(janitor) # for clean_names
library(ggeasy) # making ggplot customisation easy
library(gganimate) # for animating map by year
library(transformr) # i think i need this for gganimate
library(patchwork) # to patch plots together
library(PNWColors)

library(tidyquant)
library(plotly)
library(DT)
library(ggrepel)
library(tidyr)
library(shinycssloaders)
library(shinythemes)

library(shinyBS)
library(scales)
library(shinyWidgets)
library(bslib)
#library(rjson)

library(shinydashboard)
library(shinycssloaders)
library(shinyjs)
library(shinyWidgets)
library(ggridges)
library(lubridate)
library(plotly)
library(qicharts2)
library(rintrojs)


library(AMR)
library(data.table)
library(DT)
library(ggridges)
library(lubridate)
library(survival)
library(ggpubr)
library(survminer)
library(viridis)
library(zoo)

# 1 import the data

data<-read_csv("panel_election_results_state.csv",show_col_types = FALSE)

# 2 change the variable name and assigned the predicted party based on the result
melted_data<-data%>%
  rename(
    value = Result,  # Renaming 'Result' to 'value'
    state = State,
    Type= Voice# Renaming 'State' to 'state'
  )%>%
  mutate(party = ifelse(value ==1, "Republican", "Democratic")) %>%
  mutate(Type = ifelse(Type == "direct", "Direct", Type))

# 3.1 2024-8-19 data has duplicates when retry to append the data
data_0819_msnbc <- melted_data %>%
  filter(Type == "MSNBC", Date == "2024-08-19") %>%
  distinct()  # This removes duplicates within the MSNBC data for August 19th

# 3.2 Filter for all data that is NOT MSNBC on August 19, 2024
data_other <- melted_data %>%
  filter(!(Type == "MSNBC" & Date == "2024-08-19"))

# 3.3 Combine the cleaned MSNBC data for August 19 with the rest of the data
melted_data <- bind_rows(data_other, data_0819_msnbc) %>%
  distinct(Trial, Type, Date, state, .keep_all = TRUE)

# 4 Assign the electoral_votes to each state in each trial
electoral_votes <- data.frame(
  state = c("AL", "AK", "AZ", "AR", "CA", "CO", "CT", "DE", "FL", "GA", 
            "HI", "ID", "IL", "IN", "IA", "KS", "KY", "LA", "ME", "MD", 
            "MA", "MI", "MN", "MS", "MO", "MT", "NE", "NV", "NH", "NJ", 
            "NM", "NY", "NC", "ND", "OH", "OK", "OR", "PA", "RI", "SC", 
            "SD", "TN", "TX", "UT", "VT", "VA", "WA", "WV", "WI", "WY", "DC"),
  Electoral_Votes = c(9, 3, 11, 6, 54, 10, 7, 3, 30, 16, 
                      4, 4, 19, 11, 6, 6, 8, 8, 4, 10, 
                      11, 15, 10, 6, 10, 4, 5, 6, 4, 14, 
                      5, 28, 16, 3, 17, 7, 8, 19, 4, 9, 
                      3, 11, 40, 6, 3, 13, 12, 4, 10, 3, 3))

# 5 merage data
melted_data<-melted_data%>%
  left_join(electoral_votes, by ="state")

# For calculation
# 
subdata1 <- melted_data %>%
  mutate(StateFull = state.name[match(state, state.abb)]) %>%
  group_by(Type, Date) %>%
  summarise(
    TotalTrial = n(),
    No_Republican = sum(value == 1, na.rm = TRUE),
    No_Democratic = sum(value == 0, na.rm = TRUE),
    .groups = 'drop'
  ) %>%
  mutate(
    Republican = No_Republican / TotalTrial,
    Democratic = No_Democratic / TotalTrial
  )



# Comments: sub1 data generates variable based on four types of avg group by Type and Date but does not take electoral votes into account
sub1 <- subdata1 %>%
  select(Date, Type, Democratic) %>%  # Select relevant columns
  pivot_wider(names_from = Type, values_from = Democratic, names_prefix = "avg_")%>% 
  arrange(Date)

# Comments: sub2 includes total observations for each type per day but does not take electoral votes into account
sub2 <-subdata1 %>%
  select(Date, Type, No_Democratic) %>%  # Select relevant columns
  pivot_wider(names_from = Type, values_from = No_Democratic, names_prefix = "Count_")%>% 
  arrange(Date)



# Comment: subdata2 creates the probability of aggregated state winner by Date and Type 
# Creating the second subset with state-specific totals and proportions
# ******************Percent_byState variable used for state-level time series 
subdata2 <- melted_data %>%
  group_by(Date, Type, state) %>%
  summarise(
    Percent_byState = mean(value, na.rm = TRUE),
    TotalTrial_byState = n(),
    No_Republican_State = sum(value == 1, na.rm = TRUE),
    No_Democratic_State = sum(value == 0, na.rm = TRUE),
    .groups = 'drop'
  )  %>% 
  mutate(Percent_byState_chr=sprintf("%1.2f%%", 100*Percent_byState))%>% 
  mutate(Percent_byState_Demo=1-Percent_byState)%>% 
  mutate(Percent_byState_chr_Demo=sprintf("%1.2f%%", 100*Percent_byState_Demo))


# Creating the second subset with state-specific totals,showing total trial for specific day with value 1 or 2
subdata3 <- melted_data %>%
  mutate(StateFull = state.name[match(state, state.abb)]) %>%
  group_by(Date, StateFull, Type, state, value) %>%
  summarise(
    TotalTrial_byStatebyParty = n()
  )


# Create the dataframe

# ***********Final dataset for state map and state level time series trend

extended_data <- melted_data %>%
  left_join(subdata1, by = c("Type", "Date")) %>%
  left_join(subdata2, by = c("Date","Type", "state"))%>%
  left_join(subdata3, by = c("Date","Type", "state", "value")) %>%
  mutate(Percent_byStateParty=TotalTrial_byStatebyParty/TotalTrial_byState) %>%
  mutate(Date=as.Date(Date)) %>%
  mutate(year = as.numeric(format(as.Date(Date), "%Y")))%>%
  mutate(abb=state)%>%
  mutate(Predicted_party=ifelse(Percent_byState>=0.5, "Republican", "Democratic")) 


# *********** For national level graph

# method #1 calculate the average winning probability in each state and assign the electoral votes
subdata2_1 <-subdata2%>%
  left_join(electoral_votes, by="state")%>%
  select(Date,Type,state,Percent_byState_Demo,Electoral_Votes)%>%
  #filter(Date=="2024-08-13")%>%
  mutate(proj_party=ifelse(Percent_byState_Demo>=0.5, "Democratic", "Republican")) 


subdata2_2 <-subdata2_1%>%
  group_by(Date, proj_party,Type) %>%
  summarize(
    Total_votes = sum(Electoral_Votes)
  )


# number should be 535 for now
trials<-subdata2_1 %>%
  group_by(Date,Type)%>%
  summarize(
    Votes = sum(Electoral_Votes)
  )

votes_trials_percent<-subdata2_2 %>%
  left_join(trials,by=c("Type","Date")) %>%
  mutate(votes_percent=Total_votes/Votes) %>%
  select(Date, Type,proj_party, votes_percent) %>%  
  pivot_wider(names_from = Type, values_from = votes_percent, names_prefix = "Votes_percent_") 

votes_trials<-subdata2_2 %>%
  select(Date, Type,proj_party, Total_votes) %>%  # Select relevant columns
  pivot_wider(names_from = Type, values_from = Total_votes, names_prefix = "Votes_")%>% 
  arrange(Date) 

subdata2_reshape <-votes_trials%>%
  left_join(votes_trials_percent, by=c("Date","proj_party")) %>% 
  filter(proj_party=="Democratic")%>%
  arrange(Date) %>%
  filter(proj_party=="Democratic")
  .groups = 'drop'

#*******************************************************************
subdata2_reshape <- subdata2_reshape %>%
  ungroup() 
#*******************************************************************

##method #2 assign the votes first and then calculate the average

subdata3_1<-melted_data %>%
  group_by(Type, Trial, Date,party) %>%
  summarise(
    Number_Repub_Win = n(),
    votes_party =sum(Electoral_Votes),
    .groups = 'drop'
  ) 


subdata3_1_2<-melted_data %>%
  group_by(Type, Date, party) %>%
  summarise(
    Number_Repub_Win = n(),
    votes_party =sum(Electoral_Votes),
    .groups = 'drop'
  ) %>%
  arrange(Date)

#calculate total number of votes
subdata3_2_2<-melted_data %>%
  group_by(Type, Date) %>%
  summarise(
    Votes =sum(Electoral_Votes),
    .groups = 'drop'
  ) 


subdata3_2_reshape<-subdata3_1_2%>%
  left_join(subdata3_2_2,by=c("Type","Date")) %>%
  mutate(Votes_perent=votes_party/Votes) %>%
  select(Date, Type,party, Votes_perent) %>%  # Select relevant columns
  pivot_wider(names_from = Type, values_from = Votes_perent, names_prefix = "Votes_")%>% 
  filter(party=="Democratic")%>%
  arrange(Date) 
.groups = 'drop'


# calculate total trials for each type each date for both parties, should be 100 per day for each type but we lose trials
trial_counts<-subdata3_1 %>%
  group_by(Type,Date) %>%
  mutate(trial_counts= n_distinct(Trial))%>%
  select(Type,Date,party,trial_counts)%>%
  distinct() # drop duplicates 
.groups = 'drop'

trial_votes<-melted_data %>%
  group_by(Type, Date,party) %>%
  summarise(
    votes_party =sum(Electoral_Votes),
    .groups = 'drop'
  ) %>%
  left_join(trial_counts, by=c("party","Type","Date" ))%>%
  mutate(average_votes=votes_party/trial_counts)


#total should be 535 for now and 538 later
total_votes_count <- trial_votes %>%
  select(Date, Type, average_votes) %>%  
  group_by(Type, Date)%>%
  summarise(
    votes_total=sum(average_votes)
  )
 
# merge two datasets before reshape
trial_votes <-trial_votes%>%
  left_join(total_votes_count, by=c("Type","Date" )) %>%
  mutate(average_votes = round(average_votes, digits = 0)) %>%  # Round average_votes
  mutate(average_votes_percent=average_votes/votes_total)

#*************** Dataset with predicted votes 
#* For table
average_votes<- trial_votes %>%
select(Date, Type, party, average_votes)%>%
  rename(
    Voice = Type,  # Renaming 'Result' to 'value'
    Party= party,
    AverageVotes=average_votes)

#For graph
average_votes_reshape <- trial_votes %>%
  select(Date, Type, party, average_votes) %>%  # Select relevant columns
  pivot_wider(names_from = Type, values_from = average_votes, names_prefix = "Votes_")


# For table
average_votes_percent<- trial_votes %>%
  select(Date, Type, party, average_votes_percent) %>%
  mutate(average_votes_percent = round(average_votes_percent, digits = 2))%>%
  rename(
    Voice = Type,  # Renaming 'Result' to 'value'
    Party= party,
    WinLikelihood=average_votes_percent)


# For graph
average_votes_percent_reshape<- trial_votes %>%
  select(Date, Type, party, average_votes_percent) %>%  # Select relevant columns
  pivot_wider(names_from = Type, values_from = average_votes_percent, names_prefix = "Votes_Percent_")

# Final Dataset for national level winner for average votes and average votes percent
#*******************************************************************
trial_votes_reshape <-average_votes_reshape%>%
  left_join(average_votes_percent_reshape, by=c("Date","party"))%>% 
  filter(party=="Democratic")%>%
  arrange(Date) 
#*******************************************************************

  

#------------Data  process done

#write_csv(extended_data,"/Users/sunmingrun/Desktop/AI Project/panel_election_results_help.csv")


#------color
pal <- pnw_palette(name = "Bay", n = 8, type = "continuous")
color_for_1 <- pal[8]  # Close to red
color_for_0 <- pal[1]  # Close to blue


pal2<- pnw_palette("Moth",5)
color_plot6_Repub <- pal2[3]

pal3<- pnw_palette("Sunset",5)
color_plot6_Demo <- pal3[1]

#data
#function 1 state unique label
state_group <- extended_data %>%
  select(StateFull) %>%
  arrange(StateFull) %>%
  distinct()

voice_group <- extended_data %>%
  select(Type) %>%
  arrange(Type) %>%
  distinct()

party_group <- extended_data %>%
  select(party) %>%
  arrange(party) %>%
  distinct()

filtered_data <- extended_data %>%
  filter(Date == "2024-08-15")

distinct <- filtered_data %>%
  distinct(Type,Date, No_Republican,No_Democratic,Republican,Democratic,TotalTrial_byState)







# FLUID DESIGN FUNCTION ---------------------------------------------------

fluid_design <- function(id, w, x, y, z) {
  fluidRow(
    div(
      id = id,
      column(
        width = 6,
        uiOutput(w),
        uiOutput(y)
      ),
      column(
        width = 6,
        uiOutput(x),
        uiOutput(z)
      )
    )
  )
}

# UI
ui <- dashboardPage(
  skin = "red",
  title = "Presidential Election",
  
  dashboardHeader(title = "Election GPT"),
  dashboardSidebar(
    width = 300,
    div(class = "inlay", style = "height:15px;width:100%;background-color: #ecf0f5;"),
    sidebarMenu(
      div(id = "sidebar_button",
          bsButton(inputId = "confirm", 
                   label = "START EXPLORE", 
                   icon = icon("play-circle"), 
                   style = "danger")
      ),
      div(class = "inlay", style = "height:15px;width:100%;background-color: #ecf0f5;"),
      menuItem(
        "STATES",
        tabName = "stateselect",
        icon = icon("spinner"),
        checkboxGroupButtons(
          inputId = "allInput",
          label = "CHOOSE GROUPS OF STATES",
          choices = "ALL / CLEAR",
          size = "sm",
          selected = "ALL / CLEAR"
        ),
        checkboxGroupInput(
          inputId = "statesInput",
          label = "",
          choices = state_group$StateFull
        )
      ),
      br(),
      br(),
      menuItem(
        "VOICE",
        tabName = "voice",
        icon = icon("user-md"),
        checkboxGroupInput(
          inputId = "voicechoice",
          label = "VOICECHOICE",
          choices = voice_group$Type,
          selected = "Direct"
        )
      ),
      br(),
      br(),
      menuItem(
        "YEAR",
        tabName = "year",
        icon = icon("calendar"),
        sliderInput(
          inputId = "yearInput",
          label = "Year",
          value = c(min(extended_data$year, na.rm = TRUE), max(extended_data$year, na.rm = TRUE)),
          min = min(extended_data$year, na.rm = TRUE),
          max = max(extended_data$year, na.rm = TRUE),
          step = 1L,
          sep = ""
        ),
        dateInput("date", "DAYS TO FIRST PREDICTION (USE SINGLE DATE TO PLOT MAP):",   
                  value = "2024-08-13"),
        dateRangeInput("date2", "START DATE TO END DATE ((USE SINGLE DATE TO PLOT MAP):",   
                       start = "2024-08-13",
                       end   = Sys.Date() )
      ),
      br(),
      br(),
      menuItem(
        "PARTY",
        tabName = "Party",
        icon = icon("male"),
        radioButtons(
          inputId = "partychoice",
          label = "PARTYCHOICE",
          choices = party_group$party,
          selected = "Republican"
        )
      )
    )
  ),
  
  dashboardBody(
    tags$head(
      tags$link(
        rel = "stylesheet", 
        type = "text/css", 
        href = "ElectionGPT_style.css")
    ),
    
    useShinyjs(),
    introjsUI(),
    
    # MAIN BODY ---------------------------------------------------------------
    
    fluidRow(
      column(
        width = 12,
        bsButton("map", 
                 label = "STATE LEVEL OUTCOME", 
                 icon = icon("map-marker"), 
                 style = "success"),
        bsButton("trend", 
                 label = "NATIONAL LEVEL OUTCOME", 
                 icon = icon("chart-bar"), 
                 style = "success"),
        bsButton("about", 
                 label = "ABOUT", 
                 icon = icon("gears"), 
                 style = "success")
      )
    ),
    
    
    
    fluidRow(
      div(
        id = "map_panel", 
        column(
          width = 12,
          uiOutput("box_pat6")
        ),
        column(
          width = 6,
          uiOutput("box_pat")
        ),
        column(
          width = 6,
          uiOutput("box_pat2")
        ),
        column(
          width = 6,
          uiOutput("box_pat3")
        ),
        column(
          width = 6,
          uiOutput("box_pat4")
        )
      ),
      
      
      fluidRow(
        div(
          id = "trend_panel", 
          column(
            width = 6,
            uiOutput("box_pat7")
        ),
        column(
          width = 6,
          uiOutput("box_pat8")
        ),
        column(
          width = 6,
          uiOutput("box_table1")
        ),
        column(
          width = 6,
          uiOutput("box_table2")
        )
      ),
      
      fluidRow(
        div(
          id="about_panel",
          column(
            width=6,
            style = "padding-left: 50px;", 
            h4(p("About the Project")),
            h5(p("The project began as an attempt to combine our interest in artificial intelligence, focusing on its predictive power and potential to shape the future.")),
            h5(p("Step 1: Pull 100 news stories from Event Registry: API Search for news stories related to the prompt: “2024 US presidential election”."),
               p("Step 2: Feed stories to Chat-GPT in 4 distinct voices：Four characters, each representing different perspectives, will generate 100 stories:"),
               h6(p("• Voice 1: Anonymous/Direct truthful reporter"),
                  p("• Voice 2: Fox Reporter Bret Baier"),
                  p("• Voice 3: MSNBC Reporter Rachel Maddow"),
                  p("• Voice 4: BBC Reporter Laura Kuenssberg"),
               ),
               p("Step 3: Generate election stories from each character’s perspective:For each character, 100 stories are written about the election outcome in each state."),
               p("Step 4: Extract the election winners from each story: Use GPT to extract only the name of the winners from the stories for each character."),
               p("Step 5: Save winners and display percentage of daily trials that went to each party in each state: 1 = Trump/Republican and 0 = Harris/Democrat"),
               p("Step 6: Repeat the process daily, appending new results to the previous day’s data panel.")
            ),
            br(),
            h5(p("We hope you find it interesting and/or useful.  Any comments or questions are welcome at email address"),
               p("The source for this project is available ", a("on github", href = "https://github.com/"), ".")),
            #hr(),
            
          ),
          column(6,
                 #br(),
                 # HTML('<img src="GregPicCrop.png", height="110px"
                 # style="float:right"/>','<p style="color:black"></p>'),
                 h4(p("About the Author")),
                 h5(p("Scott Cunningham is the Ben H. Williams Professor of Economics at Baylor University.  He specializes in a range of topics in applied microeconomics, such as mental illness, drug policy and sex work."),
                    p("Jared Black"),
                    p("Coco Mingrun Sun")
                 )
          ),
          fluidRow(
            column(12,
                   div(style="text-align: center;",
                       imageOutput("home_img", width = "50%", height = "auto")
                   )
            )
          )
        )
      )
    )
  )
) 
)


server <- function(input, output, session) {
  
  
  update_all <- function(x) {
    updateSelectInput(session, "tab",
                      choices = c("", "Maps", "Trends", "About"),
                      label = "",
                      selected = x
    )
  }
  
  
  
  observeEvent(input$map, {
    update_all("Maps")
  })
  observeEvent(input$trend, {
    update_all("Trends")
  })
  observeEvent(input$diagnostics, {
    update_all("About")
  })
 
  
  # update confirm button
  
  observeEvent(input$confirm, {
    updateButton(
      session, 
      inputId = "confirm", 
      label = "ENJOY EXPLORE", 
      icon = icon("bar-chart"), 
      style = "primary")
  })
  
  # hide the underlying selectInput in sidebar for better design
  observeEvent("", {
    hide("tab")
  })
  
  # update all/none group in sidebar
  observe({
    x <- input$allInput
    if (!is.null(x)) {
      x <- c("North Carolina","Georgia") #character(0) 
    }
    else {
      x <- state_group$StateFull
    }
    
    updateCheckboxGroupInput(
      session,
      "statesInput",
      label = NULL, 
      choices = state_group$StateFull,
      selected = x
    )
  })
  
  
  # DYNAMIC RENDER RULES ----------------------------------------------------
  
  observeEvent("", {
    show("map_panel")
    hide("trend_panel")
    hide("about_panel")
    #hide("outcome_panel")
  }, once = TRUE)
  
  
  observeEvent(input$map, {
    show("map_panel")
    hide("trend_panel")
    hide("about_panel")
    #hide("outcome_panel")
  })
  observeEvent(input$trend, {
    show("trend_panel")
    hide("about_panel")
    #hide("outcome_panel")
    hide("map_panel")
  })
  observeEvent(input$about, {
    show("about_panel")
    hide("trend_panel")
    #hide("outcome_panel")
    hide("map_panel")
  })
  

  
  # show active button with color
  
  observeEvent(input$tab, {
    x <- input$tab
    updateButton(session, "map", style = {
      if (x == "Maps") {
        paste("warning")
      } else {
        paste("success")
      }
    })
    updateButton(session, "trend", style = {
      if (x == "Trends") {
        paste("warning")
      } else {
        paste("success")
      }
    })
    updateButton(session, "about", style = {
      if (x == "About") {
        paste("warning")
      } else {
        paste("success")
      }
    })

  })
  
  
  ######################################3
  
  # UI - Map - 1 ----------------------------------------------------------
  
  # Reactive function for USA map data
  USA_map_Anonymous <- reactive({
    req(input$date)
    #req(input$USMap_state)
    filter(extended_data, Type=="Direct") %>%
      filter(Date %in% input$date)%>%
      #filter(StateFull %in% input$USMap_state)%>%
      mutate(party_numeric= ifelse(Predicted_party == "Republican", 1, 0))%>%
      mutate(new_variable = runif(n(), min = 0, max = 50))%>%
      mutate(hover = paste(state, '<br>', 
                           "State:", StateFull, "<br>",
                           "Date:", Date, "<br>",
                           "Type:", Type, "<br>",
                           "Predicted Party:", Predicted_party,"<br>",
                           "Percent",Percent_byState_chr))
  })
  
  
  
  
  # UI - Map - 2 ----------------------------------------------------------
  
  # Reactive function for USA map data
  USA_map_BBC <- reactive({
    req(input$date)
    #req(input$USMap_state)
    filter(extended_data, Type=="BBC") %>%
      filter(Date %in% input$date)%>%
      #filter(StateFull %in% input$USMap_state)%>%
      mutate(party_numeric= ifelse(Predicted_party == "Republican", 1, 0))%>%
      mutate(new_variable = runif(n(), min = 0, max = 50))%>%
      mutate(hover = paste(state, '<br>', 
                           "State:", StateFull, "<br>",
                           "Date:", Date, "<br>",
                           "Type:", Type, "<br>",
                           "Predicted Party:", Predicted_party,"<br>",
                           "Percent",Percent_byState_chr))
  })
  
  # UI - Map - 3 ----------------------------------------------------------
  
  # Reactive function for USA map data
  USA_map_Fox <- reactive({
    req(input$date)
    #req(input$USMap_state)
    filter(extended_data, Type=="Fox") %>%
      filter(Date %in% input$date)%>%
      #filter(StateFull %in% input$USMap_state)%>%
      mutate(party_numeric= ifelse(Predicted_party == "Republican", 1, 0))%>%
      mutate(new_variable = runif(n(), min = 0, max = 50))%>%
      mutate(hover = paste(state, '<br>', 
                           "State:", StateFull, "<br>",
                           "Date:", Date, "<br>",
                           "Type:", Type, "<br>",
                           "Predicted Party:", Predicted_party,"<br>",
                           "Percent",Percent_byState_chr))
  })
  
  # UI - Map - 4 ----------------------------------------------------------
  
  # Reactive function for USA map data
  USA_map_MSNBC <- reactive({
    req(input$date)
    #req(input$USMap_state)
    filter(extended_data, Type=="MSNBC") %>%
      filter(Date %in% input$date)%>%
      #filter(StateFull %in% input$USMap_state)%>%
      mutate(party_numeric= ifelse(Predicted_party == "Republican", 1, 0))%>%
      mutate(new_variable = runif(n(), min = 0, max = 50))%>%
      mutate(hover = paste(state, '<br>', 
                           "State:", StateFull, "<br>",
                           "Date:", Date, "<br>",
                           "Type:", Type, "<br>",
                           "Predicted Party:", Predicted_party,"<br>",
                           "Percent",Percent_byState_chr))
  })
  
  
  
  # UI 5- Time Series 1 ---DISCARD
  
  Count_data_4Voice <- reactive({
    #req(input$TimeseriesVoices) 
    req(input$date2)
    filter(trial_votes_reshape,Date >= input$date2[1] & Date <= input$date2[2])
  })
  
  
  # UI 6- Time Series 2
  Count_data <- reactive({
    req(input$voicechoice) 
    req(input$statesInput)
    req(input$date2)
    #req(input$partychoice)
    filter(extended_data, Type %in% input$voicechoice) %>%
      filter(StateFull %in% input$statesInput) %>%
      filter(Date >= input$date2[1] & Date <= input$date2[2]) %>%
      arrange(Date)
    #%>%filter(party %in% input$partychoice)
    
  })
  
  # UI 7- Time Series 1 Calculate the average and assign the votes --DISCARD
  Votes_data_4Voice <- reactive({
    #req(input$TimeseriesVoices) 
    req(input$date2)
    filter(subdata2_reshape,Date >= input$date2[1] & Date <= input$date2[2])
  })
  
  
  # UI 8- Time Series 1 Assign the votes and calculate the average---DISCARD
  
  Votes_data_4Voice_2 <- reactive({
    #req(input$TimeseriesVoices) 
    req(input$date2)
    filter(trial_votes_reshape,Date >= input$date2[1] & Date <= input$date2[2])
  })
  
  
  # UI 9 & 10 both average votes and average votes percent use this function
  Votes_final <-reactive({
    req(input$date2)
    filter(trial_votes_reshape,Date >= input$date2[1] & Date <= input$date2[2])
  })
  

  # UI Table 1 average votes 
  Table1_votes <-reactive({
    #filter(average_votes,Type %in%  input$box_table1)
    
    datatable(
      average_votes, 
      rownames = FALSE, 
      extensions = "Buttons",
      options = list(
        dom = 'Bfrtp',
        buttons = c('csv', 'excel', 'pdf'),
        style = "bootstrap",
        lengthMenu = c(seq(5, 150, 5))
      )
    )

  })
  
  # UI Table 2 average votes perentage
  Table2_votes_percent <-reactive({
    #filter(average_votes,Type %in%  input$box_table1)
    
    datatable(
      average_votes_percent, 
      rownames = FALSE, 
      extensions = "Buttons",
      options = list(
        dom = 'Bfrtp',
        buttons = c('csv', 'excel', 'pdf'),
        style = "bootstrap",
        lengthMenu = c(seq(5, 150, 5))
      )
    )
    
  })
 
  

  # Render UI -Map 1
  
  output$box_pat <- renderUI({
    div(
      style = "position: relative; backgroundColor: #ecf0f5",
      tabBox(
        id = "box_pat",
        width = NULL,
        height = 320,
        tabPanel(
          title = "United States Map Projected Anonymous"
        ),
        withSpinner(
          plotlyOutput("box_map_anonymous", height = 230),
          type = 4,
          color = "#d33724", 
          size = 0.7 
        )
      )
    )
  })
  
  
  
  # UI - Map - 2 BBC-------------------------------------------------------
  output$box_pat2 <- renderUI({
    div(
      style = "position: relative; backgroundColor: #ecf0f5",
      tabBox(
        id = "box_pat",
        width = NULL,
        height = 320,
        tabPanel(
          title = "United States Map Projected BBC"
        ),
        withSpinner(
          plotlyOutput("box_map_BBC", height = 230),
          type = 4,
          color = "#d33724", 
          size = 0.7 
        )
      )
    )
  })
  
  # UI - Map 3 - Fox -------------------------------------------------------
  output$box_pat3 <- renderUI({
    div(
      style = "position: relative; backgroundColor: #ecf0f5",
      tabBox(
        id = "box_pat",
        width = NULL,
        height = 320,
        tabPanel(
          title = "United States Map Projected Fox"
        ),
        withSpinner(
          plotlyOutput("box_map_Fox", height = 230),
          type = 4,
          color = "#d33724", 
          size = 0.7 
        )
      )
    )
  })
  
  # UI - MSNBC - 4 -------------------------------------------------------
  output$box_pat4 <- renderUI({
    div(
      style = "position: relative; backgroundColor: #ecf0f5",
      tabBox(
        id = "box_pat",
        width = NULL,
        height = 320,
        tabPanel(
          title = "United States Map Projected MSNBC"
        ),
        withSpinner(
          plotlyOutput("box_map_MSNBC", height = 230),
          type = 4,
          color = "#d33724", 
          size = 0.7 
        )
      )
    )
  })
  
  
  # UI - Time Trend - 1 ------------------------------------------------------------------
  output$box_pat5 <- renderUI({
    div(
      style = "position: relative; backgroundColor: #ecf0f5",
      tabBox(
        id = "box_pat",
        width = NULL,
        height = 320,
        tabPanel(
          title = "Projected Democrat Electoral College Victory Likelihood"
        ),
        withSpinner(
          plotlyOutput("plot_Overall", height = 250),
          type = 4,
          color = "#d33724", 
          size = 0.7
        )
      )
    )
  })
  
  
  # Time Trend - 2 ------Discard------------------------------------------------------------
  output$box_pat6 <- renderUI({
    div(
      style = "position: relative; backgroundColor: #ecf0f5",
      tabBox(
        id = "box_pat",
        width = NULL,
        height = 320,
        tabPanel(
          title = "State Projected Democrat Electoral College Victory Likelihood"
        ),
        withSpinner(
          plotlyOutput("plot_state", height = 230),
          type = 4,
          color = "#d33724", 
          size = 0.7 
        )
      )
    )
  })
  
  # Time Trend - 3 ------------------------------------------------------------------
  output$box_pat7 <- renderUI({
    div(
      style = "position: relative; backgroundColor: #ecf0f5",
      tabBox(
        id = "box_pat",
        width = NULL,
        height = 320,
        tabPanel(
          title = "Projected Democrat Electoral College Votes"
        ),
        withSpinner(
          plotlyOutput("distPlot", height = 230),
          type = 4,
          color = "#d33724", 
          size = 0.7 
        )
      )
    )
  })
  
  # Time Trend - 4 ------------------------------------------------------------------
  output$box_pat8 <- renderUI({
    div(
      style = "position: relative; backgroundColor: #ecf0f5",
      tabBox(
        id = "box_pat",
        width = NULL,
        height = 320,
        tabPanel(
          title = " Projected Democrat Electoral College Victory Likelihood"
        ),
        withSpinner(
          plotlyOutput("distPlot2", height = 230),
          type = 4,
          color = "#d33724", 
          size = 0.7 
        )
      )
    )
})
  # UI Table 1 votes
  output$box_table1 <- renderUI({
    div(
      style = "position: relative",
      tabBox(
        id = "box_table1",
        width = NULL,
        height = 400,
        tabPanel(
          title = "Projected Votes"
          ),
          withSpinner(
            DT::dataTableOutput("table1_votes", height = 300),
            type = 4,
            color = "#d33724",
            size = 0.7
          )
        )
      )
  
  })
  
  # UI Table 2 percentage
  output$box_table2 <- renderUI({
    div(
      style = "position: relative",
      tabBox(
        id = "box_table2",
        width = NULL,
        height = 400,
        tabPanel(
          title = "Projected Win Likelihood"
        ),
        withSpinner(
          DT::dataTableOutput("table2_votes_percent", height = 300),
          type = 4,
          color = "#d33724",
          size = 0.7
        )
      )
    )
    
  })
  
  
  #         Output
  #-------Map 1 Anonymous
  output$box_map_anonymous <- renderPlotly ({
    
    #input$confirm
    input$date
    
    l <- list(color = toRGB("white"), width = 1)
    
    g <- list(
      scope = 'usa',
      projection = list(type = 'albers usa'),
      showlakes = FALSE
    )
    
    fig <- plot_geo(USA_map_Anonymous(), locationmode = 'USA-states', marker = list(line = l)
    )
    fig <- fig %>% add_trace(
      z = ~party_numeric,  # Map the party numeric variable (0 or 1)
      text = ~hover,       # Hover text with details
      locations = ~state,  # State abbreviations
      color = ~party_numeric, # Color based on the party
      colors = c(color_for_0, color_for_1),
      showscale = FALSE
    )
    
    
    fig <- fig %>% layout(
      geo = g
    )
    
    fig
  })  
  
  
  #-------Map 2 BBC
  
  # Render the Plotly map #1
  output$box_map_BBC <- renderPlotly ({
    input$date
    #input$confirm
    
    l <- list(color = toRGB("white"), width = 1)
    
    g <- list(
      scope = 'usa',
      projection = list(type = 'albers usa'),
      showlakes = FALSE
    )
    
    fig <- plot_geo(USA_map_BBC(), locationmode = 'USA-states', marker = list(line = l)
    )
    fig <- fig %>% add_trace(
      z = ~party_numeric,  # Map the party numeric variable (0 or 1)
      text = ~hover,       # Hover text with details
      locations = ~state,  # State abbreviations
      color = ~party_numeric, # Color based on the party
      colors = c(color_for_0, color_for_1),
      showscale = FALSE
    )
    
    
    fig <- fig %>% colorbar(title = "Party", tickvals = c(0, 1), ticktext = c("Democratic", "Republican"))
    
    fig <- fig %>% layout(
      geo = g
    )
    
    fig
  })  
  
  
  #-------Map 3 Fox
  
  # Render the Plotly map #1
  output$box_map_Fox <- renderPlotly ({
    input$date
   
    l <- list(color = toRGB("white"), width = 1)
    
    g <- list(
      scope = 'usa',
      projection = list(type = 'albers usa'),
      showlakes = FALSE
    )
    
    fig <- plot_geo(USA_map_Fox(), locationmode = 'USA-states', marker = list(line = l)
    )
    fig <- fig %>% add_trace(
      z = ~party_numeric,  # Map the party numeric variable (0 or 1)
      text = ~hover,       # Hover text with details
      locations = ~state,  # State abbreviations
      color = ~party_numeric, # Color based on the party
      colors = c(color_for_0, color_for_1),
      showscale = FALSE# Democratic = blue, Republican = red
    )
    
    
    fig <- fig %>% layout(
      geo = g
    )
    
    fig
  })  
  
  #-------Map 4 MSNBC
  
  # Render the Plotly map #1
  output$box_map_MSNBC <- renderPlotly ({
    input$date

    l <- list(color = toRGB("white"), width = 1)
    
    g <- list(
      scope = 'usa',
      projection = list(type = 'albers usa'),
      showlakes = FALSE
    )
    
    fig <- plot_geo(USA_map_MSNBC(), locationmode = 'USA-states', marker = list(line = l)
    )
    fig <- fig %>% add_trace(
      z = ~party_numeric,  # Map the party numeric variable (0 or 1)
      text = ~hover,       # Hover text with details
      locations = ~state,  # State abbreviations
      color = ~party_numeric, # Color based on the party
      colors = c(color_for_0, color_for_1),
      showscale = FALSE
    )
    
    
    fig <- fig %>% colorbar(title = "Party", tickvals = c(0, 1), ticktext = c("Democratic", "Republican"))
    
    fig <- fig %>% layout(
      geo = g
    )
    
    fig
  })  
  
  
  # UI #5 Overall need to be revised with add DNC ---Discard
  output$plot_Overall<- renderPlotly({
    input$date2
    #input$confirm
    
    fig <- plot_ly(Count_data_4Voice(), x = ~Count_data_4Voice()$Date, y = ~avg_Direct, name = 'Proj Anonymous', type = 'scatter', mode = 'lines',
                   line = list(color = 'rgb(205, 12, 24)', width = 4)) 
    fig <- fig %>% add_trace(y = ~avg_BBC, name = 'Proj BBC', line = list(color = 'rgb(22, 96, 167)', width = 4)) 
    fig <- fig %>% add_trace(y = ~avg_Fox, name = 'ProjFox', line = list(color = 'rgb(205, 12, 24)', width = 4, dash = 'dash')) 
    fig <- fig %>% add_trace(y = ~avg_MSNBC, name = 'Proj MSNBC', line = list(color = 'rgb(22, 96, 167)', width =4 , dash = 'dot')) %>%
      layout(
        title = NULL,
        xaxis = list(title = "Date",
                     showgrid = FALSE),
        yaxis = list(title = "Proj Democrat Win Percent", 
                     range = c(0.4, 0.6),
                     showgrid = FALSE),
        shapes = list(
          type = "rect",
          fillcolor = "rgba(22, 96, 167, 0.2)", # Adjust fill color and transparency as needed
          line = list(color = "rgba(22, 96, 167, 0)"), # No borderline = list(color = "rgba(22, 96, 167, 0)"), # No border
          x0 = as.Date("2024-08-19"), x1 = Sys.Date() ,
          y0 = 0.4, y1 = 0.6
        ))%>%
      layout(annotations = list(
        list(
          x = min(Count_data_4Voice()$Date+5),
          y = 0.55,
          text = "Democrat Win",
          showarrow = FALSE,
          font = list(size = 12, weight = "bold", color = color_plot6_Demo),
          showgrid = FALSE
        ),
        list(
          x = min(Count_data_4Voice()$Date+5),
          y = 0.45,
          text = "Republican Win",
          showarrow = FALSE,
          font = list(size = 12, weight = "bold", color = color_plot6_Repub)
        )
      ))
  })
  
  # UI #6 Time series by state 
  output$plot_state <- renderPlotly({
    input$allInput
    input$voicechoice
    input$statesInput
    input$date2
    #input$confirm
    #input$partychoice
    
    isolate({
      if (length(Count_data()$party) ==0) {
        fig <- ggplotly(
          ggplot(data.frame(x = 1), aes(x = x)) +
            ggtitle("No party fits selected characteristics. \nPlease modify selections.") +
            theme_void() +
            theme(plot.title = element_text(face = "bold", color = "#FF8D1E", size = 12))
        )
      } else {
        # Initialize the plotly object
        fig <- plot_ly()
        # Loop through each selected state and add a trace for it
        for (k in seq_along(input$statesInput)){
          state_data <- Count_data()[Count_data()$StateFull == input$statesInput[k], ]
          fig <- add_trace(fig, data = state_data, 
                           x = ~Date, y = ~Percent_byState_Demo, type = 'scatter', mode = 'lines',
                           name = input$statesInput[k],
                           line = list(width = 2)) %>%
            layout(
              xaxis = list(title = "Date",
                           showgrid = FALSE,
                           zeroline = FALSE,
                           showline = FALSE),
              #tickformat = "%b %d, %Y"),
              yaxis = list(
                title = "Proj Democrat Win Percent",
                showline = TRUE,
                showgrid = FALSE,
                showticklabels = TRUE,
                tickformat = ".0%",  # Format ticks as percentage
                range = c(-0.1, 1.1),
                tickmode = "linear",
                tick0 = 0,
                dtick = 0.2
              )
            )
        }
        fig  # Return the plotly figure
      }
    })
  })
  
  output$distPlot <- renderPlotly({
    input$date2
    #input$confirm
    
  fig <- plot_ly(Votes_final(), x = ~Date, y = ~Votes_Direct, name = 'Proj Anonymous', type = 'scatter', mode = 'lines',
                 line = list(color = 'rgb(205, 12, 24)', width = 4)) 
  fig <- fig %>% add_trace(y = ~Votes_BBC, name = 'Proj BBC', line = list(color = 'rgb(22, 96, 167)', width = 4)) 
  fig <- fig %>% add_trace(y = ~Votes_Fox, name = 'Proj Fox', line = list(color = 'rgb(205, 12, 24)', width = 4, dash = 'dash')) 
  fig <- fig %>% add_trace(y = ~Votes_MSNBC, name = 'Proj MSNBC', line = list(color = 'rgb(22, 96, 167)', width = 4, dash = 'dot')) %>%
    layout(
      title = NULL,
      xaxis = list(title = "Date",
                   showgrid = TRUE),
      yaxis = list(title = "Proj Democrat Win Votes", 
                   range = c(140, 400),
                   showgrid = FALSE),
      shapes = list(
        list(
          type = "rect",
          fillcolor = "rgba(205, 12, 24, 0.2)", # Light red fill for 140-270
          line = list(color = "rgba(205, 12, 24, 0)"), # No border
          x0 = min(Votes_final()$Date), x1 = max(Votes_final()$Date),
          y0 = 140, y1 = 270
        ),
        list(
          type = "rect",
          fillcolor = "rgba(22, 96, 167, 0.2)", # Light blue fill for 270-400
          line = list(color = "rgba(22, 96, 167, 0)"), # No border
          x0 = min(Votes_final()$Date), x1 = max(Votes_final()$Date),
          y0 = 270, y1 = 400
        ),
        list(
          type = "line",
          x0 = min(Votes_final()$Date), x1 = max(Votes_final()$Date),
          y0 = 270, y1 = 270,
          line = list(color = "rgb(0, 0, 0)", dash = 'dash', width = 2)
        ),
        list(
          type = "line",
          x0 = as.Date("2024-08-19"), x1 = as.Date("2024-08-19"),  # Vertical line for DNC
          y0 = 140, y1 = 400,
          line = list(color = "rgb(0, 0, 0)", width = 2)
        )
      )
    ) %>%
    layout(annotations = list(
      list(
        x = min(Votes_final()$Date) + 5,
        y = 350,
        text = "Democrat Win",
        showarrow = FALSE,
        font = list(size = 12, weight = "bold", color = "rgb(22, 96, 167)"),
        showgrid = FALSE
      ),
      list(
        x = min(Votes_final()$Date) + 5,
        y = 200,
        text = "Republican Win",
        showarrow = FALSE,
        font = list(size = 12, weight = "bold", color =  "rgb(205, 12, 24)")
      ),
      list(
        x = as.Date("2024-08-20"),  # Position DNC label to the right of the vertical line
        y = 160,                    # Position within the light red area
        text = "DNC",
        showarrow = FALSE,
        font = list(size = 12, weight = "bold", color = "rgb(0, 0, 0)")
      )
    ))
})
  
  output$distPlot2 <- renderPlotly({
    input$date2
    #input$confirm
    
    fig <- plot_ly(Votes_final(), x = ~Date, y = ~Votes_Percent_Direct, name = 'Proj Anonymous', type = 'scatter', mode = 'lines',
                   line = list(color = 'rgb(205, 12, 24)', width = 4)) 
    fig <- fig %>% add_trace(y = ~Votes_Percent_BBC, name = 'Proj BBC', line = list(color = 'rgb(22, 96, 167)', width = 4)) 
    fig <- fig %>% add_trace(y = ~Votes_Percent_Fox, name = 'Proj Fox', line = list(color = 'rgb(205, 12, 24)', width = 4, dash = 'dash')) 
    fig <- fig %>% add_trace(y = ~Votes_Percent_MSNBC, name = 'Proj MSNBC', line = list(color = 'rgb(22, 96, 167)', width = 4, dash = 'dot')) %>%
      layout(
        title = NULL,
        xaxis = list(title = "Date",
                     showgrid = TRUE),
        yaxis = list(title = "Proj Democrat Win Percent", 
                     range = c(0.3, 0.7),
                     showgrid = FALSE),
        shapes = list(
          list(
            type = "rect",
            fillcolor = "rgba(205, 12, 24, 0.2)", # Light red fill for 140-270
            line = list(color = "rgba(205, 12, 24, 0)"), # No border
            x0 = min(Votes_final()$Date), x1 = max(Votes_final()$Date),
            y0 =0.3, y1 = 0.5
          ),
          list(
            type = "rect",
            fillcolor = "rgba(22, 96, 167, 0.2)", # Light blue fill for 270-400
            line = list(color = "rgba(22, 96, 167, 0)"), # No border
            x0 = min(Votes_final()$Date), x1 = max(Votes_final()$Date),
            y0 = 0.5, y1 = 0.7
          ),
          list(
            type = "line",
            x0 = min(Votes_final()$Date), x1 = max(Votes_final()$Date),
            y0 = 0.5, y1 = 0.5,
            line = list(color = "rgb(0, 0, 0)", dash = 'dash', width = 2)
          ),
          list(
            type = "line",
            x0 = as.Date("2024-08-19"), x1 = as.Date("2024-08-19"),  # Vertical line for DNC
            y0 = 0.3, y1 = 0.7,
            line = list(color = "rgb(0, 0, 0)", width = 2)
          )
        )
      ) %>%
      layout(annotations = list(
        list(
          x = min(Votes_final()$Date) + 5,
          y = 0.65,
          text = "Democrat Win",
          showarrow = FALSE,
          font = list(size = 12, weight = "bold", color = "rgb(22, 96, 167)"),
          showgrid = FALSE
        ),
        list(
          x = min(Votes_final()$Date) + 5,
          y = 0.4,
          text = "Republican Win",
          showarrow = FALSE,
          font = list(size = 12, weight = "bold", color =  "rgb(205, 12, 24)")
        ),
        list(
          x = as.Date("2024-08-20"),  # Position DNC label to the right of the vertical line
          y = 0.3,                    # Position within the light red area
          text = "DNC",
          showarrow = FALSE,
          font = list(size = 12, weight = "bold", color = "rgb(0, 0, 0)")
        )
      ))
  })
  
  
  # Table 1
  output$table1_votes <- DT::renderDataTable({
    input$box_table1
    Table1_votes()
  }, server = FALSE)
  
  # Table 2
  output$table2_votes_percent <- DT::renderDataTable({

    Table2_votes_percent()
  }, server = FALSE)
  

  
}





shinyApp(ui, server)