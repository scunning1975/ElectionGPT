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

library(rsconnect)


# download automate process


library(httr)

#Step 1
download_panel_data <- function() {
  url <- "https://raw.githubusercontent.com/scunning1975/KamalaGPT/main/news_data/election_news/panelel-ection_results_state.csv"
  destfile <- "/Users/sunmingrun/Documents/GitHub/ElectionGPT/ShinyApp/panel_data.csv"

  GET(url, write_disk(destfile, overwrite = TRUE))
}

#Step 2: Use Git to add, commit, and push changes to GitHub
push_to_github <- function() {
  # Navigate to your local GitHub repository
  setwd("/Users/sunmingrun/Documents/GitHub/ElectionGPT/ShinyApp")
  
  # Add the updated files to the staging area
  system("git add .")  # Add all changed files, or you can specify the specific file: system('git add path_to_file')
                   
  # Commit the changes with a message
  system("git commit -m 'Automated update of panel_data.csv'")
  
  # Push the changes to GitHub
  system("git push origin main")
  
  message("Changes pushed to GitHub successfully.")
}

# Step 3: Automate the process by running both functions
automate_process <- function() {
  download_panel_data()
  push_to_github()
}

# Run the entire process
automate_process()



#data<-read_csv("panel_election_results_state_final_fixed.csv",show_col_types = FALSE)
#data<-read_csv("panel_election_results_state.csv",show_col_types = FALSE)
data<-read_csv("election_panel_dataset.csv",show_col_types = FALSE)


rsconnect::setAccountInfo(name='pregptdiction',
                          token='8AF4A8FFA3DE9C3227A9308BB61CB584',
                          secret='TVVaFnWHhaU7l/TUKn3zZyju6dbpXJULIa0QfP9J')

#rsconnect :: deployApp(server="shinyapps.io",appName = "Shiny2",forceUpdate = TRUE)

# 2 change the variable name and assigned the predicted party based on the result
melted_data<-data%>%
  rename(
    value = Result,  # Renaming 'Result' to 'value'
    state = State,
    Type= Voice# Renaming 'State' to 'state'
  )%>%
  mutate(party = ifelse(value ==1, "Republican", "Democratic")) %>%
  mutate(Type = ifelse(Type == "direct", "Direct", Type)) %>%
  mutate(Date = as.Date(Date, format = "%m/%d/%y")) %>%
  mutate(Type = ifelse(Type == "Direct", "Anonymous", Type))

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
  mutate(StateFull = state.name[match(state, state.abb)])%>%
  mutate(StateFull = ifelse(state == "DC", "District of Columbia", StateFull))%>%
  group_by(Date, Type, state, StateFull) %>%
  summarise(
    Percent_byState = mean(value, na.rm = TRUE),
    TotalTrial_byState = n(),
    No_Republican_State = sum(value == 1, na.rm = TRUE),
    No_Democratic_State = sum(value == 0, na.rm = TRUE),
    .groups = 'drop'
  )  %>% 
  mutate(Percent_byState_chr=sprintf("%1.2f%%", 100*Percent_byState))%>% 
  mutate(Percent_byState_Demo=round(1-Percent_byState, digits=3))%>% 
  mutate(Percent_byState_chr_Demo=sprintf("%1.2f%%", 100*Percent_byState_Demo))

# Creating the second subset with state-specific totals,showing total trial for specific day with value 1 or 2
subdata3 <- melted_data %>%
  mutate(StateFull = state.name[match(state, state.abb)]) %>%
  group_by(Date, StateFull, Type, state, value) %>%
  summarise(
    TotalTrial_byStatebyParty = n()
  )
  .groups = 'drop'


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
  mutate(Predicted_party=ifelse(Percent_byState>=0.5, "Republican", "Democratic")) %>%
  select(-StateFull.y) %>%
  rename(
         StateFull=StateFull.x) 


extended_data2<-extended_data%>%
  select(Date,Type,party,StateFull,state,Percent_byState_Demo,Percent_byState_chr_Demo,Percent_byStateParty,Predicted_party)%>%
  distinct(Date,Type,party,StateFull,state,Percent_byState_Demo,Percent_byState_chr_Demo,Percent_byStateParty,Predicted_party)


#extended_data2_check<-melted_data%>%
  #filter(Type=="Fox")%>%
  #filter(state=="NC")%>%
  #filter(Date=="2024-8-20")

#View(extended_data2_check)
# Create the dc_data with DC information for each Date and Type
dates <-unique(extended_data2$Date)

dc_data <- expand.grid(
  state = "DC",
  StateFull = "District of Columbia",
  party = "Democratic",
  Predicted_party = "Democratic",
  Percent_byState_Demo = 1,
  Percent_byState_chr_Demo = "100.00%",
  Percent_byStateParty = 1,
  Type = c("Direct", "MSNBC", "BBC", "Fox"),  # The four types
  Date = dates
)


# Ensure the column order is the same as in extended_data2


# Combine the original data with the dc_data and ensure it is sorted by Date, Type, and state
extended_data2 <- rbind(extended_data2, dc_data) %>%
  arrange(Date, Type, state)


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

#-------------import expert data
expert <-read_csv("Expert_Opinions.csv",show_col_types = FALSE)

expert_data<-expert%>%
  rename(
  Date = date,  # Renaming 'Result' to 'value'
)%>%
  mutate(Date = as.Date(Date, format = "%m/%d/%y")) %>%
  mutate(Silver = round(Silver/100,digit=2)) %>%
  mutate(Times = round(Times/100,digit=2)) %>%
  mutate(party="Democratic")

expert_all<-average_votes_percent_reshape%>%
  filter(party=="Democratic") %>%
  left_join(expert_data,by=c("Date","party"))
           

#------------Data  process done

#write_csv(extended_data,"/Users/sunmingrun/Desktop/AI Project/panel_election_results_help.csv")


#------color
pal <- pnw_palette(name = "Bay", n = 16, type = "continuous")
color_for_1 <- pal[16]  # Close to red
color_for_0 <- pal[1]  # Close to blue


#pal2<- pnw_palette("Moth",12)
#color_for_low1 <- pal2[6]

#pal4<-pnw_palette(name = "Bay", n = 16, type = "continuous")
#color_for_low0 <- pal4[5]


pal2<-pnw_palette(name = "Bay", n = 16, type = "continuous")
color_for_low1 <- pal2[13]


pal3<- pnw_palette("Shuksan2",5)
color_for_low0 <- pal3[2]


pal5 <- pnw_palette("Winter",100)
color_for_05 <- pal5[97]




custom_colorscale <- list(
  list(0, "#DD4124"),       # Red at the low end (0) (Democrats)
  list(0.25, color_for_low1),    # Lighter red between 0 and 0.5
  list(0.5, color_for_05),     # Very light red/pink at the middle (0.5) "#ffe6e6"
  list(0.75, color_for_low0),    # Lighter blue between 0.5 and 1
  list(1, "#00496F")        # Blue at the high end (1) (Republicans)
)
#data
#function 1 state unique label
state_group <- subdata2 %>%
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





steps<-read_csv2("help.csv",show_col_types = FALSE)
intro <- read_csv2("intro.csv",show_col_types = FALSE)


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
  
  # HEADER ------------------------------------------------------------------
    
  dashboardHeader(
    title = span("Election GPT"),
    titleWidth = 300,
    dropdownMenu(
      type = "notifications", 
      headerText = strong("HELP"), 
      icon = icon("question"), 
      badgeStatus = NULL,
      notificationItem(
        text = "1: Select the states for which you want to see the winning probability for each party.",
        icon = icon("spinner")
      ),
      notificationItem(
        text = "2: Choose the reporting voice to generate election-result stories.",
        icon = icon("user-md")
      ),
      notificationItem(
        text = "3: Select a single date for the map and use a date range for the state average Democratic victory graph.",
        icon = icon("calendar")
      )
    ),
    tags$li(
      a(
        strong("ABOUT ElectionGPT"),
        height = 40,
        href = "https://github.com/scunning1975/ElectionGPT/blob/main/README.md",  # Make sure this link is correct
        title = "",
        target = "_blank"
      ),
      class = "dropdown"
    ) 
  ),

  
# SIDEBAR -----------------------------------------------------------------
  
  
  
  
  dashboardSidebar(
    width = 300,
    introBox(data.step = 2, data.intro = intro$text[2], 
    div(class = "inlay", style = "height:15px;width:100%;background-color: #ecf0f5;"),
    sidebarMenu(
      introBox(data.step = 4, data.intro = intro$text[4], # intro tour
      div(id = "sidebar_button",
          bsButton(inputId = "confirm", 
                   label = "START EXPLORE", 
                   icon = icon("play-circle"), 
                   style = "danger")
      )
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
        radioButtons(
          inputId = "voicechoice",
          label = "VOICECHOICE",
          choices = voice_group$Type,
          selected = "Anonymous" 
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
        dateRangeInput("date2", "START DATE TO END DATE ((USE DATE RANGE FOR STATE TREND PLOT):",   
                       start = "2024-08-13",
                       end   = Sys.Date() )
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
        introBox(
        bsButton("map", 
                 label = "STATE LEVEL", 
                 style = "success"),
        bsButton("trend", 
                 label = "NATIONAL LEVEL", 
                 style = "success"),
        bsButton("expert", 
                 label = "EXPERT COMPARISON", 
                 style = "success"),
        bsButton("about", 
                 label = "ABOUT", 
                 style = "success"),
        data.step = 1, data.intro = intro$text[1])
      )
    ),
    
    
    
    fluidRow(
      div(
        id = "map_panel", 
        column(
          width = 12,
          introBox(data.step = 3, data.intro = intro$text[3],
          uiOutput("box_pat6")
          )
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
            style = "padding-left: 50px",
            width = 6,
            uiOutput("box_pat7")
        ),
        column(
          style = "padding-right: 50px;",
          width = 6,
          uiOutput("box_pat8")
        ),
        column(
          width = 6,
          style = "padding-left: 50px;",
          uiOutput("box_table1")
        ),
        column(
          width = 6,
          style = "padding-right: 50px;",
          uiOutput("box_table2")
        )
      ),
      
      
      fluidRow(
        div(
          id = "expert_panel", 
          column(
            width = 12,
            style = "padding-left: 50px; padding-right: 50px;",
            uiOutput("box_pat9")
          ),
          column(
            width = 12,
            style = "padding-left: 50px; padding-right: 50px;",
            uiOutput("box_table3")
          )
        ),
        
        
      fluidRow(
        div(
          id="about_panel",
          column(
            width=6,
            style = "padding-left: 60px;", 
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
                    p("Jared Black is a PhD candidate in the Health Service Research program at Baylor University. His research focuses on the healthcare administration and drug policy."),
                    p("Coco Mingrun Sun is a PhD candidate in the Health Service Research program at Baylor University. Her research interests span the intersection of technology, healthcare, and household finance.")
                 )
          ),
            column(12,
                   div(style="text-align: center; padding-left: 115px;",
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
  
  #####UI general 
  #show intro modal
  observeEvent("", {
    showModal(modalDialog(
      includeHTML("www/intro_text.html"),
      easyClose = TRUE,
      footer = tagList(
        actionButton(inputId = "intro", label = "INTRODUCTION TOUR", icon = icon("info-circle"))
      )
    ))
  })
  
  observeEvent(input$intro,{
    removeModal()
  })
  
  # show intro tour
  observeEvent(input$intro,
               introjs(session, options = list("nextLabel" = "Continue",
                                               "prevLabel" = "Previous",
                                               "doneLabel" = "Alright. Let's go"))
  )
  
  update_all <- function(x) {
    updateSelectInput(session, "tab",
                      choices = c("", "Maps", "Trends", "Experts", "About"),
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
  observeEvent(input$expert, {
    update_all("Experts")
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
    hide("expert_panel")
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
    hide("expert_panel")
    hide("map_panel")
  })
  observeEvent(input$expert, {
    show("expert_panel")
    hide("about_panel")
    hide("trend_panel")
    hide("map_panel")
  })
  observeEvent(input$about, {
    show("about_panel")
    hide("trend_panel")
    hide("expert_panel")
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
    updateButton(session, "expert", style = {
      if (x == "Experts") {
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
    filter(extended_data2, Type=="Anonymous") %>%
      filter(Date %in% input$date)%>%
      #filter(StateFull %in% input$USMap_state)%>%
      mutate(party_numeric= ifelse(Predicted_party == "Republican", 1, 0))%>%
      mutate(new_variable = runif(n(), min = 0, max = 50))%>%
      mutate(hover = paste(state, '<br>', 
                           "State:", StateFull, "<br>",
                           "Date:", Date, "<br>",
                           "Type:", Type, "<br>",
                           "Democrat Win Chance",Percent_byState_chr_Demo,"<br>",
                           "Winner:", Predicted_party))
  })
  
  
  
  
  # UI - Map - 2 ----------------------------------------------------------
  
  # Reactive function for USA map data
  USA_map_BBC <- reactive({
    req(input$date)
    #req(input$USMap_state)
    filter(extended_data2, Type=="BBC") %>%
      filter(Date %in% input$date)%>%
      #filter(StateFull %in% input$USMap_state)%>%
      mutate(party_numeric= ifelse(Predicted_party == "Republican", 1, 0))%>%
      mutate(new_variable = runif(n(), min = 0, max = 50))%>%
      mutate(hover = paste(state, '<br>', 
                           "State:", StateFull, "<br>",
                           "Date:", Date, "<br>",
                           "Type:", Type, "<br>",
                           "Democrat Win Chance",Percent_byState_chr_Demo,"<br>",
                           "Winner:", Predicted_party))
  })
  
  # UI - Map - 3 ----------------------------------------------------------
  
  # Reactive function for USA map data
  USA_map_Fox <- reactive({
    req(input$date)
    #req(input$USMap_state)
    filter(extended_data2, Type=="Fox") %>%
      filter(Date %in% input$date)%>%
      #filter(StateFull %in% input$USMap_state)%>%
      mutate(party_numeric= ifelse(Predicted_party == "Republican", 1, 0))%>%
      mutate(new_variable = runif(n(), min = 0, max = 50))%>%
      mutate(hover = paste(state, '<br>', 
                           "State:", StateFull, "<br>",
                           "Date:", Date, "<br>",
                           "Type:", Type, "<br>",
                           "Democrat Win Chance",Percent_byState_chr_Demo,"<br>",
                           "Winner:", Predicted_party))
  })
  
  # UI - Map - 4 ----------------------------------------------------------
  
  # Reactive function for USA map data
  USA_map_MSNBC <- reactive({
    req(input$date)
    #req(input$USMap_state)
    filter(extended_data2, Type=="MSNBC") %>%
      filter(Date %in% input$date)%>%
      #filter(StateFull %in% input$USMap_state)%>%
      mutate(party_numeric= ifelse(Predicted_party == "Republican", 1, 0))%>%
      mutate(new_variable = runif(n(), min = 0, max = 50))%>%
      mutate(hover = paste(state, '<br>', 
                           "State:", StateFull, "<br>",
                           "Date:", Date, "<br>",
                           "Type:", Type, "<br>",
                           "Democrat Win Chance",Percent_byState_chr_Demo,"<br>",
                           "Winner:", Predicted_party))
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
    filter(extended_data2, Type %in% input$voicechoice) %>%
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
 
  
  # UI Table 3 with expert
  Table3<-reactive({
    #filter(average_votes,Type %in%  input$box_table1)
    expert_all_rename<-expert_all%>%
      rename(
        Anonymous=Votes_Percent_Anonymous,
        BBC=Votes_Percent_BBC,
        Fox=Votes_Percent_Fox,
        MSNBC=Votes_Percent_MSNBC,
        "Nate Sliver"=Silver,
        "New York Times"=Times
      ) %>%
      mutate(Anonymous=round(Anonymous,digits=2)) %>%
      mutate(BBC=round(BBC,digits=2)) %>%
      mutate(Fox=round(Fox,digits=2)) %>%
      mutate(MSNBC=round(MSNBC,digits=2)) 
    
    datatable(
      expert_all_rename, 
      rownames = FALSE, 
      extensions = "Buttons",
      options = list(
        dom = 'Bfrtp',
        buttons = c('csv', 'excel', 'pdf'),
        style = "bootstrap",
        lengthMenu = c(seq(10, 150, 10))
      )
    )
    
  })
  
  
  
  # UI Expert Date
  Expert_Data <- reactive({
    #req(input$TimeseriesVoices) 
    req(input$date2)
    filter(expert_all,Date >= input$date2[1] & Date <= input$date2[2])
  })
  
#-------------------------------------------------------------------------------
  # Render UI -Map 1
  
  output$box_pat <- renderUI({
    div(
      style = "position: relative; backgroundColor: #ecf0f5",
      tabBox(
        id = "box_pat",
        width = NULL,
        height = 320,
        tabPanel(
          title = paste("United States Map Anonymous", input$date)
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
          title = paste("United States Map BBC", input$date)
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
          title = paste("United States Map Fox", input$date)
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
          title = paste("United States Map MSNBC", input$date)
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
  
  
  # UI Time Trend - 2 ------Discard------------------------------------------------------------
  output$box_pat6 <- renderUI({
    div(
      style = "position: relative; backgroundColor: #ecf0f5",
      tabBox(
        id = "box_pat",
        width = NULL,
        height = 320,
        tabPanel(
          title = "State Average Democrat Victory "
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
  
  # UI Time Trend - 3 ------------------------------------------------------------------
  output$box_pat7 <- renderUI({
    div(
      style = "position: relative; backgroundColor: #ecf0f5",
      tabBox(
        id = "box_pat",
        width = NULL,
        height = 320,
        tabPanel(
          title = "Average Electoral Votes"
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
  
  # UI Time Trend - 4 ------------------------------------------------------------------
  output$box_pat8 <- renderUI({
    div(
      style = "position: relative; backgroundColor: #ecf0f5",
      tabBox(
        id = "box_pat",
        width = NULL,
        height = 320, 
        tabPanel(
          title = " Average Democrat Victory "
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
          title = "Average Votes"
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
          title = " Win Likelihood"
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
  
  # UI Table 3 with expert
  output$box_table3 <- renderUI({
    div(
      style = "position: relative",
      tabBox(
        id = "box_table3",
        width = NULL,
        height = 550,
        tabPanel(
          title = " Win Likelihood By Voice and Expert"
        ),
        withSpinner(
          DT::dataTableOutput("table3", height = 500),
          type = 4,
          color = "#d33724",
          size = 0.7
        )
      )
    )
    
  })
  

  
  # UI 9 -----Expert
  output$box_pat9 <- renderUI({
    div(
      style = "position: relative; backgroundColor: #ecf0f5",
      tabBox(
        id = "box_table",
        width = NULL,
        height = 320, 
        tabPanel(
          title = " Average Democrat Victory With Expert Opinions "
        ),
        withSpinner(
          plotlyOutput("distPlot3", height = 230),
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
      z = ~Percent_byState_Demo,  
      text = ~hover,       # Hover text with details
      locations = ~state,  # State abbreviations
      color = ~Percent_byState_Demo,
      colorscale = custom_colorscale,
      showscale = TRUE,
      hoverinfo = 'text' 
    )
    
    
    fig <- fig %>% colorbar(
      title = "Probability",
      thickness = 15,       # Adjust thickness
      len = 0.8,            # Adjust length
      tickvals = c(0, 0.25, 0.5, 0.75, 1),
      cmin = 0,  # Ensure the minimum value is fixed to 0
      cmax = 1,  # Ensure the maximum value is fixed to 1
      ticks = "outside", # Set specific tick values
      ticktext = c("0", "25%", "50%", "75%", "100%")  # Labels for ticks
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
      z = ~Percent_byState_Demo,  
      text = ~hover,       # Hover text with details
      locations = ~state,  # State abbreviations
      color = ~Percent_byState_Demo,
      colorscale = custom_colorscale,
      showscale = TRUE,
      hoverinfo = 'text' 
    )
    
    
      fig <- fig %>% colorbar(
      title = "Probability",
      thickness = 15,       # Adjust thickness
      len = 0.8,            # Adjust length
      tickvals = c(0, 0.25, 0.5, 0.75, 1), 
      ticks = "outside", # Set specific tick values
      cmin = 0,  # Ensure the minimum value is fixed to 0
      cmax = 1,  # Ensure the maximum value is fixed to 1
      ticktext = c("0", "25%", "50%", "75%", "100%")  # Labels for ticks
    )
    
    
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
      z = ~Percent_byState_Demo,  
      text = ~hover,       # Hover text with details
      locations = ~state,  # State abbreviations
      color = ~Percent_byState_Demo,
      colorscale = custom_colorscale,
      showscale = TRUE,
      hoverinfo = 'text' 
    )
    
    
    fig <- fig %>% colorbar(
      title = "Probability",
      thickness = 15,       # Adjust thickness
      len = 0.8,            # Adjust length
      tickvals = c(0, 0.25, 0.5, 0.75, 1),
      ticks = "outside", # Set specific tick values
      cmin = 0,  # Ensure the minimum value is fixed to 0
      cmax = 1,  # Ensure the maximum value is fixed to 1
      ticktext = c("0", "25%", "50%", "75%", "100%")  # Labels for ticks
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
      z = ~Percent_byState_Demo,  
      text = ~hover,       # Hover text with details
      locations = ~state,  # State abbreviations
      color = ~Percent_byState_Demo,
      colorscale = custom_colorscale,
      showscale = TRUE,
      hoverinfo = 'text' 
    )
    
    
    fig <- fig %>% colorbar(
      title = "Probability",
      thickness = 15,       # Adjust thickness
      len = 0.8,            # Adjust length
      tickvals = c(0, 0.25, 0.5, 0.75, 1),  
      ticks = "outside", # Set specific tick values
      cmin = 0,  # Ensure the minimum value is fixed to 0
      cmax = 1,  # Ensure the maximum value is fixed to 1
      ticktext = c("0", "25%", "50%", "75%", "100%")  # Labels for ticks
    )
    
    
    fig <- fig %>% layout(
      geo = g
    )
    
    fig
  })  
  
  
  # UI #5 Overall need to be revised with add DNC ---Discard
  output$plot_Overall<- renderPlotly({
    input$date2
    #input$confirm
    
    fig <- plot_ly(Count_data_4Voice(), x = ~Count_data_4Voice()$Date, y = ~avg_Direct, name = 'Anonymous', type = 'scatter', mode = 'lines',
                   line = list(color = 'rgb(205, 12, 24)', width = 4)) 
    fig <- fig %>% add_trace(y = ~avg_BBC, name = 'BBC', line = list(color = 'rgb(22, 96, 167)', width = 4)) 
    fig <- fig %>% add_trace(y = ~avg_Fox, name = 'Fox', line = list(color = 'rgb(205, 12, 24)', width = 4, dash = 'dash')) 
    fig <- fig %>% add_trace(y = ~avg_MSNBC, name = 'MSNBC', line = list(color = 'rgb(22, 96, 167)', width =4 , dash = 'dot')) %>%
      layout(
        title = NULL,
        xaxis = list(title = "Date",
                     showgrid = FALSE),
        yaxis = list(title = "Percent", 
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
      if (length(Count_data()$Predicted_party) ==0) {
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
                title = "Percent",
                showline = TRUE,
                showgrid = FALSE,
                showticklabels = TRUE,
                tickformat = ".0%",  # Format ticks as percentage
                range = c(-0.1, 1.1),
                tickmode = "linear",
                tick0 = 0,
                dtick = 0.2
              ),
              shapes = list(
              list(
                type = "rect",
                fillcolor = "rgba(205, 12, 24, 0.2)", # Light red fill for 140-270
                line = list(color = "rgba(205, 12, 24, 0)"), # No border
                x0 = min(Count_data()$Date), x1 = max(Count_data()$Date),
                y0 =0, y1 = 0.5
              ),
              list(
                type = "rect",
                fillcolor = "rgba(22, 96, 167, 0.2)", # Light blue fill for 270-400
                line = list(color = "rgba(22, 96, 167, 0)"), # No border
                x0 = min(Count_data()$Date), x1 = max(Count_data()$Date),
                y0 = 0.5, y1 = 1
              ),
              list(
                type = "line",
                x0 = min(Count_data()$Date), x1 = max(Count_data()$Date),
                y0 = 0.5, y1 = 0.5,
                line = list(color = "rgb(0, 0, 0)", dash = 'dash', width = 2)
              )
            )
    ) %>%
      layout(annotations = list(
        list(
          x = min(Count_data()$Date) + 5,
          y = 0.65,
          text = "Democrat Win",
          showarrow = FALSE,
          font = list(size = 12, weight = "bold", color = "rgb(22, 96, 167)"),
          showgrid = FALSE
        ),
        list(
          x = min(Count_data()$Date) + 5,
          y = 0.4,
          text = "Republican Win",
          showarrow = FALSE,
          font = list(size = 12, weight = "bold", color =  "rgb(205, 12, 24)")
        )
      ))
        }
        fig  # Return the plotly figure
      }
    })
  })
  
  output$distPlot <- renderPlotly({
    input$date2
    #input$confirm
    
  fig <- plot_ly(Votes_final(), x = ~Date, y = ~Votes_Anonymous, name = 'Anonymous', type = 'scatter', mode = 'lines',
                 line = list(color = 'rgb(205, 12, 24)', width = 4)) 
  fig <- fig %>% add_trace(y = ~Votes_BBC, name = 'BBC', line = list(color = 'rgb(22, 96, 167)', width = 4)) 
  fig <- fig %>% add_trace(y = ~Votes_Fox, name = 'Fox', line = list(color = 'rgb(205, 12, 24)', width = 4, dash = 'dash')) 
  fig <- fig %>% add_trace(y = ~Votes_MSNBC, name = 'MSNBC', line = list(color = 'rgb(22, 96, 167)', width = 4, dash = 'dot')) %>%
    layout(
      title = NULL,
      xaxis = list(title = "Date",
                   showgrid = TRUE),
      yaxis = list(title = "Votes", 
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
      )
    ))
})
  
  output$distPlot2 <- renderPlotly({
    input$date2
    #input$confirm
    
    fig <- plot_ly(Votes_final(), x = ~Date, y = ~Votes_Percent_Anonymous, name = 'Anonymous', type = 'scatter', mode = 'lines',
                   line = list(color = 'rgb(205, 12, 24)', width = 4)) 
    fig <- fig %>% add_trace(y = ~Votes_Percent_BBC, name = 'BBC', line = list(color = 'rgb(22, 96, 167)', width = 4)) 
    fig <- fig %>% add_trace(y = ~Votes_Percent_Fox, name = 'Fox', line = list(color = 'rgb(205, 12, 24)', width = 4, dash = 'dash')) 
    fig <- fig %>% add_trace(y = ~Votes_Percent_MSNBC, name = 'MSNBC', line = list(color = 'rgb(22, 96, 167)', width = 4, dash = 'dot')) %>%
      layout(
        title = NULL,
        xaxis = list(title = "Date",
                     showgrid = TRUE),
        yaxis = list(title = "Percent", 
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
  
  
  # Table 3
  output$table3<- DT::renderDataTable({
    
    Table3()
  }, server = FALSE)
  
  
  
  # Image in About----------------------------------------------------------
  
  output$home_img <- renderImage({
    list(src = "www/Jared_Image2.png", contentType = 'image/png')
  }, deleteFile = FALSE)

  
  # Expert graph
  output$distPlot3 <- renderPlotly({
    input$date2
    #input$confirm
    
    fig <- plot_ly(Expert_Data(), x = ~Date, y = ~Votes_Percent_Anonymous, name = 'Anonymous', type = 'scatter', mode = 'lines',
                   line = list(color = 'rgb(205, 12, 24)', width = 4)) 
    fig <- fig %>% add_trace(y = ~Votes_Percent_BBC, name = 'BBC', line = list(color = 'rgb(22, 96, 167)', width = 4)) 
    fig <- fig %>% add_trace(y = ~Votes_Percent_Fox, name = 'Fox', line = list(color = 'rgb(205, 12, 24)', width = 4, dash = 'dash')) 
    fig <- fig %>% add_trace(y = ~Votes_Percent_MSNBC, name = 'MSNBC', line = list(color = 'rgb(22, 96, 167)', width = 4, dash = 'dot')) 
    fig <- fig %>% add_trace(y = ~Silver, name = 'Nate Silver', line = list(color = 'orange', width = 4, dash = 'lines')) 
    fig <- fig %>% add_trace(y = ~Times, name = 'New York Times', line = list(color = 'lightgreen', width = 4, dash = 'lines')) %>%
      layout(
        title = NULL,
        xaxis = list(title = "Date",
                     showgrid = TRUE),
        yaxis = list(title = "Percent", 
                     range = c(0.3, 0.7),
                     showgrid = FALSE),
        shapes = list(
          list(
            type = "rect",
            fillcolor = "rgba(205, 12, 24, 0.2)", # Light red fill for 140-270
            line = list(color = "rgba(205, 12, 24, 0)"), # No border
            x0 = min(Expert_Data()$Date), x1 = max(Expert_Data()$Date),
            y0 =0.3, y1 = 0.5
          ),
          list(
            type = "rect",
            fillcolor = "rgba(22, 96, 167, 0.2)", # Light blue fill for 270-400
            line = list(color = "rgba(22, 96, 167, 0)"), # No border
            x0 = min(Expert_Data()$Date), x1 = max(Expert_Data()$Date),
            y0 = 0.5, y1 = 0.7
          ),
          list(
            type = "line",
            x0 = min(Expert_Data()$Date), x1 = max(Expert_Data()$Date),
            y0 = 0.5, y1 = 0.5,
            line = list(color = "rgb(0, 0, 0)", dash = 'dash', width = 2)
          )
        )
      ) %>%
      layout(annotations = list(
        list(
          x = min(Expert_Data()$Date) + 5,
          y = 0.65,
          text = "Democrat Win",
          showarrow = FALSE,
          font = list(size = 12, weight = "bold", color = "rgb(22, 96, 167)"),
          showgrid = FALSE
        ),
        list(
          x = min(Expert_Data()$Date) + 5,
          y = 0.4,
          text = "Republican Win",
          showarrow = FALSE,
          font = list(size = 12, weight = "bold", color =  "rgb(205, 12, 24)")
        )
      ))
  })
  
}





shinyApp(ui, server)
#deployApp(appName = "ElectionGPT2", forceUpdate = TRUE)
