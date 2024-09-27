library(usmap) 
library(shiny)
library(ggplot2)
library(maps)
library(openxlsx)
library(readxl)
library(dplyr)
library(tidyr)

library(tidytuesdayR) 
library(tidyverse) 
library(janitor) 
library(ggeasy) 
library(gganimate)
library(transformr) 
library(patchwork) 
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


library(httr)


library(rsconnect)
#rsconnect::deployApp('/Users/sunmingrun/Documents/GitHub/ElectionGPT/ShinyApp')

# 1 data import
data<-read_csv("panel_election_results_state.csv",show_col_types = FALSE)


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

#---------
#data2<-read_delim("panel_control_election_results_state.csv",show_col_types = FALSE)

data2<-read_csv("panel_control_election_results_state.csv",show_col_types = FALSE)


melted_data2<-data2%>%
  rename(
    value = Result,  # Renaming 'Result' to 'value'
    state = State,
    Type= Voice# Renaming 'State' to 'state'
  )%>%
  mutate(Type = ifelse(Type == "direct", "Direct", Type)) %>%
  mutate(Date = as.Date(Date, format = "%m/%d/%y")) %>%
  mutate(Type = ifelse(Type == "Direct", "Anonymous", Type))

#````````````````

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

melted_data2<-melted_data2%>%
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

#------for data2 no news  and news
subdata2_news <- subdata2 %>%
  select(-TotalTrial_byState,-No_Democratic_State,-Percent_byState,-No_Republican_State,-Percent_byState_chr)%>%
  mutate(Predicted_party=ifelse(Percent_byState_Demo>=0.5, "Democratic", "Republican"))



subdata2_nonews <- melted_data2 %>%
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
  mutate(Percent_byState_chr_Demo=sprintf("%1.2f%%", 100*Percent_byState_Demo))%>%
  select(-TotalTrial_byState,-No_Democratic_State,-Percent_byState,-No_Republican_State,-Percent_byState_chr)%>%
  rename(
    Percent_byState_Demo2=Percent_byState_Demo,
    Percent_byState_chr_Demo2=Percent_byState_chr_Demo)%>%
  mutate(Predicted_party2=ifelse(Percent_byState_Demo2>=0.5, "Democratic", "Republican"))


#this is the combined news and no news data for switch purpose
news_nonews<-subdata2_news%>%
  left_join(subdata2_nonews, by=c("Date","Type","state","StateFull"))
  
#-----------------Combine news and no news


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

# Ensure the column order is the same as in extended_data2


# Combine the original data with the dc_data and ensure it is sorted by Date, Type, and state



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

subdata3_1_for_sentiment<-melted_data %>%
  group_by(Type, Trial, Date,party) %>%
  summarise(
    Number_Repub_Win = n(),
    votes_party =sum(Electoral_Votes),
    .groups = 'drop'
  ) %>% filter(party=="Democratic") %>%
  mutate(percent_by_trail=votes_party/538)

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

#***no news data

subdata3_1_nonews<-melted_data2 %>%
  mutate(party = ifelse(value ==1, "Republican", "Democratic"))%>%
  group_by(Type, Trial, Date,party) %>%
  summarise(
    Number_Repub_Win = n(),
    votes_party =sum(Electoral_Votes),
    .groups = 'drop'
  ) 

subdata3_1_for_sentiment_nonews<-melted_data2 %>%
  mutate(party = ifelse(value ==1, "Republican", "Democratic"))%>%
  group_by(Type, Trial, Date,party) %>%
  summarise(
    Number_Repub_Win = n(),
    votes_party =sum(Electoral_Votes),
    .groups = 'drop'
  ) %>% filter(party=="Democratic") %>%
  mutate(percent_by_trail=votes_party/538)

subdata3_1_2_nonews<-melted_data2 %>%
  mutate(party = ifelse(value ==1, "Republican", "Democratic"))%>%
  group_by(Type, Date, party) %>%
  summarise(
    Number_Repub_Win = n(),
    votes_party =sum(Electoral_Votes),
    .groups = 'drop'
  ) %>%
  arrange(Date)%>% drop_na

#calculate total number of votes
subdata3_2_2_nonews<-melted_data2 %>%
  mutate(party = ifelse(value ==1, "Republican", "Democratic"))%>%
  group_by(Type, Date) %>%
  summarise(
    Votes =sum(Electoral_Votes),
    .groups = 'drop'
  ) %>% drop_na

subdata3_2_reshape_nonews<-subdata3_1_2_nonews%>%
  left_join(subdata3_2_2_nonews,by=c("Type","Date")) %>%
  mutate(Votes_perent=votes_party/Votes) %>%
  select(Date, Type,party, Votes_perent) %>%  # Select relevant columns
  pivot_wider(names_from = Type, values_from = Votes_perent, names_prefix = "Votes_")%>% 
  filter(party=="Democratic")%>%
  select("Date","party","Votes_Anonymous","Votes_BBC","Votes_Fox","Votes_MSNBC" )%>%
  arrange(Date) 
.groups = 'drop'


# calculate total trials for each type each date for both parties, should be 100 per day for each type but we lose trials
trial_counts_nonews<-subdata3_1_nonews %>%
  group_by(Type,Date) %>%
  mutate(trial_counts= n_distinct(Trial))%>%
  select(Type,Date,party,trial_counts)%>%
  distinct() # drop duplicates 
.groups = 'drop'

trial_votes_nonews<-melted_data2 %>%
  mutate(party = ifelse(value ==1, "Republican", "Democratic"))%>%
  group_by(Type, Date,party) %>%
  summarise(
    votes_party =sum(Electoral_Votes),
    .groups = 'drop'
  ) %>%
  left_join(trial_counts_nonews, by=c("party","Type","Date" ))%>%
  mutate(average_votes=votes_party/trial_counts)


#total should be 535 for now and 538 later
total_votes_count_nonews <- trial_votes_nonews %>%
  select(Date, Type, average_votes) %>%  
  group_by(Type, Date)%>%
  summarise(
    votes_total=sum(average_votes)
  )

# merge two datasets before reshape missing 09-24
trial_votes_nonews <-trial_votes_nonews%>%
  left_join(total_votes_count_nonews, by=c("Type","Date" )) %>%
  mutate(average_votes = round(average_votes, digits = 0)) %>%  # Round average_votes
  mutate(average_votes_percent=average_votes/votes_total)

#*************** Dataset no news with predicted votes 

#For graph
average_votes_reshape2 <- trial_votes_nonews %>%
  select(Date, Type, party, average_votes) %>%  # Select relevant columns
  pivot_wider(names_from = Type, values_from = average_votes, names_prefix = "NoNews_Votes_")%>%
  select("Date","party","NoNews_Votes_Anonymous", "NoNews_Votes_BBC" ,"NoNews_Votes_Fox", "NoNews_Votes_MSNBC")%>%
  drop_na

# For graph
average_votes_percent_reshape2<- trial_votes_nonews %>%
  select(Date, Type, party, average_votes_percent) %>%  # Select relevant columns
  pivot_wider(names_from = Type, values_from = average_votes_percent, names_prefix = "NoNews_Votes_Percent_")

# Final Dataset for national level winner for average votes and average votes percent
#*******************************************************************
trial_votes_reshape2 <-average_votes_reshape2%>%
  left_join(average_votes_percent_reshape2, by=c("Date","party"))%>% 
  filter(party=="Democratic")%>%
  arrange(Date)
#*******************************************************************

#****combined

combined_nonews_news<-trial_votes_reshape %>%
  select(-"Votes_Percent_Anonymous", -"Votes_Percent_BBC", -"Votes_Percent_Fox", -"Votes_Percent_MSNBC") %>%
  left_join(trial_votes_reshape2,by=c("Date","party" ))%>%
  select("Date","party", "Votes_Anonymous", "Votes_BBC", "Votes_Fox", "Votes_MSNBC","NoNews_Votes_Anonymous", "NoNews_Votes_BBC", "NoNews_Votes_Fox", "NoNews_Votes_MSNBC") 







#-------------import expert data
# ----------------------------
expert <-read_csv("expert_combined_panel.csv",show_col_types = FALSE)

expert <- expert %>%
  distinct()  

expert_data<-expert%>%
  rename(
    Date = date,  # Renaming 'Result' to 'value'
  )%>%
  mutate(Date = as.Date(Date, format = "%m/%d/%y")) %>%
  mutate(Harris = round(Harris/100,digit=2)) %>%
  mutate(Trump = round(Trump/100,digit=2))%>%
  distinct()  
.groups = 'drop'

expert_nate <-expert_data%>%
  filter(source=="silver")


# simulation
# Comment: subdata2 creates the probability of aggregated state winner by Date and Type 
# Creating the second subset with state-specific totals and proportions
# ******************Percent_byState variable used for state-level time series 

# Creating the second subset with state-specific totals,showing total trial for specific day with value 1 or 2

# Monte_Carlo Simulation with news
monte_carlo<-melted_data %>%
  group_by(Type, Trial, Date,party) %>%
  summarise(
    Number_Repub_Win = n(),
    TotalTrial_byVoice = n(),
    votes_party =sum(Electoral_Votes),
    .groups = 'drop'
  ) %>%
  mutate(outcome = ifelse(votes_party>=270, "Winner", "Loser"))%>%
  mutate(Result = ifelse(votes_party>=270, 1, 0))

monte_carlo_overall<-monte_carlo%>%
  group_by(Type, Date,party) %>%
  summarise(
    Total_Trials=n(),
    No_Win_Trial = sum(Result == 1, na.rm = TRUE),
    .groups = 'drop'
  ) %>%
  mutate(Simulation=No_Win_Trial/Total_Trials)%>%
  mutate(Simulation_chr=sprintf("%1.2f%%", 100*Simulation))

monte_carlo_reshape<- monte_carlo_overall %>%
  select(Date, Type, party, Simulation) %>% 
  mutate(Simulation=round(Simulation,digit=2))%>%
  pivot_wider(names_from = Type, values_from = Simulation, names_prefix = "Trial_Percent_")

# -----------
# Monte_Carlo Simulation without news
monte_carlo2<-melted_data2 %>%
  mutate(party = ifelse(value ==1, "Republican", "Democratic")) %>%
  group_by(Type, Trial, Date,party) %>%
  summarise(
    Number_Repub_Win = n(),
    TotalTrial_byVoice = n(),
    votes_party =sum(Electoral_Votes),
    .groups = 'drop'
  ) %>%
  mutate(outcome = ifelse(votes_party>=270, "Winner", "Loser"))%>%
  mutate(Result = ifelse(votes_party>=270, 1, 0))

monte_carlo_overall2<-monte_carlo2%>%
  group_by(Type, Date,party) %>%
  summarise(
    Total_Trials=n(),
    No_Win_Trial = sum(Result == 1, na.rm = TRUE),
    .groups = 'drop'
  ) %>%
  mutate(Simulation=No_Win_Trial/Total_Trials)%>%
  mutate(Simulation_chr=sprintf("%1.2f%%", 100*Simulation))

monte_carlo_reshape2<- monte_carlo_overall2 %>%
  select(Date, Type, party, Simulation) %>% 
  mutate(Simulation=round(Simulation,digit=2))%>%
  pivot_wider(names_from = Type, values_from = Simulation, names_prefix = "Trial_Percent_")


#------------Data  process done

#write_csv(extended_data,"/Users/sunmingrun/Desktop/AI Project/panel_election_results_help.csv")


#------color
pal <- pnw_palette(name = "Bay", n = 16, type = "continuous")
color_for_1 <- pal[16]  # Close to red
color_for_0 <- pal[1]  # Close to blue


pal2<-pnw_palette(name = "Bay", n = 16, type = "continuous")
color_for_low1 <- pal2[13]


pal3<- pnw_palette("Shuksan2",5)
color_for_low0 <- pal3[2]


pal5 <- pnw_palette("Winter",100)
color_for_05 <- pal5[97]




custom_colorscale <- list(
  list(0, "#DD4124"),       
  list(0.25, color_for_low1),    
  list(0.5, color_for_05),     
  list(0.75, color_for_low0),   
  list(1, "#00496F")        
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
      introBox(data.step = 5, data.intro = intro$text[5], # intro tour
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
          label = "VOICE CHOICE",
          choices = voice_group$Type,
          selected = "Anonymous" 
        )
      ),
      br(),
      br(),
      menuItem(
        "Date",
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
                  value =  Sys.Date()),
        dateRangeInput("date2", "START DATE TO END DATE (USE DATE RANGE FOR STATE TREND PLOT):",   
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
          uiOutput("box_pat6_2")
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
            uiOutput("box_pat8_1")
        ),
        column(
          style = "padding-right: 50px;",
          width = 6,
          uiOutput("box_pat7")
        )
      ),
      
      
      fluidRow(
        div(
          id = "expert_panel", 
          column(
            width = 6,
            #style = "padding-left: 50px; padding-right: 50px;",
            style = "padding-left: 50px;",
            uiOutput("box_pat10")
          ),
          column(
            width = 6,
            #style = "padding-left: 50px; padding-right: 50px;",
            style = "padding-right: 50px;",
            uiOutput("box_pat9")
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
            h5(p("Step 1: Pull 100 news stories from Event Registry: API Search for news stories related to the prompt: “2024 US presidential election”. Each date corresponds to a 1 am CST newspaper article pulled on that date. For example, on September 10th, we pulled the data at 1 am CST, and therefore the news refers up to the 9th."),
               p("Step 2: Feed stories to Chat-GPT in 4 distinct voices:Four characters, each representing different perspectives, will generate 100 stories:"),
               h6(p("• Voice 1: Anonymous/Direct truthful reporter"),
                  p("• Voice 2: Fox Reporter Bret Baier"),
                  p("• Voice 3: MSNBC Reporter Rachel Maddow"),
                  p("• Voice 4: BBC Reporter Laura Kuenssberg"),
               ),
               p("Step 3: Generate election stories from each character’s perspective: For each character, 100 stories are written about the election outcome in each state."),
               p("Step 4: Extract the election winners from each story: Use GPT to extract only the names of the winners from the stories for each character."),
               p("Step 5: Save winners and display the percentage of daily trials that went to each party in each state: 1 = Trump/Republican and 0 = Harris/Democrat"),
               p("Step 6: Repeat the process daily, appending new results to the previous day’s data panel.")
            ),
            br(),
            h5(p("We hope you find it interesting and/or useful.  Any comments or questions are welcome at the email address"),
               p("The source for this project is available ", a("on github", href = "https://github.com/"), ".")),
            #hr(),
            
          ),
          column(6,
                 #br(),
                 # HTML('<img src="GregPicCrop.png", height="110px"
                 # style="float:right"/>','<p style="color:black"></p>'),
                 h4(p("About the Author")),
                 h5(p("Scott Cunningham is the Ben H. Williams Professor of Economics at Baylor University.  He specializes in a range of topics in applied microeconomics, such as mental illness, drug policy, and sex work."),
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
    req(input$box_map1)
    #req(input$USMap_state)
    filter(news_nonews, Type=="Anonymous") %>%
      filter(Date %in% input$date)%>%
      #filter(StateFull %in% input$USMap_state)%>%
      mutate(party_numeric= ifelse(Predicted_party == "Republican", 1, 0))%>%
      mutate(new_variable = runif(n(), min = 0, max = 50))%>%
      mutate(hover = paste(state, '<br>', 
                           "State:", StateFull, "<br>",
                           "Date:", Date, "<br>",
                           "Type:", Type, "<br>",
                           "Democrat Win Chance",Percent_byState_chr_Demo,"<br>",
                           "Winner:", Predicted_party))%>%
      mutate(party_numeric= ifelse(Predicted_party == "Republican", 1, 0))%>%
      mutate(hover2 = paste(state, '<br>', 
                           "State:", StateFull, "<br>",
                           "Date:", Date, "<br>",
                           "Type:", Type, "<br>",
                           "Democrat Win Chance",Percent_byState_chr_Demo2,"<br>",
                           "Winner:", Predicted_party2))
  })
  
  
  
  
  # UI - Map - 2 ----------------------------------------------------------
  
  # Reactive function for USA map data
  USA_map_BBC <- reactive({
    req(input$date)
    req(input$box_map2)
    #req(input$USMap_state)
    filter(news_nonews, Type=="BBC") %>%
      filter(Date %in% input$date)%>%
      #filter(StateFull %in% input$USMap_state)%>%
      mutate(party_numeric= ifelse(Predicted_party == "Republican", 1, 0))%>%
      mutate(new_variable = runif(n(), min = 0, max = 50))%>%
      mutate(hover = paste(state, '<br>', 
                           "State:", StateFull, "<br>",
                           "Date:", Date, "<br>",
                           "Type:", Type, "<br>",
                           "Democrat Win Chance",Percent_byState_chr_Demo,"<br>",
                           "Winner:", Predicted_party))%>%
      mutate(party_numeric= ifelse(Predicted_party == "Republican", 1, 0))%>%
      mutate(hover2 = paste(state, '<br>', 
                            "State:", StateFull, "<br>",
                            "Date:", Date, "<br>",
                            "Type:", Type, "<br>",
                            "Democrat Win Chance",Percent_byState_chr_Demo2,"<br>",
                            "Winner:", Predicted_party2))
  })
  
  
  # UI - Map - 3 ----------------------------------------------------------
  
  # Reactive function for USA map data
  USA_map_Fox <- reactive({
    req(input$date)
    req(input$box_map3)
    #req(input$USMap_state)
    filter(news_nonews, Type=="Fox") %>%
      filter(Date %in% input$date)%>%
      #filter(StateFull %in% input$USMap_state)%>%
      mutate(party_numeric= ifelse(Predicted_party == "Republican", 1, 0))%>%
      mutate(new_variable = runif(n(), min = 0, max = 50))%>%
      mutate(hover = paste(state, '<br>', 
                           "State:", StateFull, "<br>",
                           "Date:", Date, "<br>",
                           "Type:", Type, "<br>",
                           "Democrat Win Chance",Percent_byState_chr_Demo,"<br>",
                           "Winner:", Predicted_party))%>%
      mutate(party_numeric= ifelse(Predicted_party == "Republican", 1, 0))%>%
      mutate(hover2 = paste(state, '<br>', 
                            "State:", StateFull, "<br>",
                            "Date:", Date, "<br>",
                            "Type:", Type, "<br>",
                            "Democrat Win Chance",Percent_byState_chr_Demo2,"<br>",
                            "Winner:", Predicted_party2))
  })
  
  
  # UI - Map - 4 ----------------------------------------------------------
  
  # Reactive function for USA map data
  USA_map_MSNBC <- reactive({
    req(input$date)
    req(input$box_map4)
    #req(input$USMap_state)
    filter(news_nonews, Type=="MSNBC") %>%
      filter(Date %in% input$date)%>%
      #filter(StateFull %in% input$USMap_state)%>%
      mutate(party_numeric= ifelse(Predicted_party == "Republican", 1, 0))%>%
      mutate(new_variable = runif(n(), min = 0, max = 50))%>%
      mutate(hover = paste(state, '<br>', 
                           "State:", StateFull, "<br>",
                           "Date:", Date, "<br>",
                           "Type:", Type, "<br>",
                           "Democrat Win Chance",Percent_byState_chr_Demo,"<br>",
                           "Winner:", Predicted_party))%>%
      mutate(party_numeric= ifelse(Predicted_party == "Republican", 1, 0))%>%
      mutate(hover2 = paste(state, '<br>', 
                            "State:", StateFull, "<br>",
                            "Date:", Date, "<br>",
                            "Type:", Type, "<br>",
                            "Democrat Win Chance",Percent_byState_chr_Demo2,"<br>",
                            "Winner:", Predicted_party2))
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
  
  # UI 6- Time Series 2 news without news
  News_NoNews <- reactive({
    req(input$voicechoice) 
    req(input$statesInput)
    req(input$date2)
    req(input$box_year1)
    #req(input$partychoice)
    filter(news_nonews, Type %in% input$voicechoice) %>%
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
  
  
  # UI 9 monte
  # for news
  monte_carlo <-reactive({
    req(input$date2)
    filter(monte_carlo_reshape,Date >= input$date2[1] & Date <= input$date2[2])%>%
    filter(party=="Democratic")
  }) 
  
  # for no news
  monte_carlo2 <-reactive({
    req(input$date2)
    filter(monte_carlo_reshape2,Date >= input$date2[1] & Date <= input$date2[2])%>%
      filter(party=="Democratic")
  }) 
  
  # UI 10 both average votes and average votes percent use this function
  Votes_final <-reactive({
    req(input$date2)
    filter(trial_votes_reshape,Date >= input$date2[1] & Date <= input$date2[2])
  })
  
  Votes_final_2 <-reactive({
    req(input$date2)
    filter(trial_votes_reshape2,Date >= input$date2[1] & Date <= input$date2[2])
  })
  
  #for combined
  Votes_final_3<-reactive({
    req(input$date2)
    filter(combined_nonews_news,Date >= input$date2[1] & Date <= input$date2[2])
  })

  # UI Table 1 average votes 
  Table1 <-reactive({
    #filter(average_votes,Type %in%  input$box_table1)
    monte_carlo_select<-monte_carlo_overall%>%
      select(-Simulation) %>%
      rename(
        Party="party",
        Voice="Type",
        "Total Trial"=Total_Trials,
        "Average Win"=Simulation_chr,
        "Total Winning Trial"=No_Win_Trial
      )
    
    datatable(
      monte_carlo_select, 
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
    average_votes_percent<-average_votes_percent%>%
      rename(
        "Aaverage EC Votes Out of 538"=WinLikelihood
      )
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
        "Nate Silver"=Silver,
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
    filter(expert_data,Date >= input$date2[1] & Date <= input$date2[2])
  })
  
  expert_Economist <- reactive({
    req(input$date2)
    req(input$box_expert)
    filter(expert_data, source=="economist") %>%
      filter(Date >= input$date2[1] & Date <= input$date2[2])
  })
  
  expert_silver <- reactive({
    req(input$date2)
    req(input$box_expert)
    filter(expert_data, source=="silver") %>%
      filter(Date >= input$date2[1] & Date <= input$date2[2]) %>%
      arrange("Date")
  })
  
  expert_times <- reactive({
    req(input$date2)
    req(input$box_expert)
    filter(expert_data, source=="times-siena") %>%
      filter(Date >= input$date2[1] & Date <= input$date2[2])
  })
  
  observe({
    print(expert_silver(),n=60)
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
          title = paste("United States Map Anonymous", input$date),
          div(
            style = "position: absolute; left:0.5em; bottom: 0.5em;",
            dropdown(
              radioGroupButtons(
                inputId = "box_map1",
                label = "Select time period", 
                choiceNames = c("News", "No News"),
                choiceValues = c("With News", "Without News"), 
                selected = "With News",  
                direction = "vertical"
              ),
              size = "xs",
              icon = icon("gear", class = "opt"), 
              up = TRUE
            )
          ),
          withSpinner(
            plotlyOutput("box_map_anonymous", height = 230),
            type = 4,
            color = "#d33724", 
            size = 0.7 
          )
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
          title = paste("United States Map BBC", input$date),
          div(
            style = "position: absolute; left:0.5em; bottom: 0.5em;",
            dropdown(
              radioGroupButtons(
                inputId = "box_map2",
                label = "Select time period", 
                choiceNames = c("News", "No News"),
                choiceValues = c("With News", "Without News"), 
                selected = "With News", 
                direction = "vertical"
              ),
              size = "xs",
              icon = icon("gear", class = "opt"), 
              up = TRUE
            )
          ),
          withSpinner(
            plotlyOutput("box_map_BBC", height = 230),
            type = 4,
            color = "#d33724", 
            size = 0.7 
          )
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
          title = paste("United States Map Fox", input$date),
          div(
            style = "position: absolute; left:0.5em; bottom: 0.5em;",
            dropdown(
              radioGroupButtons(
                inputId = "box_map3",
                label = "Select time period", 
                choiceNames = c("News", "No News"),
                choiceValues = c("With News", "Without News"), 
                selected = "With News",  
                direction = "vertical"
              ),
              size = "xs",
              icon = icon("gear", class = "opt"), 
              up = TRUE
            )
          ),
          withSpinner(
            plotlyOutput("box_map_Fox", height = 230),
            type = 4,
            color = "#d33724", 
            size = 0.7 
          )
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
          title = paste("United States Map MSNBC", input$date),
          div(
            style = "position: absolute; left:0.5em; bottom: 0.5em;",
            dropdown(
              radioGroupButtons(
                inputId = "box_map4",
                label = "Select time period", 
                choiceNames = c("News", "No News"),
                choiceValues = c("With News", "Without News"),  
                selected = "With News", 
                direction = "vertical"
              ),
              size = "xs",
              icon = icon("gear", class = "opt"), 
              up = TRUE
            )
        ),
        withSpinner(
          plotlyOutput("box_map_MSNBC", height = 230),
          type = 4,
          color = "#d33724", 
          size = 0.7 
        )
      )
    )
    )
  })
  
  

  # UI - State Trend current working -----------------------------------------------------
  output$box_pat6_2 <- renderUI({
    div(
      style = "position: relative; backgroundColor: #ecf0f5",
      tabBox(
        id = "box_pat",
        width = NULL,
        height = 320,
        tabPanel(
          title = "State Average Democrat Victory",
          div(
            style = "position: absolute; left:0.5em; bottom: 0.5em;",
            introBox(data.step = 4, data.intro = intro$text[4], 
            dropdown(
              radioGroupButtons(
                inputId = "box_year1",
                label = "Select time period", 
                choiceNames = c("News", "No News"),
                choiceValues = c("With News", "Without News"), 
                selected = "With News", 
                direction = "vertical"
              ),
              size = "xs",
              icon = icon("gear", class = "opt"), 
              up = TRUE
            )
          )
          ),
          withSpinner(
            plotlyOutput("plot_state2", height = 230),
            type = 4,
            color = "#d33724", 
            size = 0.7 
          )
        )
      )
    )
  })
  

  output$plot_state2 <- renderPlotly({
    input$allInput
    input$voicechoice
    input$statesInput
    input$date2
    input$box_year1
    #input$confirm
    #input$partychoice
    isolate({
      if (input$box_year1 == "no_news") {
        fig <- plot_ly()
        # Loop through each selected state and add a trace for it
        for (k in seq_along(input$statesInput)){
          state_data <- News_NoNews()[News_NoNews()$StateFull == input$statesInput[k], ]
          fig <- add_trace(fig, data = state_data, 
                           x = ~Date, y = ~Percent_byState_Demo2, type = 'scatter', mode = 'lines',
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
                  x0 = min(News_NoNews()$Date), x1 = max(News_NoNews()$Date),
                  y0 =0, y1 = 0.5
                ),
                list(
                  type = "rect",
                  fillcolor = "rgba(22, 96, 167, 0.2)", # Light blue fill for 270-400
                  line = list(color = "rgba(22, 96, 167, 0)"), # No border
                  x0 = min(News_NoNews()$Date), x1 = max(News_NoNews()$Date),
                  y0 = 0.5, y1 = 1
                ),
                list(
                  type = "line",
                  x0 = min(News_NoNews()$Date), x1 = max(News_NoNews()$Date),
                  y0 = 0.5, y1 = 0.5,
                  line = list(color = "rgb(0, 0, 0)", dash = 'dash', width = 2)
                )
              )
            ) %>%
            layout(annotations = list(
              list(
                x = min(News_NoNews()$Date) + 5,
                y = 0.65,
                text = "Democrat Win",
                showarrow = FALSE,
                font = list(size = 12, weight = "bold", color = "rgb(22, 96, 167)"),
                showgrid = FALSE
              ),
              list(
                x = min(News_NoNews()$Date) + 5,
                y = 0.4,
                text = "Republican Win",
                showarrow = FALSE,
                font = list(size = 12, weight = "bold", color =  "rgb(205, 12, 24)")
              )
            ))
        }
        fig  # Return the plotly figure
      } else {
        # Initialize the plotly object
        fig <- plot_ly()
        # Loop through each selected state and add a trace for it
        for (k in seq_along(input$statesInput)){
          state_data <- News_NoNews()[News_NoNews()$StateFull == input$statesInput[k], ]
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
                  x0 = min(News_NoNews()$Date), x1 = max(News_NoNews()$Date),
                  y0 =0, y1 = 0.5
                ),
                list(
                  type = "rect",
                  fillcolor = "rgba(22, 96, 167, 0.2)", # Light blue fill for 270-400
                  line = list(color = "rgba(22, 96, 167, 0)"), # No border
                  x0 = min(News_NoNews()$Date), x1 = max(News_NoNews()$Date),
                  y0 = 0.5, y1 = 1
                ),
                list(
                  type = "line",
                  x0 = min(News_NoNews()$Date), x1 = max(News_NoNews()$Date),
                  y0 = 0.5, y1 = 0.5,
                  line = list(color = "rgb(0, 0, 0)", dash = 'dash', width = 2)
                )
              )
            ) %>%
            layout(annotations = list(
              list(
                x = min(News_NoNews()$Date) + 5,
                y = 0.65,
                text = "Democrat Win",
                showarrow = FALSE,
                font = list(size = 12, weight = "bold", color = "rgb(22, 96, 167)"),
                showgrid = FALSE
              ),
              list(
                x = min(News_NoNews()$Date) + 5,
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

  

  

  observe({
    data <- News_NoNews()  # Store the reactive output in a variable
    print("News_NoNews data has changed!")  # Log a message to the console
    print(head(data))  # Print the first few rows of the data for inspection
  })
  
  
  # UI Time Trend - 3 ------------------------------------------------------------------
  output$box_pat8_1 <- renderUI({
    div(
      style = "position: relative; backgroundColor: #ecf0f5",
      tabBox(
        id = "box_pat",
        width = NULL,
        height = 320, 
        tabPanel(
          title = "Average Harris Electoral College Votes Win",
          div(
            style = "position: absolute; left:0.5em; bottom: 0.5em;",
            dropdown(
              radioGroupButtons(
                inputId = "box_votes",
                label = "Select time period", 
                choiceNames = c("News", "No News", "Combined"),
                choiceValues = c("With News", "Without News", "Combined"), 
                selected = "With News",  
                direction = "vertical"
              ),
              size = "xs",
              icon = icon("gear", class = "opt"), 
              up = TRUE
            )
          ),
          withSpinner(
            plotlyOutput("distPlot1", height = 230),
            type = 4,
            color = "#d33724", 
            size = 0.7 
          )
        )
      )
    )
  })
  
  # UI Time Trend -4 Average Win by Trial ------------------------------------------------------------------
  output$box_pat7 <- renderUI({
    div(
      style = "position: relative; backgroundColor: #ecf0f5",
      tabBox(
        id = "box_pat",
        width = NULL,
        height = 320,
        tabPanel(
          title = "Average Harris Win by Trial",
          div(
            style = "position: absolute; left:0.5em; bottom: 0.5em;",
            dropdown(
              radioGroupButtons(
                inputId = "box_monte",
                label = "Select time period", 
                choiceNames = c("News", "No News"),
                choiceValues = c("With News", "Without News"), 
                selected = "With News",  
                direction = "vertical"
              ),
              size = "xs",
              icon = icon("gear", class = "opt"), 
              up = TRUE
            )
        ),
        withSpinner(
          plotlyOutput("distPlot", height = 230),
          type = 4,
          color = "#d33724", 
          size = 0.7 
        )
      )
    )
    )
  })
  


  
  # UI 9 -----Expert -Overall
  output$box_pat9 <- renderUI({
    div(
      style = "position: relative; backgroundColor: #ecf0f5",
      tabBox(
        id = "box_table",
        width = NULL,
        height = 320, 
        tabPanel(
          title = "ElectionGPT Opinion",
          div(
            style = "position: absolute; left:0.5em; bottom: 0.5em;",
            dropdown(
              radioGroupButtons(
                inputId = "box_year2",
                label = "Select time period", 
                choiceNames = c("Votes Percent", "Votes"),
                choiceValues = c("Votes Percent", "Votes"), 
                selected = "Votes Percent",    
                direction = "vertical"
              ),
              size = "xs",
              icon = icon("gear", class = "opt"), 
              up = TRUE
            )
          ),
          withSpinner(
            plotlyOutput("distPlot3", height = 230),
            type = 4,
            color = "#d33724", 
            size = 0.7 
          )
        )
      )
    )
  })
  
  output$box_pat10 <- renderUI({
    div(
      style = "position: relative; backgroundColor: #ecf0f5",
      tabBox(
        id = "box_table",
        width = NULL,
        height = 320, 
        tabPanel(
          title = "Experts Opinion",
          div(
            style = "position: absolute; left:0.5em; bottom: 0.5em;",
            dropdown(
              radioGroupButtons(
                inputId = "box_expert",
                label = "Select time period", 
                choiceNames = c("Nate Silver", "Economist", "Times Siena"),
                choiceValues = c("Nate Silver", "Economist", "Times Siena"), 
                selected = "Nate Silver",  
                direction = "vertical"
              ),
              size = "xs",
              icon = icon("gear", class = "opt"), 
              up = TRUE
            )
        ),
        withSpinner(
          plotlyOutput("distPlot5", height = 230),
          type = 4,
          color = "#d33724", 
          size = 0.7 
        )
      )
    )
    )
  })
  
  output$box_pat11 <- renderUI({
    div(
      style = "position: relative; backgroundColor: #ecf0f5",
      tabBox(
        id = "box_table",
        width = NULL,
        height = 320, 
        tabPanel(
          title = "Nate  Opinions "
        ),
        withSpinner(
          plotlyOutput("distPlot5", height = 230),
          type = 4,
          color = "#d33724", 
          size = 0.7 
        )
      )
    )
  })
  
  output$box_pat12 <- renderUI({
    div(
      style = "position: relative; backgroundColor: #ecf0f5",
      tabBox(
        id = "box_table",
        width = NULL,
        height = 320, 
        tabPanel(
          title = "Times Siena Opinion "
        ),
        withSpinner(
          plotlyOutput("distPlot6", height = 230),
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
    input$date
    input$box_map1
    
    if (input$box_map1 == "With News") {
      
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
        ticks = "outside", # Set specific tick values
        cmin = 0,  # Ensure the minimum value is fixed to 0
        cmax = 1,  # Ensure the maximum value is fixed to 1
        ticktext = c("0", "25%", "50%", "75%", "100%")  # Labels for ticks
      )
      
      fig <- fig %>% layout(
        title = input$box_map1
      )
      
      
      fig <- fig %>% layout(
        geo = g
      )
      
      fig
    } else {
      l <- list(color = toRGB("white"), width = 1)
      
      g <- list(
        scope = 'usa',
        projection = list(type = 'albers usa'),
        showlakes = FALSE
      )
      
      fig <- plot_geo(USA_map_Anonymous(), locationmode = 'USA-states', marker = list(line = l)
      )
      fig <- fig %>% add_trace(
        z = ~Percent_byState_Demo2,  
        text = ~hover2,       # Hover text with details
        locations = ~state,  # State abbreviations
        color = ~Percent_byState_Demo2,
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
        title = input$box_map1
      )
      
      
      fig <- fig %>% layout(
        geo = g
      )
      
      fig
    }
  })  
  
  
  #-------Map 2 BBC

  
  # Render the Plotly map #2
  output$box_map_BBC <- renderPlotly ({
    input$date
    input$box_map2
    
    if (input$box_map2 == "With News") {
      
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
        title = input$box_map2
      )
      
      
      fig <- fig %>% layout(
        geo = g
      )
      
      fig
    } else {
      l <- list(color = toRGB("white"), width = 1)
      
      g <- list(
        scope = 'usa',
        projection = list(type = 'albers usa'),
        showlakes = FALSE
      )
      
      fig <- plot_geo(USA_map_BBC(), locationmode = 'USA-states', marker = list(line = l)
      )
      fig <- fig %>% add_trace(
        z = ~Percent_byState_Demo2,  
        text = ~hover2,       # Hover text with details
        locations = ~state,  # State abbreviations
        color = ~Percent_byState_Demo2,
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
        title = input$box_map2
      )
      
      
      
      fig <- fig %>% layout(
        geo = g
      )
      
      fig
    }
  })  
  
  #-------Map 3 Fox
  
  # Render the Plotly map #1
  output$box_map_Fox <- renderPlotly ({
    input$date
    input$box_map3
    
    if (input$box_map3 == "With News") {
      
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
        title = input$box_map3
      )
      
      
      fig <- fig %>% layout(
        geo = g
      )
      
      fig
    } else {
      l <- list(color = toRGB("white"), width = 1)
      
      g <- list(
        scope = 'usa',
        projection = list(type = 'albers usa'),
        showlakes = FALSE
      )
      
      fig <- plot_geo(USA_map_Fox(), locationmode = 'USA-states', marker = list(line = l)
      )
      fig <- fig %>% add_trace(
        z = ~Percent_byState_Demo2,  
        text = ~hover2,       # Hover text with details
        locations = ~state,  # State abbreviations
        color = ~Percent_byState_Demo2,
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
        title = input$box_map3
      )
      
      
      fig <- fig %>% layout(
        geo = g
      )
      
      fig
    }
  })  
  
  #-------Map 4 MSNBC
  
  # Render the Plotly map #1
  output$box_map_MSNBC <- renderPlotly ({
    input$date
    input$box_map4
    
    if (input$box_map4 == "With News") {

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
      title = input$box_map4
    )
    
    
    fig <- fig %>% layout(
      geo = g
    )
    
    fig
    } else {
      l <- list(color = toRGB("white"), width = 1)
      
      g <- list(
        scope = 'usa',
        projection = list(type = 'albers usa'),
        showlakes = FALSE
      )
      
      fig <- plot_geo(USA_map_MSNBC(), locationmode = 'USA-states', marker = list(line = l)
      )
      fig <- fig %>% add_trace(
        z = ~Percent_byState_Demo2,  
        text = ~hover2,       # Hover text with details
        locations = ~state,  # State abbreviations
        color = ~Percent_byState_Demo2,
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
        title = input$box_map4
      )
      
      
      fig <- fig %>% layout(
        geo = g
      )
  
      fig
    }
  })  
  
  

  # UI #6 Time series by state 
  output$plot_state2 <- renderPlotly({
    input$allInput
    input$voicechoice
    input$statesInput
    input$date2
    input$box_year1
    #input$confirm
    #input$partychoice
    
    if (input$box_year1 == "Without News") {
      fig <- plot_ly()
      # Loop through each selected state and add a trace for it
      for (k in seq_along(input$statesInput)){
        state_data <- News_NoNews()[News_NoNews()$StateFull == input$statesInput[k], ]
        fig <- add_trace(fig, data = state_data, 
                         x = ~Date, y = ~Percent_byState_Demo2, type = 'scatter', mode = 'lines',
                         name = input$statesInput[k],
                         line = list(width = 2)) %>%
          layout(
            title= input$box_year1,
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
                x0 = min(News_NoNews()$Date), x1 = max(News_NoNews()$Date),
                y0 =0, y1 = 0.5
              ),
              list(
                type = "rect",
                fillcolor = "rgba(22, 96, 167, 0.2)", # Light blue fill for 270-400
                line = list(color = "rgba(22, 96, 167, 0)"), # No border
                x0 = min(News_NoNews()$Date), x1 = max(News_NoNews()$Date),
                y0 = 0.5, y1 = 1
              ),
              list(
                type = "line",
                x0 = min(News_NoNews()$Date), x1 = max(News_NoNews()$Date),
                y0 = 0.5, y1 = 0.5,
                line = list(color = "rgb(0, 0, 0)", dash = 'dash', width = 2)
              )
            )
          ) %>%
          layout(annotations = list(
            list(
              x = min(News_NoNews()$Date) + 5,
              y = 0.65,
              text = "Democrat Win",
              showarrow = FALSE,
              font = list(size = 12, weight = "bold", color = "rgb(22, 96, 167)"),
              showgrid = FALSE
            ),
            list(
              x = min(News_NoNews()$Date) + 5,
              y = 0.4,
              text = "Republican Win",
              showarrow = FALSE,
              font = list(size = 12, weight = "bold", color =  "rgb(205, 12, 24)")
            )
          ))
      }
      fig  # Return the plotly figure
    } else {
      # Initialize the plotly object
      fig <- plot_ly()
      # Loop through each selected state and add a trace for it
      for (k in seq_along(input$statesInput)){
        state_data <- News_NoNews()[News_NoNews()$StateFull == input$statesInput[k], ]
        fig <- add_trace(fig, data = state_data, 
                         x = ~Date, y = ~Percent_byState_Demo, type = 'scatter', mode = 'lines',
                         name = input$statesInput[k],
                         line = list(width = 2)) %>%
          layout(
            title= input$box_year1,
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
                x0 = min(News_NoNews()$Date), x1 = max(News_NoNews()$Date),
                y0 =0, y1 = 0.5
              ),
              list(
                type = "rect",
                fillcolor = "rgba(22, 96, 167, 0.2)", # Light blue fill for 270-400
                line = list(color = "rgba(22, 96, 167, 0)"), # No border
                x0 = min(News_NoNews()$Date), x1 = max(News_NoNews()$Date),
                y0 = 0.5, y1 = 1
              ),
              list(
                type = "line",
                x0 = min(News_NoNews()$Date), x1 = max(News_NoNews()$Date),
                y0 = 0.5, y1 = 0.5,
                line = list(color = "rgb(0, 0, 0)", dash = 'dash', width = 2)
              )
            )
          ) %>%
          layout(annotations = list(
            list(
              x = min(News_NoNews()$Date) + 5,
              y = 0.65,
              text = "Democrat Win",
              showarrow = FALSE,
              font = list(size = 12, weight = "bold", color = "rgb(22, 96, 167)"),
              showgrid = FALSE
            ),
            list(
              x = min(News_NoNews()$Date) + 5,
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
  

  # monte carlo
  output$distPlot <- renderPlotly({
    input$date2
    input$box_monte
    #input$confirm
    if (input$box_monte=="With News"){
      fig <- plot_ly(monte_carlo(), x = ~Date, y = ~Trial_Percent_Anonymous, name = 'Anonymous', type = 'scatter', mode = 'lines',
                     line = list(color = 'rgb(205, 12, 24)', width = 4)) 
      fig <- fig %>% add_trace(y = ~Trial_Percent_BBC, name = 'BBC', line = list(color = 'rgb(22, 96, 167)', width = 4)) 
      fig <- fig %>% add_trace(y = ~Trial_Percent_Fox, name = 'Fox', line = list(color = 'rgb(205, 12, 24)', width = 4, dash = 'dash')) 
      fig <- fig %>% add_trace(y = ~Trial_Percent_MSNBC, name = 'MSNBC', line = list(color = 'rgb(22, 96, 167)', width = 4, dash = 'dot')) %>%
        layout(
          title = input$box_monte,
          xaxis = list(title = "Date",
                       showgrid = TRUE),
          yaxis = list(title = "Percent", 
                       range = c(0, 1.1),
                       showgrid = FALSE),
          shapes = list(
            list(
              type = "rect",
              fillcolor = "rgba(205, 12, 24, 0.2)", # Light red fill for 140-270
              line = list(color = "rgba(205, 12, 24, 0)"), # No border
              x0 = min(monte_carlo()$Date), x1 = max(monte_carlo()$Date),
              y0 =0, y1 = 0.5
            ),
            list(
              type = "rect",
              fillcolor = "rgba(22, 96, 167, 0.2)", # Light blue fill for 270-400
              line = list(color = "rgba(22, 96, 167, 0)"), # No border
              x0 = min(monte_carlo()$Date), x1 = max(monte_carlo()$Date),
              y0 = 0.5, y1 = 1.1
            ),
            list(
              type = "line",
              x0 = min(monte_carlo()$Date), x1 = max(monte_carlo()$Date),
              y0 = 0.5, y1 = 0.5,
              line = list(color = "rgb(0, 0, 0)", dash = 'dash', width = 2)
            )
          )
        ) %>%
        layout(annotations = list(
          list(
            x = min(monte_carlo()$Date) + 5,
            y = 0.65,
            text = "Democrat Win",
            showarrow = FALSE,
            font = list(size = 12, weight = "bold", color = "rgb(22, 96, 167)"),
            showgrid = FALSE
          ),
          list(
            x = min(monte_carlo()$Date) + 5,
            y = 0.4,
            text = "Republican Win",
            showarrow = FALSE,
            font = list(size = 12, weight = "bold", color =  "rgb(205, 12, 24)")
          )
        ))
    } else {
      fig <- plot_ly(monte_carlo2(), x = ~Date, y = ~Trial_Percent_Anonymous, name = 'Anonymous', type = 'scatter', mode = 'lines',
                     line = list(color = 'rgb(205, 12, 24)', width = 4)) 
      fig <- fig %>% add_trace(y = ~Trial_Percent_BBC, name = 'BBC', line = list(color = 'rgb(22, 96, 167)', width = 4)) 
      fig <- fig %>% add_trace(y = ~Trial_Percent_Fox, name = 'Fox', line = list(color = 'rgb(205, 12, 24)', width = 4, dash = 'dash')) 
      fig <- fig %>% add_trace(y = ~Trial_Percent_MSNBC, name = 'MSNBC', line = list(color = 'rgb(22, 96, 167)', width = 4, dash = 'dot')) %>%
        layout(
          title = input$box_monte,
          xaxis = list(title = "Date",
                       showgrid = TRUE),
          yaxis = list(title = "Percent", 
                       range = c(0, 1.1),
                       showgrid = FALSE),
          shapes = list(
            list(
              type = "rect",
              fillcolor = "rgba(205, 12, 24, 0.2)", # Light red fill for 140-270
              line = list(color = "rgba(205, 12, 24, 0)"), # No border
              x0 = min(monte_carlo2()$Date), x1 = max(monte_carlo2()$Date),
              y0 =0, y1 = 0.5
            ),
            list(
              type = "rect",
              fillcolor = "rgba(22, 96, 167, 0.2)", # Light blue fill for 270-400
              line = list(color = "rgba(22, 96, 167, 0)"), # No border
              x0 = min(monte_carlo2()$Date), x1 = max(monte_carlo2()$Date),
              y0 = 0.5, y1 = 1.1
            ),
            list(
              type = "line",
              x0 = min(monte_carlo2()$Date), x1 = max(monte_carlo2()$Date),
              y0 = 0.5, y1 = 0.5,
              line = list(color = "rgb(0, 0, 0)", dash = 'dash', width = 2)
            )
          )
        ) %>%
        layout(annotations = list(
          list(
            x = min(monte_carlo2()$Date) + 5,
            y = 0.65,
            text = "Democrat Win",
            showarrow = FALSE,
            font = list(size = 12, weight = "bold", color = "rgb(22, 96, 167)"),
            showgrid = FALSE
          ),
          list(
            x = min(monte_carlo2()$Date) + 5,
            y = 0.4,
            text = "Republican Win",
            showarrow = FALSE,
            font = list(size = 12, weight = "bold", color =  "rgb(205, 12, 24)")
          )
        ))
    }
  })
  
  
  
  # electorial votes decided already to be used
  output$distPlot1 <- renderPlotly({
    input$date2
    input$box_votes
    
    if (input$box_votes=="With News"){
    fig <- plot_ly(Votes_final(), x = ~Date, y = ~Votes_Anonymous, name = 'Anonymous', type = 'scatter', mode = 'lines',
                   line = list(color = 'rgb(205, 12, 24)', width = 4)) 
    fig <- fig %>% add_trace(y = ~Votes_BBC, name = 'BBC', line = list(color = 'rgb(22, 96, 167)', width = 4)) 
    fig <- fig %>% add_trace(y = ~Votes_Fox, name = 'Fox', line = list(color = 'rgb(205, 12, 24)', width = 4, dash = 'dash')) 
    fig <- fig %>% add_trace(y = ~Votes_MSNBC, name = 'MSNBC', line = list(color = 'rgb(22, 96, 167)', width = 4, dash = 'dot')) %>%
      layout(
        title =  input$box_votes,
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
    } else if (input$box_votes=="Without News"){
      fig <- plot_ly(Votes_final_2(), x = ~Date, y = ~NoNews_Votes_Anonymous , name = 'Anonymous', type = 'scatter', mode = 'lines',
                     line = list(color = 'rgb(205, 12, 24)', width = 4)) 
      fig <- fig %>% add_trace(y = ~NoNews_Votes_BBC , name = 'BBC', line = list(color = 'rgb(22, 96, 167)', width = 4)) 
      fig <- fig %>% add_trace(y = ~NoNews_Votes_Fox , name = 'Fox', line = list(color = 'rgb(205, 12, 24)', width = 4, dash = 'dash')) 
      fig <- fig %>% add_trace(y = ~NoNews_Votes_MSNBC  , name = 'MSNBC', line = list(color = 'rgb(22, 96, 167)', width = 4, dash = 'dot')) %>%
        layout(
          title = input$box_votes,
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
              x0 = min(Votes_final_2()$Date), x1 = max(Votes_final_2()$Date),
              y0 = 140, y1 = 270
            ),
            list(
              type = "rect",
              fillcolor = "rgba(22, 96, 167, 0.2)", # Light blue fill for 270-400
              line = list(color = "rgba(22, 96, 167, 0)"), # No border
              x0 = min(Votes_final_2()$Date), x1 = max(Votes_final_2()$Date),
              y0 = 270, y1 = 400
            ),
            list(
              type = "line",
              x0 = min(Votes_final_2()$Date), x1 = max(Votes_final_2()$Date),
              y0 = 270, y1 = 270,
              line = list(color = "rgb(0, 0, 0)", dash = 'dash', width = 2)
            )
          )
        ) %>%
        layout(annotations = list(
          list(
            x = min(Votes_final_2()$Date) + 5,
            y = 350,
            text = "Democrat Win",
            showarrow = FALSE,
            font = list(size = 12, weight = "bold", color = "rgb(22, 96, 167)"),
            showgrid = FALSE
          ),
          list(
            x = min(Votes_final_2()$Date) + 5,
            y = 200,
            text = "Republican Win",
            showarrow = FALSE,
            font = list(size = 12, weight = "bold", color =  "rgb(205, 12, 24)")
          )
        ))
    } else {
      fig <- plot_ly(Votes_final_3(), x = ~Date, y = ~Votes_Anonymous , name = 'News Anonymous', type = 'scatter', mode = 'lines',
                     line = list(color = 'rgb(205, 12, 24)', width = 4)) 
      fig <- fig %>% add_trace(y = ~Votes_BBC , name = 'News BBC', line = list(color = 'rgb(22, 96, 167)', width = 4)) 
      fig <- fig %>% add_trace(y = ~Votes_Fox , name = 'News Fox', line = list(color = 'rgb(205, 12, 24)', width = 4, dash = 'dash')) 
      fig <- fig %>% add_trace(y = ~Votes_MSNBC  , name = 'News MSNBC', line = list(color = 'rgb(22, 96, 167)', width = 4, dash = 'dot')) 
      fig <- fig %>% add_trace(y = ~NoNews_Votes_Anonymous , name = 'No News Anonymous', line = list(color = 'purple', width = 4, mode = 'lines')) 
      fig <- fig %>% add_trace(y = ~NoNews_Votes_BBC , name = 'No News BBC', line = list(color = 'purple', width = 4, dash = 'dash')) 
      fig <- fig %>% add_trace(y = ~NoNews_Votes_Fox  , name = 'No News Fox', line = list(color = 'darkgreen', width = 4, dash = 'lines'))
      fig <- fig %>% add_trace(y = ~NoNews_Votes_MSNBC  , name = 'No News MSNBC', line = list(color = 'orange', width = 4, dash = 'lines'))%>%
        layout(
          title = input$box_votes,
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
              x0 = min(Votes_final_3()$Date), x1 = max(Votes_final_3()$Date),
              y0 = 140, y1 = 270
            ),
            list(
              type = "rect",
              fillcolor = "rgba(22, 96, 167, 0.2)", # Light blue fill for 270-400
              line = list(color = "rgba(22, 96, 167, 0)"), # No border
              x0 = min(Votes_final_3()$Date), x1 = max(Votes_final_3()$Date),
              y0 = 270, y1 = 400
            ),
            list(
              type = "line",
              x0 = min(Votes_final_3()$Date), x1 = max(Votes_final_3()$Date),
              y0 = 270, y1 = 270,
              line = list(color = "rgb(0, 0, 0)", dash = 'dash', width = 2)
            )
          )
        ) %>%
        layout(annotations = list(
          list(
            x = min(Votes_final_3()$Date) + 5,
            y = 350,
            text = "Democrat Win",
            showarrow = FALSE,
            font = list(size = 12, weight = "bold", color = "rgb(22, 96, 167)"),
            showgrid = FALSE
          ),
          list(
            x = min(Votes_final_3()$Date) + 5,
            y = 200,
            text = "Republican Win",
            showarrow = FALSE,
            font = list(size = 12, weight = "bold", color =  "rgb(205, 12, 24)")
          )
        ))
    }
  })
  
  

  
  # Image in About----------------------------------------------------------
  
  output$home_img <- renderImage({
    list(src = "www/Jared_Image2.png", contentType = 'image/png')
  }, deleteFile = FALSE)

  
  # Expert graph

  

  
  
  # Assuming expert_nate is your filtered dataset for the 'silver' source
  output$distPlot5 <- renderPlotly({
    input$date2
    input$box_expert
    if (input$box_expert=="Nate Silver") {
      fig <- plot_ly(expert_silver(), x = ~Date, y = ~Harris, name = 'Harris', type = 'scatter', mode = 'lines',
                     line = list(color = 'rgb(22, 96, 167)', width = 4))
      
      fig <- fig %>% add_trace(y = ~Trump, name = 'Trump', line = list(color = 'rgb(205, 12, 24)', width = 4)) %>%
        layout(
          title = input$box_expert,
          xaxis = list(title = "Date", showgrid = TRUE),
          yaxis = list(title = "Percent", range = c(0.30, 0.70), showgrid = FALSE)
        )
      
    fig  # Return the final plotly figure
    
    } else if (input$box_expert=="Economist") {
      fig <- plot_ly(expert_Economist(), x = ~Date, y = ~Harris, name = 'Harris', type = 'scatter', mode = 'lines',
                     line = list(color = 'rgb(22, 96, 167)', width = 4))
      
      fig <- fig %>% add_trace(y = ~Trump, name = 'Trump', line = list(color = 'rgb(205, 12, 24)', width = 4)) %>%
        layout(
          title = input$box_expert,
          xaxis = list(title = "Date", showgrid = TRUE),
          yaxis = list(title = "Percent", range = c(0.30, 0.70), showgrid = FALSE)
        )
      
      fig  # Return the final plotly figure
    } else {
      fig <- plot_ly(expert_times(), x = ~Date, y = ~Harris, name = 'Harris', type = 'scatter', mode = 'lines',
                     line = list(color = 'rgb(22, 96, 167)', width = 4))
      
      fig <- fig %>% add_trace(y = ~Trump, name = 'Trump', line = list(color = 'rgb(205, 12, 24)', width = 4)) %>%
        layout(
          title = input$box_expert,
          xaxis = list(title = "Date", showgrid = TRUE),
          yaxis = list(title = "Percent", range = c(0.30, 0.70), showgrid = FALSE)
        )
      
      fig  # Return the final plotly figure
      
    }
  })
  
  

  # votes percent 
  output$distPlot3 <- renderPlotly({
    input$date2
    input$box_year2
    #input$confirm
    if (input$box_year2=="Votes Percent"){
      fig <- plot_ly(Votes_final(), x = ~Date, y = ~Votes_Percent_Anonymous , name = 'Anonymous', type = 'scatter', mode = 'lines',
                     line = list(color = 'rgb(205, 12, 24)', width = 4)) 
      fig <- fig %>% add_trace(y = ~Votes_Percent_BBC , name = 'BBC', line = list(color = 'rgb(22, 96, 167)', width = 4)) 
      fig <- fig %>% add_trace(y = ~Votes_Percent_Fox , name = 'Fox', line = list(color = 'rgb(205, 12, 24)', width = 4, dash = 'dash')) 
      fig <- fig %>% add_trace(y = ~Votes_Percent_MSNBC, name = 'MSNBC', line = list(color = 'rgb(22, 96, 167)', width = 4, dash = 'dot')) %>%
        layout(
          title = "Average Harris Electoral College Votes Out of 538",
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
              y0 =0, y1 = 0.5
            ),
            list(
              type = "rect",
              fillcolor = "rgba(22, 96, 167, 0.2)", # Light blue fill for 270-400
              line = list(color = "rgba(22, 96, 167, 0)"), # No border
              x0 = min(Votes_final()$Date), x1 = max(Votes_final()$Date),
              y0 = 0.5, y1 = 1.1
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
    } else {
      fig <- plot_ly(Votes_final(), x = ~Date, y = ~Votes_Anonymous, name = 'Anonymous', type = 'scatter', mode = 'lines',
                     line = list(color = 'rgb(205, 12, 24)', width = 4)) 
      fig <- fig %>% add_trace(y = ~Votes_BBC, name = 'BBC', line = list(color = 'rgb(22, 96, 167)', width = 4)) 
      fig <- fig %>% add_trace(y = ~Votes_Fox, name = 'Fox', line = list(color = 'rgb(205, 12, 24)', width = 4, dash = 'dash')) 
      fig <- fig %>% add_trace(y = ~Votes_MSNBC, name = 'MSNBC', line = list(color = 'rgb(22, 96, 167)', width = 4, dash = 'dot')) %>%
        layout(
          title = "Average Harris Electoral College Votes",
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
    }
  })
  
  

  
     
}





shinyApp(ui, server)