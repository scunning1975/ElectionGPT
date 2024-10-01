library(usmap) 
library(ggplot2)
library(maps)
library(openxlsx)
library(readxl)
library(dplyr)
library(tidyr)
library(tidyverse) 
library(PNWColors)
library(plotly)


# ----------------------#
# Election Data Process #
#                       #
#-----------------------#

# 1 data import
data<-read_csv("/Users/sunmingrun/Documents/GitHub/ElectionGPT/data/panel_election_results_state.csv",show_col_types = FALSE)


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



##assign the votes first and then calculate the average
subdata3_1<-melted_data %>%
  group_by(Type, Trial, Date,party) %>%
  summarise(
    Number_Repub_Win = n(),
    votes_party =sum(Electoral_Votes),
    .groups = 'drop'
  ) 

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

#*************** Dataset with average votes 
average_votes<- trial_votes %>%
  select(Date, Type, party, average_votes)%>%
  rename(
    Voice = Type,  # Renaming 'Result' to 'value'
    Party= party,
    AverageVotes=average_votes)


#------------Election data  process done

# ----------------------#
# Sentiment Score Data  #
#                       #
#-----------------------#

data_sentiment_raw<-read.csv("/Users/sunmingrun/Documents/GitHub/ElectionGPT/NLP/news_data_csv/combined_news_data_2024.csv")

data_sentiment<-data_sentiment_raw%>%
  distinct()%>%
  select("feed.date","date","sentiment")%>%
  mutate(date = as.Date(date, format = "%Y-%m-%d")) %>%
  mutate(feed.date = as.Date(feed.date, format = "%Y-%m-%d")) %>%
  filter(date>="2024-08-13")%>%
  arrange("feed.date")%>%
  group_by(feed.date) %>%  # Remove quotes around date
  summarise(
    avg_sentiment = mean(sentiment, na.rm = TRUE),
    No_News = n(),
    .groups = 'drop'  # Ensure no group is carried forward after summarise
  )



# ----------------------#
# Merge Dataset         #
#                       #
#-----------------------#

data_sentiment_formege<-data_sentiment%>%
  rename(
    Date=feed.date
  )


avgvotes_sentiment <- average_votes %>%
  left_join(data_sentiment_formege, by="Date")

# ----------------------#
# Check for balance data#
#                       #
#-----------------------#

check <- avgvotes_sentiment%>%
  filter(Party=="Democratic") %>%
  group_by(Date) %>%
  summarise(
    Total_number =n()
  )

# ----------------------#
# Create CSV for stata  #
#                       #
#-----------------------#

write_csv(avgvotes_sentiment,"/Users/sunmingrun/Documents/GitHub/ElectionGPT/NLP/avgvotes_sentiment.csv")


# ----------------------#
# Graph 1               #
# Sentiment score graph #
#                       #
#-----------------------#

fig <- plot_ly(data_sentiment, x = ~feed.date, y = ~avg_sentiment, type = 'scatter', mode = 'lines',
               showlegend = FALSE) 

fig <- fig %>% layout(title = "Sentiment Analysis",
                      #paper_bgcolor='rgb(255,255,255)', plot_bgcolor='rgb(229,229,229)',
                      xaxis = list(title = "Date",
                                   #gridcolor = 'rgb(255,255,255)',
                                   showgrid = FALSE,
                                   showline = FALSE,
                                   showticklabels = TRUE,
                                   #tickcolor = 'rgb(127,127,127)',
                                   ticks = 'outside',
                                   zeroline = FALSE),
                      yaxis = list(title = "Average Sentiment Score By Day",
                                   #gridcolor = 'rgb(255,255,255)',
                                   showgrid = TRUE,
                                   showline = FALSE,
                                   showticklabels = TRUE,
                                   #tickcolor = 'rgb(127,127,127)',
                                   ticks = 'outside',
                                   zeroline = FALSE))

fig

# ---------------------------------------------PART 2 ------------------------------------------

# ----------------------#
# Create Dataset        #
#                       #
#-----------------------#

data_state_counts_raw <- read_csv("/Users/sunmingrun/Documents/GitHub/ElectionGPT/NLP/state_full_count_title_body.csv",show_col_types = FALSE)

data_state_counts <- data_state_counts_raw %>%
  mutate(StateFull=(ifelse(state=="DC","District of Columbia",state))) %>%
  arrange(feed_date) %>%
  filter(feed_date>="2024-08-13") %>%
  rename(
    Date="feed_date",
    counts="state_name_count"
  ) %>%
  select(-state)%>%
  mutate(state = state.abb[match(StateFull, state.name)])%>%
  mutate(state = ifelse(StateFull == "District of Columbia", "DC", state))

check_state_counts <-data_state_counts %>%
  group_by(Date) %>%
  summarise(
    No_states=n()
  )

# ----------------------#
# Graph 2               #
# State counts graph    #
#                       #
#-----------------------#



# --------trash
data_sentiment<-read.csv("/Users/sunmingrun/Documents/GitHub/ElectionGPT/NLP/JSON_news_data.csv") 
data_sentiment2 <- data_sentiment %>%
  filter(date=="2024-09-14")

data_sentiment3 <- data_sentiment %>%
  select(-"body") %>%
  mutate(date = as.Date(date, format = "%Y-%m-%d")) %>%
  group_by(date) %>%  # Remove quotes around date
  summarise(
    avg_sentiment = mean(sentiment, na.rm = TRUE),
    No_News = n(),
    .groups = 'drop'  # Ensure no group is carried forward after summarise
  )


