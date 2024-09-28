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



data_sentiment<-read.csv("/Users/sunmingrun/Documents/GitHub/ElectionGPT/NLP/news_data_csv/combined_news_data_2024.csv")

data_sentiment<-data_sentiment%>%
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

