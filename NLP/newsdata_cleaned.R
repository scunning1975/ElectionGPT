# data cleaning

setwd("/Users/sunmingrun/Documents/GitHub/ElectionGPT/NLP")
data <- read_csv("combined_news_data.csv",show_col_types = FALSE)

data_cleaned <-data %>%
  select(date,sentiment,relevance,source.uri,source.title) %>%
  arrange(date)

#View(data_cleaned)

write_csv(newsdata_cleaned,"news_data.csv")
