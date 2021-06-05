# Machine Learning Final Project
# Reddit and WallStreetBets Sentiment Analysis / Machine Learning
library(dplyr)
library(quanteda)
library(readtext)
library(stringr)
library(tidyverse)
library(reshape2)
library(class)

setwd("C:/Labs/Machine Learning/Final Paper/Code")


# Data Import -------------------------------------------------------------


# Sentiment Data
data = read.csv("C:/Labs/Machine Learning/Final Paper/Data/sentiment/reddit_sentiment.csv")
data = data[,2:8]
data$texlen = nchar(data$text)

####### Stock Data

# How to filter for a stock
gme_data = data %>%
  select(text, score, num_comments, timestamp, sentences, scores, scores_avg,texlen) %>%   
  filter(str_detect(text, "GME|game stop|gme|Game Stop"))

# GME closing values
gme = read.csv("C:/Labs/Machine Learning/Final Paper/Data/stocks/GME_jan_may_21.csv")
gme = gme[,-(6:7)]
gme = gme[,-(2:4)]



# Data Cleaning -----------------------------------------------------------

# simple return: percentage change for gme stock
gme = gme %>%
  mutate(pct_change = (Close/lag(Close) - 1) * 100)

# direction of price change
gme['direction'] = sign(gme['pct_change'])

# adpt type of date in both data set
gme$Date = as.Date(gme$Date)
gme_data$timestamp = as.Date(gme_data$timestamp)

# Rank Sentiment Data
df = gme_data[,c(3,4,7,8)]  # drop columns with texts

data.frame(table(df$timestamp)) # count how many posts for each day

df = arrange(df,timestamp, -num_comments) # set order for data: increase with date, decrease with number of comments

# Keep the data with top 10 largest number of comments
df <- df %>% 
  group_by(timestamp) %>% 
  mutate(rank = row_number()) %>% 
  select(timestamp,rank, scores_avg) %>% 
  filter(rank <= 10)

# create variable names for next step
df['variable'] = paste0('Top', as.character(df$rank))

# transform the long panel data in to wide panel data
wide <- select(df,timestamp,variable, scores_avg) %>%
  dcast(timestamp ~ variable, value.var = "scores_avg") %>%
  select(timestamp,Top1,Top2,Top3,Top4,Top5,Top6,Top7,Top8,Top9,Top10)

# join the sentiment data with stock data, order by date
ml_join_data <- left_join(gme, wide,by = c("Date" = "timestamp")) %>%
  arrange(Date)

# drop the observations with empty value
gme_final_data <- na.omit(ml_join_data)

write.csv(gme_final_data, 
          file = "C:/Labs/Machine Learning/Final Paper/Data/analysis data/gme.csv",quote=F,row.names = F)