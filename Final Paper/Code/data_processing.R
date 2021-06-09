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

# Repeat the data import procedure for every stock
stock_name = c("GME","AAPL","AMC","BB","CLNE","CLOV","OCGN","TLRY","TSLA")
var_name = tolower(stock_name)
keyword_list= c("GME|game stop|gme|Game Stop",
                "AAPL|apple|aapl|Apple",
                "AMC|amc",
                "BB|bb|BlackBerry|blackberry|Black Berry|black berry",
                "CLNE|clne|Clean Energy|clean energy",
                "CLOV|clov|Clover|clover",
                "OCGN|cogn|Ocugen|ocugen",
                "TLRY|tlry|Tilray|tilray",
                "TSLA|tsla|Tesla|tesla")

i = 0
sentiment_data_name = c()
for (var in var_name){
  i = i+1
  sentiment_data_name[i] = paste0(var,"_data")
}

for (i in 1:length(var_name)){
  df = data %>%
    select(text, score, num_comments, timestamp, sentences, scores, scores_avg,texlen) %>%  
    filter(str_detect(text, keyword_list[i]))
  assign(sentiment_data_name[i],df)
  
  file_path = paste0("C:/Labs/Machine Learning/Final Paper/Data/stocks/",
                     stock_name[i],".csv")
  df2 = read.csv(file_path)
  df2 = df2[,-(6:7)]
  df2 = df2[,-(2:4)]
  df2['Label'] = stock_name[i]
  assign(var_name[i], df2)
}
rm(df,df2)  

# A separate procedure for BTC
# BTC sentiment data
btc_data = data %>%
  select(text, score, num_comments, timestamp, sentences, scores, scores_avg,texlen) %>%
  filter(str_detect(text, "BTC|btc|Bitcoin|bitcoin|Bit coin|bit coin"))

# BTC closing values
btc = read.csv("C:/Labs/Machine Learning/Final Paper/Data/stocks/btc.csv")
btc = btc[,-7]
btc = btc[,-(2:5)]
btc['Label'] = "BTC"
names(btc)[names(btc)=="date"] <- "Date" 
names(btc)[names(btc)=="close"] <- "Close" # rename the column to align with other dataset

# add BTC into name list
stock_name = c(stock_name,"BTC")
var_name = tolower(stock_name)
sentiment_data_name = c(sentiment_data_name, "btc_data")


# Data Cleaning -----------------------------------------------------------,

data_clean <- function(stock_data,sentiment_data){
  # simple return: percentage change
  df = stock_data %>%
    mutate(pct_change = (Close/lag(Close) - 1) * 100)
  # direction of price change
  df['direction'] = sign(df['pct_change'])
  # adpt type of date in both data set
  df$Date = as.Date(df$Date)
  sentiment_data$timestamp = as.Date(sentiment_data$timestamp)
  # Rank Sentiment Data
  df2 = sentiment_data[,c(3,4,7,8)]  # drop columns with texts
  df2 = arrange(df2,timestamp, -num_comments) # set order for data: increase with date, decrease with number of comments
  # Keep the data with top 10 largest number of comments
  df2 <- df2 %>% 
    group_by(timestamp) %>% 
    mutate(rank = row_number()) %>% 
    select(timestamp,rank, scores_avg) %>% 
    filter(rank <= 10)
  # create variable names for next step
  df2['variable'] = paste0('Top', as.character(df2$rank))
  # transform the long panel data in to wide panel data 
  wide <- select(df2,timestamp,variable, scores_avg) %>%
    dcast(timestamp ~ variable, value.var = "scores_avg")  %>%
    select(one_of(c("timestamp","Top1","Top2","Top3","Top4","Top5",
                    "Top6","Top7","Top8","Top9","Top10")))
  # join the sentiment data with stock data, order by date
  ml_join_data <- left_join(df, wide,by = c("Date" = "timestamp")) %>%
    arrange(Date)
  # drop the observations with empty value
  final_data <- na.omit(ml_join_data)
  return(final_data) 
}

i = 0
final_data_name = c()
for (var in var_name){
  i = i+1
  final_data_name[i] = paste0(var,"_final_data")
}

for (i in 1:length(var_name)){
  assign(final_data_name[i], 
         data_clean(stock_data = get(var_name[i]),
                    sentiment_data = get(sentiment_data_name[i]))
         )
}

rm(list = c(var_name,sentiment_data_name))

combined_data = c()
for (i in 1:length(var_name)){
  combined_data = rbind(combined_data,get(final_data_name[i]))
}

write.csv(combined_data, 
          file = "C:/Labs/Machine Learning/Final Paper/Data/analysis data/combined_data.csv",quote=F,row.names = F)



####### Stock Data

# # How to filter for a stock,
# gme_data = data %>%
#   select(text, score, num_comments, timestamp, sentences, scores, scores_avg,texlen) %>%   
#   filter(str_detect(text, "GME|game stop|gme|Game Stop"))
# 
# # GME closing values
# gme = read.csv("C:/Labs/Machine Learning/Final Paper/Data/stocks/GME_jan_may_21.csv")
# gme = gme[,-(6:7)]
# gme = gme[,-(2:4)]

#######  Data Clean
# # simple return: percentage change for gme stock
# gme = gme %>%
#   mutate(pct_change = (Close/lag(Close) - 1) * 100)
# 
# # direction of price change
# gme['direction'] = sign(gme['pct_change'])
# 
# # adpt type of date in both data set
# gme$Date = as.Date(gme$Date)
# gme_data$timestamp = as.Date(gme_data$timestamp)
# 
# # Rank Sentiment Data
# df = gme_data[,c(3,4,7,8)]  # drop columns with texts
# 
# data.frame(table(df$timestamp)) # count how many posts for each day
# 
# df = arrange(df,timestamp, -num_comments) # set order for data: increase with date, decrease with number of comments
# 
# # Keep the data with top 10 largest number of comments
# df <- df %>% 
#   group_by(timestamp) %>% 
#   mutate(rank = row_number()) %>% 
#   select(timestamp,rank, scores_avg) %>% 
#   filter(rank <= 10)
# 
# # create variable names for next step
# df['variable'] = paste0('Top', as.character(df$rank))
# 
# # transform the long panel data in to wide panel data
# wide <- select(df,timestamp,variable, scores_avg) %>%
#   dcast(timestamp ~ variable, value.var = "scores_avg") %>%
#   select(timestamp,Top1,Top2,Top3,Top4,Top5,Top6,Top7,Top8,Top9,Top10)
# 
# # join the sentiment data with stock data, order by date
# ml_join_data <- left_join(gme, wide,by = c("Date" = "timestamp")) %>%
#   arrange(Date)
# 
# # drop the observations with empty value
# gme_final_data <- na.omit(ml_join_data)

# write.csv(gme_final_data, 
#           file = "C:/Labs/Machine Learning/Final Paper/Data/analysis data/gme.csv",quote=F,row.names = F)

