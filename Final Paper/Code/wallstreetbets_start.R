# Machine Learning Final Project
# Reddit — WallStreetBets Sentiment Analysis / Machine Learning
library(dplyr)
library(quanteda)
library(readtext)
library(stringr)
library(tidyverse)


setwd("~/Desktop/Uni/FS21/Machine Learning/machine_learning_unisg21/Final Paper")

# Data
data = read.csv("Data/sentiment/reddit_sentiment.csv")
data = data[,2:8]
names(data) = c("text","score","num_comments","timestamp","sentences","scores","score_sum")


#######
# Data Cleaning
#data$body = recode(data$body, "[removed]"="", .default = data$body)
#data$body = recode(data$body, "[deleted]"="", .default = data$body)
#data$body = recode(data$body, "nan"="", .default = data$body)
#
## mutate Text column combining title and body
#data = data %>%
#  mutate(Text = paste(title, body, sep = " "))
#
## remove unnecessary columns
#data = data[,-(7)]
#data = data[,-(4:5)]
#data = data[,-(1:2)]
#data = data[,c(4,1,2,3)]
#
#write.csv(data, file="Data/wsb_reddit_complete_clean.csv")
#######

# How to filter for a stock
gme_data = data %>%
  select(text, score, num_comments, timestamp, sentences,scores,score_sum) %>%   
  filter(str_detect(text, "GME|game stop|gme|Game Stop"))



# GME closing values
gme = read.csv("Data/stocks/GME_jan_may_21.csv")
gme = gme[,-(6:7)]
gme = gme[,-(2:4)]

# percentage change for gme stock
gme = gme %>%
  mutate(pct_change = (Close/lag(Close) - 1) * 100)


