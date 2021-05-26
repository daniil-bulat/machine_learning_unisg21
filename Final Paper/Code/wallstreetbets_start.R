# Machine Learning Final Project
# Reddit — WallStreetBets Sentiment Analysis / Machine Learning

setwd("~/Desktop/Uni/FS21/Machine Learning/machine_learning_unisg21/Final Paper")

# Data
data = read.csv("Data/wsb_reddit_data.csv")

# Data Cleaning
data$body = recode(data$body, "[removed]"="", .default = data$body)
data$body = recode(data$body, "[deleted]"="", .default = data$body)
data$body = recode(data$body, "nan"="", .default = data$body)

# mutate Text column combining title and body
data = data %>%
  mutate(Text = paste(title, body, sep = " "))

# remove unnecessary columns
data = data[,-(6)]
data = data[,-(3:4)]
data = data[,-(1)]
data = data[,c(4,1,2,3)]



# How to filter for a stock
library(stringr)
library(dplyr)

gme_data = data %>%
  select(Text, score, comms_num, timestamp) %>%   
  filter(str_detect(Text, "GME|game stop|gme|Game Stop"))



# GME closing values
gme = read.csv("Data/GME_jan_may_21.csv")
gme = gme[,-(6:7)]
gme = gme[,-(2:4)]

# percentage change for gme stock
library(tidyverse)
gme = gme %>%
  mutate(pct_change = (Close/lag(Close) - 1) * 100)


