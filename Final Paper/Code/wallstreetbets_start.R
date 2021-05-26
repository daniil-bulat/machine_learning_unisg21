# Machine Learning Final Project
# Reddit — WallStreetBets Sentiment Analysis / Machine Learning

setwd("~/Desktop/Uni/FS21/Machine Learning/machine_learning_unisg21/Final Paper")

# Data from Kaggle
data = read.csv("Data/reddit_wsb.csv")
colnames(data) = c("title", "score", "id", "url", "comms_num", "created", "body", "timestamp")
data = data[,-6]

# Data prior to 28.02.2121 - Reddit API
data_1 = read.csv("Data/reddit_wsb1.csv")
colnames(data_1) = c("id", "title", "body", "score", "timestamp", "comms_num", "url")
data_1 = data_1[,c(2,4,1,7,6,3,5)]

# Complete Data
data_c = rbind(data_1,data)
data_c$timestamp = as.POSIXct(strptime(data_c$timestamp, "%Y-%m-%d"))
write.csv(data_c, file="Data/wsb_reddit_data.csv")

# GME closing values
gme = read.csv("Data/GME_jan_may_21.csv")
gme = gme[,-(6:7)]
gme = gme[,-(2:4)]


# percentage change for gme stock
library(tidyverse)
gme = gme %>%
  mutate(pct_change = (Close/lag(Close) - 1) * 100)



