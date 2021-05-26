# Reddit Wall Street Bets Sentiment Analysis / Machine Learining

data = read.csv("reddit_wsb.csv")
colnames(data) = c("title", "score", "id", "url", "comms_num", "created", "body", "timestamp")
data = data[,-6]

data_1 = read.csv("reddit_wsb1.csv")
colnames(data_1) = c("id", "title", "body", "score", "timestamp", "comms_num", "url")
data_1 = data_1[,c(2,4,1,7,6,3,5)]

data_c = rbind(data_1,data)
data_c$timestamp = as.POSIXct(strptime(data_c$timestamp, "%Y-%m-%d"))

# GME closing values
gme = read.csv("GME_jan_may_21.csv")
gme = gme[,-(6:7)]
gme = gme[,-(2:4)]



# pct change
library(tidyverse)
gme = gme %>%
  mutate(pct_change = (Close/lag(Close) - 1) * 100)

