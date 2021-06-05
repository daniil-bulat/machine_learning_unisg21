
################################################################################
# Machine Learning
################################################################################
# Project: Neural Network
################################################################################

# install packages
#install.packages("readr")
#install.packages("neuralnet")

# load libraries
library(readr)
library(neuralnet)

# set working directory
setwd("/Users/jones/Desktop")

# clean environment
rm(list = ls())


# data cleaning ################################################################

# data
url  = "https://raw.githubusercontent.com/daniil-bulat/machine_learning_unisg21/main/Final%20Paper/Data/sentiment/reddit_sentiment.csv"
data = read_csv(url)
data = data[,2:8]
data$texlen = nchar(data$text)

## stock data
# get data
gme_data = data %>%
  select(text, score, num_comments, timestamp, sentences, scores, scores_avg,texlen) %>%   
  filter(str_detect(text, "GME|game stop|gme|Game Stop"))

# GME closing values
url = "https://raw.githubusercontent.com/daniil-bulat/machine_learning_unisg21/main/Final%20Paper/Data/stocks/GME_jan_may_21.csv"
gme = read.csv(url)
gme = gme[,-(6:7)]
gme = gme[,-(2:4)]

# percentage change for gme stock
gme = gme %>%
  mutate(pct_change = (Close/lag(Close) - 1) * 100)

## machine learning data
# remove unnecesary rows
ml_data = gme_data[,-(5:6)]
ml_data = ml_data[,-(1)]

# adapt type of date
gme$Date = as.Date(as.character(gme$Date))

# join sentiment and stock data
ml_join_data = left_join(gme, ml_data,
                         by = c("Date" = "timestamp"))

ml_join_data = ml_join_data[-(1:73),-2]

# give sentiments weight
ml_join_data$weighted_sent = ml_join_data$scores_avg *(ml_join_data$score + ml_join_data$num_comments)

# filter by Day
gme_final_data = ml_join_data %>%
  group_by(Date) %>% summarise(mean(weighted_sent),mean(pct_change),mean(texlen))

# five column names
names(gme_final_data)[names(gme_final_data) == "mean(weighted_sent)"] = "Sentiment"
names(gme_final_data)[names(gme_final_data) == "mean(pct_change)"] = "Pct_Change"

# final data set
data = gme_final_data
rm(gme, gme_data, gme_final_data, ml_data, ml_join_data, url) 


# machine learning #############################################################

## neural network
# creating index variable 
data = data[, c(2:4)]

# random sampling
set.seed(123)
samplesize = 0.60 * nrow(data)
index = sample(seq_len(nrow(data)), size = samplesize)

# create training and test set
data.train = data[index, ]
data.test  = data[-index, ]

# scale data for neural network
max = apply(data , 2 , max)
min = apply(data, 2 , min)
scaled = as.data.frame(scale(data, center = min, scale = max - min))

## fit neural network 
# creating training and test set
train.NN = scaled[index , ]
test.NN  = scaled[-index , ]

# fit neural network
set.seed(123)
NN = neuralnet(Pct_Change ~ Sentiment + mean(texlen), train.NN, hidden = 3, linear.output = T)

# plot neural network
plot(NN)

## prediction using neural network
predict_test.NN = compute(NN, test.NN[, c(1, 3)])
predict_test.NN = (predict_test.NN$net.result * (max(data$Pct_Change) - min(data$Pct_Change))) + min(data$Pct_Change)

plot(data.test$Pct_Change, predict_test.NN, col='blue', pch=16, ylab = "predicted Pct_Change NN", xlab = "Pct_Change")
abline(0,1)

# calculate (Root) Mean Square Error ((R)MSE)
MSE.NN  = mean((data.test$Pct_Change - predict_test.NN)^2)
RMSE.NN = (sum((data.test$Pct_Change - predict_test.NN)^2) / nrow(data.test)) ^ 0.5

# calculate R-squared
RSS.NN <- sum((data.test$Pct_Change - predict_test.NN)^2)
TSS.NN <- sum((data.test$Pct_Change - mean(data.test$Pct_Change))^2)
RSQ.NN <- 1 - (RSS.NN/TSS.NN)
