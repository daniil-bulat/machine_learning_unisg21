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

# Simpre return: percentage change for gme stock
gme = gme %>%
  mutate(pct_change = (Close/lag(Close) - 1) * 100)

gme['direction'] = sign(gme['pct_change'])

gme$Date = as.Date(gme$Date)

# Rank Sentiment Data

data$timestamp = as.Date(data$timestamp)
class(data$timestamp)

df = data[,c(3,4,7,8)]

data.frame(table(df$timestamp))

df = arrange(df,timestamp, -num_comments)

df <- df %>% 
  group_by(timestamp) %>% 
  mutate(rank = row_number())

df1 <- df %>% 
  select(timestamp,rank, scores_avg) %>% 
  filter(rank <= 20)

df1['variable'] = paste0('Top', as.character(df1$rank))

df2 <- select(df1,timestamp,variable, scores_avg) %>%
  dcast(timestamp ~ variable, value.var = "scores_avg") %>%
  select(timestamp,Top1,Top2,Top3,Top4,Top5,Top6,Top7,Top8,Top9,Top10,
         Top11,Top12,Top13,Top14,Top15,Top16,Top17,Top18,Top19,Top20)


ml_join_data <- left_join(gme, df2,by = c("Date" = "timestamp")) %>%
  arrange(Date)

head(ml_join_data)

gme_final_data <- na.omit(ml_join_data)

head(gme_final_data)



# Logistic Regression -----------------------------------------------------

# load Data
Data = gme_final_data
Data$direction[Data$direction == -1] = 0

# Shuffle and Split Data (70-30)
set.seed(123)
Data = Data[sample(nrow(Data)),]
ntrain = floor(nrow(Data)*0.7)
Data.Train = Data[1:ntrain,]
Data.Test = Data[(ntrain+1):nrow(Data),]

# Logistic Regression on Training Data
glm.fit = glm(direction ~ . - Date - Close - pct_change,data = Data.Train,family = binomial)
summary(glm.fit)


# * In-Sample Predictions -------------------------------------------------

# Get predictions
yhat = predict(glm.fit, type = "response")
yhat = ifelse(yhat < 0.5, 0,1)

# misclassifications
result = data.frame(Date = Data.Train$Date,direction = Data.Train$direction,prediction = yhat)
result$error = ifelse(result$direction != result$prediction,1,0)

# Number of misclassified cases
misclas = sum(result$error)

# False positives 
x = ifelse(result$prediction == 1 & result$direction == 0, 1, 0)
falPos = sum(x)

# False negatives 
x = ifelse(result$prediction == 0 & result$direction == 1, 1, 0)
falNeg = sum(x)

# compute the accuracy rate
mean(result$direction == result$prediction)


# * Out-of-Sample Predictions ---------------------------------------------

# Get predictions
yhat = predict(glm.fit, type = "response", newdata = Data.Test)
yhat = ifelse(yhat < 0.5, 0,1)

# misclassifications
result = data.frame(Date = Data.Test$Date,direction = Data.Test$direction,prediction = yhat)
result$error = ifelse(result$direction != result$prediction,1,0)
table(result$direction,result$prediction)

# Number of misclassified cases
misclas = sum(result$error)

# False positives 
x = ifelse(result$prediction == 1 & result$direction == 0, 1, 0)
falPos = sum(x)

# False negatives 
x = ifelse(result$prediction == 0 & result$direction == 1, 1, 0)
falNeg = sum(x)

# compute the accuracy rate
mean(result$direction == result$prediction)



# KNN ---------------------------------------------------------------------

# Run the KNN algorithm for K = 1,10,25
xvars = c("Top1","Top2","Top3","Top4","Top5","Top6","Top7","Top8","Top9","Top10",
          "Top11","Top12","Top13","Top14","Top15","Top16","Top17","Top18","Top19","Top20")
yvars = "direction"

knn_F_1 = knn(train = Data.Train[xvars], 
              test = Data.Test[xvars],
              cl = as.matrix(Data.Train[yvars]), 
              k = 1, 
              prob = FALSE )

knn_F_10 = knn(train = Data.Train[xvars], 
               test = Data.Test[xvars],
               cl = as.matrix(Data.Train[yvars]), 
               k = 10, 
               prob = FALSE )

knn_F_25 = knn(train = Data.Train[xvars], 
               test = Data.Test[xvars],
               cl = as.matrix(Data.Train[yvars]), 
               k = 25, 
               prob = FALSE )

err_F_1 = round(sum(knn_F_1 != as.matrix(Data.Test[yvars]))/length(knn_F_1),2)
err_F_10 = round(sum(knn_F_10 != as.matrix(Data.Test[yvars]))/length(knn_F_10),2)
err_F_25 = round(sum(knn_F_25 != as.matrix(Data.Test[yvars]))/length(knn_F_25),2)

K = c(1,10,25)
errors = c(err_F_1,err_F_10,err_F_25)

# misclassifications
result = data.frame(Date = Data.Test$Date,direction = Data.Test$direction,
                    pred_1 = knn_F_1)
result$error = ifelse(result$direction != result$prediction,1,0)
table(result$direction,result$prediction)

# Number of misclassified cases
misclas = sum(result$error)





