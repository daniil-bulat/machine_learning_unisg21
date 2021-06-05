# Machine Learning Final Project
# Reddit and WallStreetBets Sentiment Analysis / Machine Learning
library(dplyr)
library(quanteda)
library(readtext)
library(stringr)
library(tidyverse)
library(reshape2)
library(class)

library(tree)
library(randomForest)
library(glmnet)

setwd("~/UNIVERSITA/_St.GALLEN - Master in Quant Economics and Finance/Subjects/2^ Semester/Machine Learning/project")



# Data Import -------------------------------------------------------------


####### Sentiment Data
data = read.csv("reddit_sentiment.csv")
data = data[,2:8]
data$texlen = nchar(data$text)

####### Stock Data

# How to filter for a stock
gme_data = data %>%
  select(text, score, num_comments, timestamp, sentences, scores, scores_avg,texlen) %>%   
  filter(str_detect(text, "GME|game stop|gme|Game Stop"))

# GME closing values
gme = read.csv("GME_jan_may_21.csv")
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



#-------------------------------------------------------------------------------
##### Decision Tree

# load Data
Data = gme_final_data
Data$direction[Data$direction == -1] = 0

# Shuffle and Split Data (70-30)
set.seed(123)
Data = Data[sample(nrow(Data)),]
ntrain = floor(nrow(Data)*0.7)
Data.Train = Data[1:ntrain,]
Data.Test = Data[(ntrain+1):nrow(Data),]

# Decision Tree on Training Data
dec_tree = tree(direction ~ . - Date - Close - pct_change,data = Data.Train)
summary(dec_tree)
# plotting
plot(dec_tree)
text(dec_tree)

# * In-Sample Predictions -------------

# Get predictions
pred_train = predict(dec_tree)
pred_train = ifelse(pred_train < 0.5, 0,1)

# misclassifications
result_train = data.frame(Date = Data.Train$Date,direction = Data.Train$direction,prediction = pred_train)
result_train$error = ifelse(result_train$direction != result_train$prediction,1,0)

# Number of misclassified cases
misclas_train = sum(result_train$error)

# False positives 
x_train = ifelse(result_train$prediction == 1 & result_train$direction == 0, 1, 0)
falPos_train = sum(x_train)

# False negatives 
x_2_train = ifelse(result_train$prediction == 0 & result_train$direction == 1, 1, 0)
falNeg = sum(x_2_train)

# compute the accuracy rate
mean(result_train$direction == result_train$prediction)


# * Out-of-Sample Predictions ----------

# Get predictions
pred_test = predict(dec_tree, newdata = Data.Test)
pred_test = ifelse(pred_test < 0.5, 0,1)

# misclassifications
result_test = data.frame(Date = Data.Test$Date,direction = Data.Test$direction,prediction = pred_test)
result_test$error = ifelse(result_test$direction != result_test$prediction,1,0)

# Number of misclassified cases
misclas_test = sum(result_test$error)

# False positives 
x_test = ifelse(result_test$prediction == 1 & result_test$direction == 0, 1, 0)
falPos_test = sum(x_test)

# False negatives 
x_2_test = ifelse(result_test$prediction == 0 & result_test$direction == 1, 1, 0)
falNeg_test = sum(x_2_test)

# compute the accuracy rate
mean(result_test$direction == result_test$prediction)




#-------------------------------------------------------------------------------
##### Random Forest

# Random Forest on Training Data
ran_forest = randomForest(direction ~ . - Date - Close - pct_change,data = Data.Train)
summary(ran_forest)
# plotting
plot(ran_forest)

# * In-Sample Predictions -----------

# Get predictions
pred_train_rf = predict(ran_forest)
pred_train_rf = ifelse(pred_train_rf < 0.5, 0,1)

# misclassifications
result_train_rf = data.frame(Date = Data.Train$Date,direction = Data.Train$direction,prediction = pred_train_rf)
result_train_rf$error = ifelse(result_train_rf$direction != result_train_rf$prediction,1,0)

# Number of misclassified cases
misclas_train_rf = sum(result_train_rf$error)

# False positives 
x_train_rf = ifelse(result_train_rf$prediction == 1 & result_train_rf$direction == 0, 1, 0)
falPos_train_rf = sum(x_train_rf)

# False negatives 
x_2_train_rf = ifelse(result_train_rf$prediction == 0 & result_train_rf$direction == 1, 1, 0)
falNeg_rf = sum(x_2_train_rf)

# compute the accuracy rate
mean(result_train_rf$direction == result_train_rf$prediction)


# * Out-of-Sample Predictions ---------

# Get predictions
pred_test_rf = predict(ran_forest, newdata = Data.Test)
pred_test_rf = ifelse(pred_test_rf < 0.5, 0,1)

# misclassifications
result_test_rf = data.frame(Date = Data.Test$Date,direction = Data.Test$direction,prediction = pred_test_rf)
result_test_rf$error = ifelse(result_test_rf$direction != result_test_rf$prediction,1,0)

# Number of misclassified cases
misclas_test_rf = sum(result_test_rf$error)

# False positives 
x_test_rf = ifelse(result_test_rf$prediction == 1 & result_test_rf$direction == 0, 1, 0)
falPos_test_rf = sum(x_test_rf)

# False negatives 
x_2_test_rf = ifelse(result_test_rf$prediction == 0 & result_test_rf$direction == 1, 1, 0)
falNeg_test_rf = sum(x_2_test_rf)

# compute the accuracy rate
mean(result_test_rf$direction == result_test_rf$prediction)



