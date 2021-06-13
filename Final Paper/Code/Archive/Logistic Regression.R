# Machine Learning Final Project
# Reddit and WallStreetBets Sentiment Analysis / Machine Learning
library(dplyr)
library(quanteda)
library(readtext)
library(stringr)
library(tidyverse)
library(reshape2)
library(class)
library(ggplot2)

# Logistic Regression -----------------------------------------------------

# set the workspace
rm(list = ls())
mainDir = "C:/Labs/Machine Learning/Final Paper/Code"
setwd(mainDir)

# The data (Make sure they are in the proper working directory)

Data = read.csv("C:/Labs/Machine Learning/Final Paper/Data/analysis data/combined_data_5_features.csv")

Data$Date = as.Date(Data$Date)
Data = Data[,-(1:4)]



# Standardize Features
standFun = function(x){
  out = (x - mean(x))/sd(x)
  return(out)
}

features = names(Data)[2:6]

# Keep only the columns in the data frame that we need for the learning task
keepList = c(features, "direction")

Data = Data[keepList]

for (i in 1:length(features)){
  Data[[features[i]]] = standFun(Data[[features[i]]])
}

# Adjust dependent variable to suit for logistic regression
Data$direction[Data$direction == -1] = 0

# Split the data into training and test
# Shuffle and Split Data (70-30)
set.seed(123)
Data = Data[sample(nrow(Data)),]
ntrain = floor(nrow(Data)*0.7)
Data.Train = Data[1:ntrain,]
Data.Test = Data[(ntrain+1):nrow(Data),]

# Logistic Regression on Training Data
# glm.fit = glm(direction ~ . ,data = Data.Train,family = binomial)
# summary(glm.fit)

# consider another case that only use the first two features
glm.fit = glm(direction ~ Top1 + Top2 ,data = Data.Train,family = binomial)
summary(glm.fit)

round(glm.fit$coefficients,3)

# * In-Sample Predictions -------------------------------------------------

# Get predictions and errors


yhat = predict(glm.fit, type = "response")
Data.Train$lastIndex  = yhat
Data.Train$direction[Data.Train$direction == 0] = -1
Data.Train$lastPrediction = ifelse(Data.Train$lastIndex >=0.5, 1, -1)
Data.Train$lastError  = Data.Train$direction - Data.Train$lastPrediction

# Number of misclassified cases (in % of total sample)
misclasTrain = round(sum(Data.Train$lastError!=0)/nrow(Data.Train)*100, digits = 1)

# False positives (in % of negatives)
x = ifelse(Data.Train$lastPrediction == 1 & Data.Train$direction == -1, 1, 0)
falPosTrain = round(sum(x)/length(Data.Train$direction)*100, digits = 1)

# False negatives (in % of positives)
x = ifelse(Data.Train$lastPrediction == -1 & Data.Train$direction == 1, 1, 0)
falNegTrain = round(sum(x)/length(Data.Train$direction)*100, digits = 1)

# compute the accuracy rate
acTrain = round(mean(Data.Train$direction == Data.Train$lastPrediction)*100,2)

# MSE
MSETrain = round(mean(Data.Train$lastError^2),2)

# * Out-of-Sample Predictions ---------------------------------------------

# Get predictions and errors
yhat = predict(glm.fit, type = "response", newdata = Data.Test)
Data.Test$lastIndex  = yhat
Data.Test$direction[Data.Test$direction == 0] = -1
Data.Test$lastPrediction = ifelse(Data.Test$lastIndex >=0.5, 1, -1)
Data.Test$lastError  = Data.Test$direction - Data.Test$lastPrediction

# Number of misclassified cases (in % of total sample)
misclasTest = round(sum(Data.Test$lastError!=0)/nrow(Data.Test)*100, digits = 1)

# False positives (in % of negatives)
x = ifelse(Data.Test$lastPrediction == 1 & Data.Test$direction == -1, 1, 0)
falPosTest = round(sum(x)/length(Data.Test$direction)*100, digits = 1)

# False negatives (in % of positives)
x = ifelse(Data.Test$lastPrediction == -1 & Data.Test$direction == 1, 1, 0)
falNegTest = round(sum(x)/length(Data.Test$direction)*100, digits = 1)

# compute the accuracy rate
acTest = round(mean(Data.Test$direction == Data.Test$lastPrediction)*100,2)

# MSE
MSETest = round(mean(Data.Test$lastError^2),2)