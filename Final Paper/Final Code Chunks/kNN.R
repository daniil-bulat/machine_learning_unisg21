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

# KNN ---------------------------------------------------------------------

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

# Split the data into training and test
# Shuffle and Split Data (70-30)
set.seed(123)
Data = Data[sample(nrow(Data)),]
ntrain = floor(nrow(Data)*0.7)
Data.Train = Data[1:ntrain,]
Data.Test = Data[(ntrain+1):nrow(Data),]

# Run the KNN algorithm 
xvars = c("Top1","Top2" 
          ,"Top3","Top4","Top5"
)
yvars = "direction"


# * In-sample Prediction --------------------------------------------------


# set up a holding
acc = numeric()

for (i in 1:50){
  set.seed(123)
  predict = knn(train = Data.Train[xvars], 
                test = Data.Train[xvars],
                cl = as.matrix(Data.Train[yvars]), 
                k = i, 
                prob = FALSE
  )
  acc = c(acc,mean(predict == Data.Train$direction))
}

k = 1:50
acc = as.data.frame(acc,k)
ggplot(data = acc, aes(x = k, y = 1 - acc)) + 
  geom_line(col = "red") +
  xlab("K") +
  ylab("error rate") +
  theme_bw()

which.max(acc$acc)

# out-of-sample prediction
set.seed(123)

yhat = knn(train = Data.Train[xvars], 
           test = Data.Train[xvars],
           cl = as.matrix(Data.Train[yvars]), 
           k = 1, 
           prob = FALSE )

Data.Train$lastIndex = yhat
Data.Train$lastPrediction = ifelse(Data.Train$lastIndex == "1", 1, -1)
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

acTrain
MSETrain
misclasTrain
falPosTrain
falNegTrain



# *Out-of-sample prediction -----------------------------------------------


# set up a holding
acc = numeric()

for (i in 1:50){
  set.seed(123)
  predict = knn(train = Data.Train[xvars], 
                test = Data.Test[xvars],
                cl = as.matrix(Data.Train[yvars]), 
                k = i, 
                prob = FALSE
  )
  acc = c(acc,mean(predict == Data.Test$direction))
}

k = 1:50
acc = as.data.frame(k = 1:50, acc)
ggplot(data = acc, aes(x = k, y = 1 - acc)) + 
  geom_line(col = "red") +
  xlab("K") +
  ylab("error rate") +
  theme_bw()

which.max(acc$acc)

# out-of-sample prediction
set.seed(123)

yhat = knn(train = Data.Train[xvars], 
           test = Data.Test[xvars],
           cl = as.matrix(Data.Train[yvars]), 
           k = 14, 
           prob = FALSE )

Data.Test$lastIndex = yhat
Data.Test$lastPrediction = ifelse(Data.Test$lastIndex == "1", 1, -1)
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