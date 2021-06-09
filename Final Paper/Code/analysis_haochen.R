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



# Import Data -------------------------------------------------------------

# load Data
Data = read.csv("C:/Labs/Machine Learning/Final Paper/Data/analysis data/combined_data.csv")
Data$direction[Data$direction == -1] = 0
Data$Date = as.Date(Data$Date)
Data = Data[,-(1:4)]

# Standardize Features
standFun = function(x){
  out = (x - mean(x))/sd(x)
  return(out)
}

features = names(Data)[2:11]

for (i in 1:10){
  Data[[features[i]]] = standFun(Data[[features[i]]])
}

# Logistic Regression -----------------------------------------------------

# Shuffle and Split Data (70-30)
set.seed(123)
Data = Data[sample(nrow(Data)),]
ntrain = floor(nrow(Data)*0.7)
Data.Train = Data[1:ntrain,]
Data.Test = Data[(ntrain+1):nrow(Data),]

# Logistic Regression on Training Data
glm.fit = glm(direction ~ . ,data = Data.Train,family = binomial)
summary(glm.fit)


# * In-Sample Predictions -------------------------------------------------

# Get predictions
yhat = predict(glm.fit, type = "response")
yhat = ifelse(yhat < 0.5, 0,1)

# misclassifications
result = data.frame(direction = Data.Train$direction,prediction = yhat)
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

# compute the MSE



# * In-sample with first two variables ------------------------------------

# Logistic Regression on Training Data
glm.fit = glm(direction ~ Top1+Top2 ,data = Data.Train,family = binomial)
summary(glm.fit)


# * In-Sample Predictions -------------------------------------------------

# Get predictions
yhat = predict(glm.fit, type = "response")
yhat = ifelse(yhat < 0.5, 0,1)

# misclassifications
result = data.frame(direction = Data.Train$direction,prediction = yhat)
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
result = data.frame(direction = Data.Test$direction,prediction = yhat)
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
set.seed(123)
# Run the KNN algorithm for K = 1,10,25
xvars = c("Top1","Top2"
          #,"Top3","Top4","Top5","Top6","Top7","Top8","Top9","Top10"
          )
yvars = "direction"

# out-of-sample prediction
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
result = data.frame(direction = Data.Test$direction,
                    pred_1 = knn_F_1)
result$error = ifelse(result$direction != result$pred_1,1,0)
table(result$direction,result$prediction)

# Number of misclassified cases
misclas = sum(result$error)





