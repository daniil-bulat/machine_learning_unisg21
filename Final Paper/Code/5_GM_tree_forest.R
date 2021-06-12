# Machine Learning Final Project
# Reddit and WallStreetBets Sentiment Analysis / Machine Learning
library(dplyr)
library(quanteda)
library(readtext)
library(stringr)
library(tidyverse)
library(reshape2)
library(class)
library(rpart)
library(rpart.plot)
library(tree)
library(randomForest)
library(reprtree)
library(glmnet)
library(stargazer)

setwd("~/UNIVERSITA/_St.GALLEN - Master in Quant Economics and Finance/Subjects/2^ Semester/Machine Learning/project")



# Data Import -------------------------------------------------------------

# load Data
Data = read.csv("combined_data_5_features.csv")
Data$direction[Data$direction == -1] = 0
Data$Date = as.Date(Data$Date)
Data = Data[,-(1:4)]
# 
# Standardize Features
# standFun = function(x){
#   out = (x - mean(x))/sd(x)
#   return(out)
# }
# 
# features = names(Data)[2:11]
# 
# for (i in 1:10){
#   Data[[features[i]]] = standFun(Data[[features[i]]])
# }

# removing useless stuff
rm(features, i, standFun)

#-------------------------------------------------------------------------------
##### Decision Tree

# Shuffle and Split Data (70-30)
set.seed(123)
Data = Data[sample(nrow(Data)),]
ntrain = floor(nrow(Data)*0.7)
# Converting direction into categorical
Data$direction = as.factor(Data$direction)
Data.Train = Data[1:ntrain,]
Data.Test = Data[(ntrain+1):nrow(Data),]

# Decision Tree on Training Data
dec_tree = rpart(direction ~ ., data = Data.Train)
summary(dec_tree)
# plotting
rpart.plot(dec_tree, type = 3)
rpart.rules(dec_tree)


# * In-Sample Predictions -------------

# Get predictions
pred_train = predict(dec_tree, type = "class")

# Accuracy
accuracy = function(actual, predicted) {
  mean(actual == predicted) 
}

# RMSE
rmse = function(actual, predicted) {
  actual = as.numeric(actual)
  predicted = as.numeric(predicted)
  sqrt(mean(actual - predicted) ^ 2)
}

# Train accuracy rate
accuracy_train = accuracy(Data.Train$direction, pred_train)*100

# Train false positive and negatives 
table(predicted = pred_train, actual = Data.Train$direction)
f_pos_train = (21/183)*100
f_neg_train = (18/183)*100

# Misclassifications
mis_train = f_pos_train + f_pos_train

# MSE train
mse_train = rmse(Data.Train$direction, pred_train)

# * Out-of-Sample Predictions ----------

# Get predictions
pred_test = predict(dec_tree, newdata = Data.Test, type = "class")

# Test accuracy rate
accuracy_test = accuracy(Data.Test$direction, pred_test)*100

# Test false positive and negatives 
table(predicted = pred_test, actual = Data.Test$direction)
f_pos_test = (17/79)*100
f_neg_test = (19/79)*100

# Misclassifications
mis_test = f_neg_test + f_pos_test

# MSE test
mse_test = rmse(Data.Test$direction, pred_test)

# Performances table
Performance_train = data.frame(accuracy_train, mse_train, mis_train, f_pos_train, f_neg_train)
colnames(Performance_train) = c("Accuracy", "MSE", "Missclass", "False Pos.", "False Neg.")
Performance_test = data.frame(accuracy_test, mse_test, mis_test, f_pos_test, f_neg_test)
colnames(Performance_test) = c("Accuracy", "MSE", "Missclass","False Pos.", "False Neg.")
Performance_tree = rbind(Performance_train, Performance_test)
rownames(Performance_tree) = c("In-Sample Prediction", "Out-of-Sample Prediction")

stargazer(Performance_tree, digits = 2, summary = F)

#-------------------------------------------------------------------------------
# removing stuff
rm(Performance_test, Performance_train, accuracy_test, accuracy_train,
   f_neg_test, f_neg_train, f_pos_test, f_pos_train, mse_test, mse_train, ntrain,
   pred_test, pred_train)


#-------------------------------------------------------------------------------
##### Random Forest

# Random Forest on Training Data
ran_forest = randomForest(direction ~ ., data = Data.Train, mtry = 3, 
                          importance = TRUE, ntrees = 500)
ran_forest
importance = importance(ran_forest, type = 1)
# plotting
plot(ran_forest)
varImpPlot(ran_forest, type = 1)


# * In-Sample Predictions -----------

# Get predictions
pred_train = predict(ran_forest, type = "class")

# Train accuracy rate
accuracy_train = accuracy(Data.Train$direction, pred_train)*100

# Train false positive and negatives 
table(predicted = pred_train, actual = Data.Train$direction)
f_pos_train = (52/183)*100
f_neg_train = (43/183)*100

# Misclassifications
mis_train = f_pos_train + f_pos_train

# MSE train
mse_train = rmse(Data.Train$direction, pred_train)


# * Out-of-Sample Predictions ---------

# Get predictions
pred_test = predict(ran_forest, newdata = Data.Test, type = "class")

# Test accuracy rate
accuracy_test = accuracy(Data.Test$direction, pred_test)*100

# Test false positive and negatives 
table(predicted = pred_test, actual = Data.Test$direction)
f_pos_test = (20/79)*100
f_neg_test = (15/79)*100

# Misclassifications
mis_test = f_neg_test + f_pos_test

# MSE test
mse_test = rmse(Data.Test$direction, pred_test)


# Performance table
Performance_train = data.frame(accuracy_train, mse_train, mis_train, f_pos_train, f_neg_train)
colnames(Performance_train) = c("Accuracy", "MSE", "Missclass", "False Pos.", "False Neg.")
Performance_test = data.frame(accuracy_test, mse_test, mis_test, f_pos_test, f_neg_test)
colnames(Performance_test) = c("Accuracy", "MSE", "Missclass","False Pos.", "False Neg.")
Performance_forest = rbind(Performance_train, Performance_test)
rownames(Performance_forest) = c("In-Sample Prediction", "Out-of-Sample Prediction")

stargazer(Performance_forest, digits = 2, summary = F)

#-------------------------------------------------------------------------------
# removing stuff
rm(Performance_test, Performance_train, accuracy_test, accuracy_train,
   f_neg_test, f_neg_train, f_pos_test, f_pos_train, mse_test, mse_train,
   pred_test, pred_train, importance)

