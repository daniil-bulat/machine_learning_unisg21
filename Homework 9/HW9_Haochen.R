# Univeristy of St.Gallen
# 8330 Mahcine Learning, 2021 Spring
# Homework 8
# May 5, 2021

# Set up
rm(list = ls())
set.seed(123)

mainDir = "C:/Labs/Machine Learning/HW9"
setwd(mainDir)
getwd()


# Exercise 1 --------------------------------------------------------------

# Install packages required
library(MASS)

# Data Preparation
Data = Boston
set.seed(123)
Data = Data[sample(nrow(Data)),]
ntrain = floor(nrow(Data)*0.7)
Data.Train = Data[1:ntrain,]
Data.Test = Data[(ntrain+1):nrow(Data),]


# * Question 1 ------------------------------------------------------------

# Load the tree package.
library(tree)

# Use the tree command with the prespecifed stopping criteria for the complete data.
tree.Train = tree(medv~., data = Data.Train)
tree.Train 
summary(tree.Train )

# Plot it
plot(tree.Train )
text(tree.Train )

# * Question 2 ------------------------------------------------------------

# Number of terminal nodes: 8
# Input variables: "lstat" "rm" "dis" "tax"  

# * Question 3 ------------------------------------------------------------

# use the predict command to get predictions for the test set.
yhat1 = predict(tree.Train, newdata = Data.Test)

# Compute the RSS for the test set.
RSS1 = (Data.Test$medv - yhat1)%*%(Data.Test$medv - yhat1)
RSS1

# * Question 4 ------------------------------------------------------------

# Prune the tree to get the best pruned subtree with five terminal nodes.
p.tree.Train = prune.tree(tree.Train, best = 5)

# Plot it.
plot(p.tree.Train)
text(p.tree.Train)

# used predictors: "lstat" "rm" "dis" 
summary(p.tree.Train)

# * Question 5 ------------------------------------------------------------

# use the predict command with the  smaller tree to get predictions for the 
# test set.
yhat2 = predict(p.tree.Train, newdata = Data.Test)

# Compute the RSS for the test set.
RSS2 = (Data.Test$medv - yhat2)%*%(Data.Test$medv - yhat2)
RSS2

# * Question 6 ------------------------------------------------------------

# Load the randomForest package.
library(randomForest)

# run the command randomForest(medv ., data = Data.Train)
forest.Train = randomForest(medv ~  ., data = Data.Train)
forest.Train

# number of trees: 500
# number of variables chosen at random for each split: 4
# MSE: 11.37757

# run the command again
randomForest(medv ~  ., data = Data.Train)

# Why the result is slightly different: Randomness

# * Question 7 ------------------------------------------------------------

# use the predict command to use the random forest for predictions on the 
# test set.
yhat3 = predict(forest.Train, newdata = Data.Test)

# Compute the RSS for the test set.
RSS3 = (Data.Test$medv - yhat3)%*%(Data.Test$medv - yhat3)
RSS3

RSS1
RSS2
RSS3



