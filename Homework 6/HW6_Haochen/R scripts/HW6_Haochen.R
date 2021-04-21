# Univeristy of St.Gallen
# 8330 Mahcine Learning, 2021 Spring
# Homework 6
# Apr 21, 2021

# Initial Setting

# PLEASE ADJUST YOUR WORKING DIRECTORY
rm(list = ls())
mainDir = "C:\\Labs\\Machine Learning\\HW6\\R scripts"
setwd(mainDir)

# Exercise 1 --------------------------------------------------------------


# Prepare Data

load("../Data/wdbcData.RData") 

# Shuffle and Split Data (70-30)
set.seed(123)
Data = Data[sample(nrow(Data)),]
ntrain = floor(nrow(Data)*0.7)
Data.Train = Data[1:ntrain,]
Data.Test = Data[(ntrain+1):nrow(Data),]


# * Question 1 ------------------------------------------------------------

# Run logistic regression on the training data with respect to perimeter_mean and concaveP_mean

glm.fit = glm(diagnosis~perimeter_mean + concaveP_mean,data = Data.Train,family = binomial)
summary(glm.fit)

# * Question 2 ------------------------------------------------------------

# Get a list of predictions. How do you interpret these as probabilities.
yhat = predict(glm.fit, type = "response")
contrasts(Data.Train$diagnosis)

# * Question 3 ------------------------------------------------------------

# Predict the actual class label (B or M)
yhat = ifelse(yhat<0.5,"B","M")

# * Question 4 ------------------------------------------------------------

# Compute the number of misclassifications, as well as the absolute number of false positives and false negatives.
result = data.frame(id = Data.Train$id,diagnosis = Data.Train$diagnosis,prediction = yhat)
result$error = ifelse(result$diagnosis != result$prediction,1,0)

# Number of misclassified cases
misclas = sum(result$error)

# False positives 
x = ifelse(result$prediction == "M" & result$diagnosis == "B", 1, 0)
falPos = sum(x)

# False negatives 
x = ifelse(result$prediction == "B" & result$diagnosis == "M", 1, 0)
falNeg = sum(x)





# Exercise 2 --------------------------------------------------------------


# * Question 1 ------------------------------------------------------------

# Briefly summarize what we are predicting in dependance on which data in this example.

# We are using data from ISLR called Smarket, which consistent information of percentage returns for the S&P 500 stock index over 1250 days from the beginning of 2001 until the end of 2005.
# What we are predicting is the direction of the market (percentage returns of the S&P 500  index) changes on the specified date.


# * Question 2 ------------------------------------------------------------

# What is the outcome, with 2 or 6 features, respectively?
# How does our prediction compare to predicting that the market goes up all the time.

# With 6 features, the logistic regression correctly predicted the movement of the market 48% of the time.

# With 2 features, the logistic regression correctly predicted the movement of the market 56% of the time.

# In 2005, the market goes up in 141 days of 252 trading days, around 56% of the time. If we predicting the market goes up all the time, we would predict correctly 56% of the time. The logistic regression predicts the increase of the market 31% of the time in 2005 when the model uses 6 features, and 72% of the time when the model uses 2 features, respectively. So the model with 6 features performs worse than predicting the market goes up all the time, whereas the model with 2 features performs better than the other two.


# * Question 3 ------------------------------------------------------------

# Run your own logistic regression with only Lag1 as input variable.

# prepare data
library(ISLR)
attach(Smarket)

# split data into train (before 2004) and test (2005) data
train = (Year < 2005)
Smarket.2005 = Smarket[!train,]
Direction.2005 = Direction[!train]

# Logistic model with only Lag1
glm.fits = glm(Direction ~ Lag1, data = Smarket, family = binomial,subset = train)

# predict for the test data
glm.probs = predict(glm.fits,Smarket.2005,type = "response")

# classify the probabilities to binary label
glm.pred = rep("Down",252)
glm.pred[glm.probs > .5] = "Up"

# Show the result
table(glm.pred,Direction.2005)

# compute the accuracy rate
mean(glm.pred == Direction.2005)

# accuracy rate when predicting an increase
116/(116+91)

# accuracy rate when predicting a decrease
20/(20+25)
