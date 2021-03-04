# Univeristy of St.Gallen
# 8330 Mahcine Learning, 2021 Spring
# Homework 1
# Mar 3, 2021

# Set up
rm(list = ls())
set.seed(123)
#setwd()


# Question 1 --------------------------------------------------------------

# Determine the Determine the linear regression function in the form 
# f(x1; x2) = m1x1 + m2x2 + c for predicting Sepal.Length depending on 
# x1 =Petal.Length and x2 =Petal.Width on the training data.

# Shuffle the Iris data
Data = iris[sample(1:150),]

# and split into training and test data (80-20)
Data.Train = Data[1:120,]
Data.Test = Data[121:150,]

# Regression on the training data - Model 1: f(x1; x2) = m1x1 + m2x2 + c
Iris.Model1 = lm(Sepal.Length ~ Petal.Length + Petal.Width, data = Data.Train)

summary(Iris.Model1)


# Question 2 --------------------------------------------------------------

# Do the same for only one attribute, x1 =Petal.Length on the training data.

# Regression on the training data - Model 2: f(x1) = m1x1 + c
Iris.Model2 = lm(Sepal.Length ~ Petal.Length, data = Data.Train)

summary(Iris.Model2)



# Question 3 --------------------------------------------------------------

# Do the same for the three attributes, x1 =Petal.Length, x2 =Petal.Width, 
# x3 =Sepal.Width on the training data.

# Regression on the training data - Model 3: f(x1; x2; x3) = m1x1 + m2x2 + m3x3 + c
Iris.Model3 = lm(Sepal.Length ~ Petal.Length + Petal.Width + Sepal.Width, data = Data.Train)

summary(Iris.Model3)



# Question 4 --------------------------------------------------------------

# Find the commands for mean and variance in R and compute the mean and 
# the variance of Petal.Length and Petal.Width, respectively.

mean(Data.Train$Petal.Length)
var(Data.Train$Petal.Length)

mean(Data.Train$Petal.Width)
var(Data.Train$Petal.Width)



# Bonus Question ----------------------------------------------------------

# Use the mean and variance commands to compute (or verify) the
# regression function for part 2) step by step without the lm-command

# Apply the formula for simple linear regression
beta.hat = var(Data.Train$Sepal.Length,Data.Train$Petal.Length)/var(Data.Train$Petal.Length)
alpha.hat = mean(Data.Train$Sepal.Length) - beta.hat*mean(Data.Train$Petal.Length)

# Check the coefficients computed and compare them with part 2
alpha.hat
beta.hat
Iris.Model2$coefficients
