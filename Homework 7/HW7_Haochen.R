# Univeristy of St.Gallen
# 8330 Mahcine Learning, 2021 Spring
# Homework 7
# Apr 29, 2021

# Initial Setting

# PLEASE ADJUST YOUR WORKING DIRECTORY
rm(list = ls())
mainDir = "C:\\Labs\\Machine Learning\\HW7\\R scripts"
setwd(mainDir)

# Exercise 1 --------------------------------------------------------------

# Prepare Packages
library(class)

# Prepare Data

load("../Data/wdbcData.RData") 




# * Question 1 ------------------------------------------------------------
# Split the data into traning and testing sets in the ratio 70%-30%

# verify correct columns
colnames(Data[5])  == "perimeter_mean"
colnames(Data[10]) == "concaveP_mean"

# Shuffle and Split Data (70-30)
set.seed(123)
Data = Data[sample(nrow(Data)),]

# Create data frame for data to be used
df = data.frame(id = Data$id,
                diagnosis = Data$diagnosis, 
                perimeter_mean = Data$perimeter_mean, 
                concaveP_mean = Data$concaveP_mean)
ntrain = floor(nrow(df)*0.7)
df.Train = df[1:ntrain,]
df.Test = df[(ntrain+1):nrow(Data),]

# * Question 2 ------------------------------------------------------------
# Run the KNN algorithm for K = 1,10,25. What type of predictions do you get,
# What do they mean?
xvars = c("perimeter_mean","concaveP_mean")
yvars = "diagnosis"

knn_T_1 = knn(train = df.Train[xvars], 
              test = df.Test[xvars],
              cl = as.matrix(df.Train[yvars]), 
              k = 1, 
              prob = TRUE )

knn_T_10 = knn(train = df.Train[xvars], 
               test = df.Test[xvars],
               cl = as.matrix(df.Train[yvars]), 
               k = 10, 
               prob = TRUE )

knn_T_25 = knn(train = df.Train[xvars], 
               test = df.Test[xvars],
               cl = as.matrix(df.Train[yvars]), 
               k = 25, 
               prob = TRUE )


# * Question 3 ------------------------------------------------------------
# Run the KNN algorithm for K = 1,10,25 and compute the (test) error rate

knn_F_1 = knn(train = df.Train[xvars], 
              test = df.Test[xvars],
              cl = as.matrix(df.Train[yvars]), 
              k = 1, 
              prob = FALSE )

knn_F_10 = knn(train = df.Train[xvars], 
               test = df.Test[xvars],
               cl = as.matrix(df.Train[yvars]), 
               k = 10, 
               prob = FALSE )

knn_F_25 = knn(train = df.Train[xvars], 
               test = df.Test[xvars],
               cl = as.matrix(df.Train[yvars]), 
               k = 25, 
               prob = FALSE )

err_F_1 = round(sum(knn_F_1 != as.matrix(df.Test[yvars]))/length(knn_F_1)* 100,2)
err_F_10 = round(sum(knn_F_10 != as.matrix(df.Test[yvars]))/length(knn_F_10)* 100,2)
err_F_25 = round(sum(knn_F_25 != as.matrix(df.Test[yvars]))/length(knn_F_25)* 100,2)


# * Question 4 ------------------------------------------------------------

# Run logistic regression on the same data, and predict the results.
# Compute the error rate.
glm.fit = glm(diagnosis~perimeter_mean + concaveP_mean,data = df.Train,family = binomial)
summary(glm.fit)
yhat = predict(glm.fit, type = "response",newdata = df.Test)
yhat = ifelse(yhat<0.5,"B","M")

err_logit = round(sum(yhat != as.matrix(df.Test[yvars]))/length(yhat) * 100,2)


# * Question 5 ------------------------------------------------------------
errors = data.frame(KNN1 = err_F_1,
                    KNN2 = err_F_10,
                    KNN3 = err_F_25,
                    logistic = err_logit)






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
