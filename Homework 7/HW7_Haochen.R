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


# Shuffle and Split Data (70-30)
set.seed(123)
Data = Data[sample(nrow(Data)),]

# Create data frame for data to be used
df = data.frame(id = Data$id,
                diagnosis = Data$diagnosis,  Data[, c(5, 10)]
                
                #perimeter_mean = Data$perimeter_mean, 
                #concaveP_mean = Data$concaveP_mean
                )
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

K = c(1,10,25)
errors = c(err_F_1,err_F_10,err_F_25)

for (i in 1:3){
  cat("The error rate of K-NN algorithm when K = ", K[i], "is", errors[i], "\n")
}

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






# Exercise 3 --------------------------------------------------------------

# * Question 1 ------------------------------------------------------------
# Write your own 2-NN regression algorithm.

# Prepare for data
L.olympics=t(data.frame(c(1896,12),c(1900,11),c(1904,11),c(1906,11.2),c(1908,10.8),
                        c(1912,10.8),c(1920,10.8),c(1924,10.6),c(1928,10.8),c(1932,10.3),
                        c(1936,10.3),c(1948,10.3),c(1952,10.4),c(1956,10.5),c(1960,10.2),
                        c(1964,10),c(1968,9.95),c(1972,10.14),c(1980,10.25),c(1984,9.99),
                        c(1988,9.92),c(1992, 9.96),c(1996, 9.84),c(2000, 9.87),c(2004,9.85),
                        c(2008,9.69),c(2012, 9.61),c(2016, 9.83)))

colnames(L.olympics)=c("year","time")
rownames(L.olympics)=1:nrow(L.olympics)
data = L.olympics

# Write the function
my_2NN = function(x.train,x.test,y){
  # initializing predictions
  predictions = c()
  # Loops to find position of test data and compute the predictions
  for (i in c(1:length(x.test))){
    for (j in c(1:length(x.train))){
      # condition set to find the position of testing data
      if ((x.test[i]>x.train[j]) & (x.test[i]<x.train[j+1])){
        
        # prediction is the average of neighbor years
        # There are two cases:
        # 1. two outcomes are identical (majority vote)
        # 2. two outcomes are different (with same probability = 1/2)
        pred = sum(y[j],y[j+1])/2
        
        # store the results
        predictions[i] = pred
      }
    }
    
  }
  
  result = data.frame(year = x.test, predictions = predictions)

  return(result)
}

# * Question 2 ------------------------------------------------------------

# Define training and testing dataset
train <- L.olympics[-c(4, 10, 17, 24), ]
test  <- L.olympics[c(4, 10, 17, 24), ]

# subtract features and outcomes
x.train = train[1:nrow(train),1]
x.test = test[1:nrow(test),1]
y = train[1:nrow(train),2]

# predict use own function
pred_2NN = my_2NN(x.train,x.test,y)

# compute the test-RSS
errors = test[1:nrow(test),2] - pred_2NN[1:nrow(test),2]
RSS = sum(errors**2)

# result

# predictions for testing data
print(pred_2NN)

# Test RSS
print(RSS)









