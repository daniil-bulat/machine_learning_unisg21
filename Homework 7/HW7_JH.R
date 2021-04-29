###############################################################################
## Filename: Machine Learning
## Topic: Homework 7
## Author: Jonas Huwyler...
## Matriculation Number: 16-610-958...
###############################################################################

# Exercise 1

# removing all variables/functions
rm(list = ls())

# install packages
install.packages('class')

# load library
library(class) 

# set seed
set.seed(123)

# call paths
PATH <- "/Users/jones/Desktop/FS21/Machine Learning/Homeworks/HW7/"
FILE <- "wdbcData.RData"
setwd(dir = PATH)

#import data
load(paste0(PATH, FILE))

Data = Data[sample(nrow(Data)),]

# verify correct columns
colnames(Data[5])  == "perimeter_mean"
colnames(Data[10]) == "concaveP_mean"

# # normalize function
# data_norm <- function(x) {((x - min(x))/ (max(x) - min(x)))}

# normalize data
# data <- data.frame(diagnosis = Data$diagnosis, lapply(Data[, c(5, 10)], data_norm))

data <- data.frame(diagnosis = Data$diagnosis, Data[, c(5, 10)])

# set seed
set.seed(123)

# train-test split
# set index intervals
ntrain <- floor(nrow(Data)*0.7)
ntest  <- (ntrain+1):nrow(Data)

# craete train and test data
data.train <- data[(1:ntrain),]
data.test  <- data[ntest,]

# KNN with prob = TRUE, we see the percentage of the votes for the winning class
knn_pred_true_1  <- knn(data.train[, c(2:3)], data.test[, c(2:3)], 
                       cl = data[(1:ntrain), 1], k = 1, prob = TRUE)
knn_pred_true_10 <- knn(data.train[, c(2:3)], data.test[, c(2:3)], 
                       cl = data[(1:ntrain), 1], k = 10, prob = TRUE)
knn_pred_true_25 <- knn(data.train[, c(2:3)], data.test[, c(2:3)], 
                       cl = data[(1:ntrain), 1], k = 25, prob = TRUE)

# We have a train data set on which we train the KNN algorithm. This means that the
# algorithm learns the location of every training data point and its type. Then the 
# algorithm takes the location of every single test data points, looks at the type 
# of it's k (i.e. 1, 10 or 25) nearest neighbors and predicts the class of the test 
# data point according to a majority vote. The probability which is in that case 
# displayed for the KNN algorithm shows us the percentage (or how close) the majority 
# vote was. If our predicted class was M and the probability 0.75, this means that
# 75% of the k neighbors of that point "voted" for the M-class.

# KNN with prob = FALSE, we do NOT see the percentage of the votes for the winning class
knn_pred_1  <- knn(data.train[, c(2:3)], data.test[, c(2:3)], cl = data[(1:ntrain), 1],
                  k = 1, prob = FALSE)
knn_pred_10 <- knn(data.train[, c(2:3)], data.test[, c(2:3)], cl = data[(1:ntrain), 1],
                  k = 10, prob = FALSE)
knn_pred_25 <- knn(data.train[, c(2:3)], data.test[, c(2:3)], cl = data[(1:ntrain), 1],
                  k = 25, prob = FALSE)

# show test errors and test error rate
err_table_1     <- table(knn_pred_1, data[ntest, 1])
err_rate_knn_1  <- round(((err_table_1[1, 2] + err_table_1[2, 1]) / length(knn_pred_1)) * 100, 2)
err_table_10    <- table(knn_pred_10, data[ntest, 1])
err_rate_knn_10 <- round(((err_table_10[1, 2] + err_table_10[2, 1]) / length(knn_pred_10)) * 100, 2)
err_table_25    <- table(knn_pred_25, data[ntest, 1])
err_rate_knn_25 <- round(((err_table_25[1, 2] + err_table_25[2, 1]) / length(knn_pred_25)) * 100, 2)

# run logistic regression on same data
log_reg <- glm(diagnosis ~ perimeter_mean + concaveP_mean, data = data.train,family = binomial)
yhat    <- predict(log_reg, data.test, type = "response")
yhat    <- ifelse(yhat<0.5,"B","M")

result       <- data.frame(diagnosis = data.test$diagnosis, prediction = yhat)
result$error <- ifelse(result$diagnosis != result$prediction, 1, 0)

# number of misclassified cases
misclas = sum(result$error)

# error rate
err_rate_log <- (round(misclas/nrow(data.test), 3))*100

# compare the error rates
errors <- data.frame(KNN1 = err_rate_knn_1, KNN10 = err_rate_knn_10, 
                     KNN25 = err_rate_knn_25, LR = err_rate_log, row.names = "Error Rate")
print(errors)

# We see that the logistic regression performed best, but only sligthly better than
# KNN with k = 10.




# Exercise 3 ###################################################################

# removing all variables/functions
rm(list = ls())

# 1.

# data
# Data
L.olympics=t(data.frame(c(1896,12),c(1900,11),c(1904,11),c(1906,11.2),c(1908,10.8),
                        c(1912,10.8),c(1920,10.8),c(1924,10.6),c(1928,10.8),c(1932,10.3),
                        c(1936,10.3),c(1948,10.3),c(1952,10.4),c(1956,10.5),c(1960,10.2),
                        c(1964,10),c(1968,9.95),c(1972,10.14),c(1980,10.25),c(1984,9.99),
                        c(1988,9.92),c(1992, 9.96),c(1996, 9.84),c(2000, 9.87),c(2004,9.85),
                        c(2008,9.69),c(2012, 9.61),c(2016, 9.83)))

colnames(L.olympics)=c("year","time")
rownames(L.olympics)=1:nrow(L.olympics)
L.olympics

my2NN <- function(data, col) {
  k <- 2
  pred <- rep(NA, nrow(data))
  
  for (i in 2:(nrow(data)-1)) {
    pred[i] <- (1/k) * (L.olympics[i-1, col] + L.olympics[i+1, col])
  }
  
  vec  <- data.frame(pred)
  data <- cbind(data, pred)
  return(data)
}

my2NN_pred <- function(data, col, year) {
  
  df <- my2NN(data, col)
  prediction <- df[which(df[,1] == year), 3]
  
  return(prediction)
}

# The algorithm is very simple, since we only have to add the previous and past 
# year's time and divide it by k (= 2). If we loop through the data set and, with 
# this procedure, create the prediction for each row, we have a prediction for
# year. If we receive new data, the only thing we have to do is to find the nearest
# neighbours and predict the class by majority vote.

# 2.
train <- L.olympics[-c(4, 10, 17, 24), ]
test  <- L.olympics[-c(4, 10, 17, 24), ]






