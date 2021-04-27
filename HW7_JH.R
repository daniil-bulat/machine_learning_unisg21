###############################################################################
## Filename: Machine Learning
## Topic: Homework 7
## Author: Jonas Huwyler...
## Matriculation Number: 16-610-958...
###############################################################################

# removing all variables/functions
rm(list = ls())

# install packages
install.packages('class')

# load library
library(class) 

# call paths
PATH <- "/Users/jones/Desktop/FS21/Machine Learning/Homeworks/HW7/"
FILE <- "wdbcData.RData"
setwd(dir = PATH)

#import data
load(paste0(PATH, FILE))

# verify correct columns
colnames(Data[5])  == "perimeter_mean"
colnames(Data[10]) == "concaveP_mean"

# normalize function
data_norm <- function(x) {((x - min(x))/ (max(x) - min(x)))}

# normalize data
data <- data.frame(diagnosis = Data$diagnosis, lapply(Data[, c(5, 10)], data_norm))

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

# KNN with prob = FALSE, we do NOT see the percentage of the votes for the winning class
knn_pred_1  <- knn(data.train[, c(2:3)], data.test[, c(2:3)], cl = data[(1:ntrain), 1],
                  k = 1, prob = FALSE)
knn_pred_10 <- knn(data.train[, c(2:3)], data.test[, c(2:3)], cl = data[(1:ntrain), 1],
                  k = 10, prob = FALSE)
knn_pred_25 <- knn(data.train[, c(2:3)], data.test[, c(2:3)], cl = data[(1:ntrain), 1],
                  k = 25, prob = FALSE)

# show errors and error rate
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



