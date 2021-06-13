################################################################################
# Machine Learning
################################################################################
# Project: Artificial Neural Network
################################################################################

# set working directory
setwd("/Users/jones/Desktop")

# clean environment
rm(list = ls())

# data gathering ###############################################################

# get data
url  = "https://raw.githubusercontent.com/daniil-bulat/machine_learning_unisg21/main/Final%20Paper/Data/analysis%20data/combined_data_5_features.csv"
data = read.csv(url)

# machine learning #############################################################

# load libraries
library(neuralnet)
library(doSNOW)
library(tibble)
library(ggplot2)

## neural network
# creating index variable 
data = data[, c(5:10)]
data$direction[data$direction == -1] = 0

# standardize data 
standFun = function(x){
  out = (x - mean(x))/sd(x)
  return(out)
}

features = names(data)[2:6]

for (i in 1:5){
  data[[features[i]]] = standFun(data[[features[i]]])
}

# random sampling
set.seed(123)
samplesize = 0.70 * nrow(data)
index = sample(seq_len(nrow(data)), size = samplesize)

## fit different neural networks

# start parallel processing to decrease computation time
# get the number of cores available
ncores <- parallel::detectCores()
# set cores for parallel processing
ctemp <- makeCluster(ncores) #
registerDoSNOW(ctemp)

# Perceptron
set.seed(123)
NN0 = neuralnet(direction ~ ., 
                data = data, 
                linear.output = FALSE,
                err.fct = 'ce', 
                act.fct = 'logistic',
                likelihood = TRUE, 
                hidden = c(0),
                stepmax = 10000000) 

# 1-Hidden Layers, Layer-1 2-neurons
set.seed(123)
NN1 = neuralnet(direction ~ ., 
                data = data, 
                linear.output = FALSE,
                err.fct = 'ce', 
                act.fct = 'logistic',
                likelihood = TRUE, 
                hidden = c(3),
                stepmax = 10000000) 

# start time
start_time <- Sys.time()

# 2-Hidden Layers, Layer-1 3-neurons, Layer-2, 2-neuron
set.seed(123)
NN2 = neuralnet(direction ~ ., 
                data = data, 
                linear.output = FALSE,
                err.fct = 'ce', 
                act.fct = 'logistic',
                likelihood = TRUE, 
                hidden = c(3, 3),
                stepmax = 10000000) 

# stop time and display
end_time <- Sys.time()
print(end_time - start_time)

# start time
start_time <- Sys.time()

# 4-Hidden Layers, Layer-1 3-neurons, Layer-2, 3-neuron, Layer-3, 3-neuron
set.seed(123)
NN3 = neuralnet(direction ~ ., 
                data = data, 
                linear.output = FALSE,
                err.fct = 'ce', 
                act.fct = 'logistic',
                likelihood = TRUE, 
                hidden = c(3, 3, 2),
                stepmax = 100000000) 

# stop time and display
end_time <- Sys.time()
print(end_time - start_time)

# stop the snow clusters
stopCluster(cl = ctemp)

# bar plot of results
Class_NN_ICs <- tibble('Network' = rep(c("NN0", "NN1", "NN2", "NN3"), each = 3), 
                       'Metric'  = rep(c('AIC', 'BIC', 'Error * 100'), length.out = 12),
                       'Value'  = c(NN0$result.matrix[4,1], NN0$result.matrix[5,1], 100*NN0$result.matrix[1,1], 
                                    NN1$result.matrix[4,1], NN1$result.matrix[5,1], 100*NN1$result.matrix[1,1],
                                    NN2$result.matrix[4,1], NN2$result.matrix[5,1], 100*NN2$result.matrix[1,1],
                                    NN3$result.matrix[4,1], NN3$result.matrix[5,1], 100*NN3$result.matrix[1,1]))

Class_NN_ICs %>%
  ggplot(aes(Network, Value, fill = Metric)) +
  geom_col(position = 'dodge')  +
  ggtitle("AIC, BIC, and Cross-Entropy Error of the Classification ANNs", "Note: ce Error displayed is 100 times its true value")

# function to measure the performance of each neural net
NN_performance <- function(NN) {
  
  # creating training and test set
  train.NN = data[index , ]
  test.NN  = data[-index , ]
  
  # plot neural network
  plot(NN, rep = 'best')
  
  ## in sample prediction using neural network
  predict_train.NN = compute(NN, train.NN[, -1])
  
  # compare
  results.train       = data.frame(actual = train.NN$direction, prediction = ifelse(predict_train.NN$net.result > 0.5, 1, 0))
  results.train$error = ifelse(results.train$actual != results.train$prediction, 1, 0)
  res.train_table     = table("actual" = results.train$actual, "pred" = results.train$prediction)
  
  # number of misclassified cases
  misclas.train = sum(results.train$error)
  
  # false positives 
  x.train.fp   = ifelse(results.train$prediction == 1 & results.train$actual == 0, 1, 0)
  falPos.train = sum(x.train.fp)
  
  # false negatives 
  x.train.fn   = ifelse(results.train$prediction == 0 & results.train$actual == 1, 1, 0)
  falNeg.train = sum(x.train.fn)
  
  # accuracy rate
  acc.train = mean(results.train$actual == results.train$prediction)
  
  # MSE 
  MSE_train = mean((results.train$actual - predict_train.NN$net.result)^2)
  
  ## out of sample prediction using neural network
  predict_test.NN = compute(NN, test.NN[, c(2:6)])
  
  # compare
  results.test       = data.frame(actual = test.NN$direction, prediction = ifelse(predict_test.NN$net.result > 0.5, 1, 0))
  results.test$error = ifelse(results.test$actual != results.test$prediction, 1, 0)
  res.test_table     = table("actual" = results.test$actual, "pred" = results.test$prediction)
  
  # number of misclassified cases
  misclas.test = sum(results.test$error)
  
  # false positives 
  x.test.fp   = ifelse(results.test$prediction == 1 & results.test$actual == 0, 1, 0)
  falPos.test = sum(x.test.fp)
  
  # false negatives 
  x.test.fn   = ifelse(results.test$prediction == 0 & results.test$actual == 1, 1, 0)
  falNeg.test = sum(x.test.fn)
  
  # accuracy rate
  acc.test = mean(results.test$actual == results.test$prediction)
  
  # MSE 
  MSE_test = mean((results.test$actual - predict_test.NN$net.result)^2)
  
  ## results
  Results    <- c("In-sample", "Out-sample")
  Accuracy   <- c(paste0(round(acc.train, 4)*100, "%"), paste0(round(acc.test, 4)*100, "%"))
  MSE        <- c(round(MSE_train, 4), round(MSE_test, 4))
  Misclassed <- c(paste0(round(misclas.train/nrow(train.NN), 4)*100, "%") , paste0(round(misclas.test/nrow(test.NN), 4)*100, "%"))
  FalsePos.  <- c(paste0(round(falPos.train/nrow(train.NN), 4)*100, "%"), paste0(round(falPos.test/nrow(test.NN), 4)*100, "%"))
  FalseNeg.  <- c(paste0(round(falNeg.train/nrow(train.NN), 4)*100, "%"), paste0(round(falNeg.test/nrow(test.NN), 4)*100, "%"))
  
  NN.results <- data.frame(Results, Accuracy, MSE, Misclassed, FalsePos., FalseNeg.)
  print(NN.results)
}

# Performance of the models
NN_performance(NN0)
NN_performance(NN1)
NN_performance(NN2)
NN_performance(NN3)

  