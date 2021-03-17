# Univeristy of St.Gallen
# 8330 Mahcine Learning, 2021 Spring
# Homework 3
# Mar 15, 2021


# Removing all variables/functions
rm(list = ls())

# Set the randam seed
set.seed(123)

# Learning rate (a smart guess)
eta = 0.000009

# Initial weights (a smart guess)
weights = c(30,0)

# Number of iterations (epochs) for the algorithm
NumIt = 20

# Data Preparation (Boston Data)
library(MASS)
# Keep only the features medv and lstat
Data = Boston[c("medv", "lstat")]
# Shuffle the data for (likely) better performance
Data = Data[sample(1:nrow(Data)),]

# Question 1 --------------------------------------------------------------

# Number of batch size for the algorithm
BatSize = 32

# Number of batches for the Data
NumBat = floor(nrow(Data)/BatSize)+1

# Create lists to store results of each epoch
NumOfIt = rep(NA,NumIt)
NumOfRSS = rep(NA,NumIt)

# Set Start time of MBGD
MBGD.start = Sys.time()

# The mini-batch gradient descent algorithm
# Loops w.r.t. epochs (20 Iterations)
for (i in 1:NumIt){
  # Loops to update weights w.r.t. mini batches (16 batches) for specified epoch
  for (j in 1:NumBat){
    # Create Mini Batch regarding for index from 1 to 16
    if (j < NumBat){
      Data.Train = Data[((j-1)*BatSize+1):(j*BatSize),]
    } else {
      Data.Train = Data[((j-1)*BatSize+1):nrow(Data),]
    }
    
    # calculate stochastic descent algorithm for the specified mini batch in specified epoch
    yhat = Data.Train$lstat*weights[2]+weights[1]
    error = Data.Train$medv - yhat
    weights[1] = weights[1] + 2*eta*sum(error)
    weights[2] = weights[2] + 2*eta*sum(error*Data.Train$lstat)
    # print(weights)
  }
  
  # Compute and store numbers of iterations and RSS for each epoch
  NumOfIt[i] = i
  yhat=Data$lstat*weights[2]+weights[1]
  error =  Data$medv  - yhat
  RSS = error%*%error
  NumOfRSS[i] = RSS
}

# Set end time of MBGD
MBGD.end = Sys.time()

# Calcualate error for the whole sample using the final weights
yhat=Data$lstat*weights[2]+weights[1]
error =  Data$medv  - yhat

# Final output
print(weights)
RSS = error%*%error
print(RSS)
RSE = sqrt(RSS/(nrow(Data)-2))
print(RSE)

# Question 2 --------------------------------------------------------------

#Plot the learning curve for the MBGD algo
plot(NumOfIt,NumOfRSS, type="b", main = "Learning Curve of Mini-Batch Gradient Descent Algorithm")


# Question 3 --------------------------------------------------------------

# Reset weights and lists to store
weights = c(30,0)
NumOfIt = rep(NA,NumIt)
NumOfRSS = rep(NA,NumIt)

# Set Start time of FGD
FGD.start = Sys.time()

# The full gradient descent algorithm (identical to the code example)
# Loops w.r.t. epochs (20 Iterations)
for (i in 1:NumIt){
  # The gradient descent algorithm
  yhat=Data$lstat*weights[2]+weights[1]
  error =  Data$medv  - yhat
  weights[1] = weights[1] + 2*eta*sum(error)
  weights[2] = weights[2] + 2*eta*(error%*%Data$lstat)
  # print(weights)
  
  # Compute and record numbers of iterations and RSS for each epoch
  NumOfIt[i] = i
  yhat=Data$lstat*weights[2]+weights[1]
  error =  Data$medv  - yhat
  RSS = error%*%error
  NumOfRSS[i] = RSS
}

# Set end time of FGD
FGD.end = Sys.time()

# Calcualate error for the whole sample using the final weights
yhat=Data$lstat*weights[2]+weights[1]
error =  Data$medv  - yhat

# Final output
print(weights)
RSS = error%*%error
print(RSS)
RSE = sqrt(RSS/(nrow(Data)-2))
print(RSE)

#Plot the learning curve for the FGD algo
plot(NumOfIt,NumOfRSS, type="b", main = "Learning Curve of Full Gradient Descent Algorithm")

# Question 4 --------------------------------------------------------------

# I would recommend MBGD since it generates a sampler RSS and RSE with the same 20 iterations.

# Question 5 --------------------------------------------------------------

MBGD.time = MBGD.end - MBGD.start
cat("The run time of MBGD for 20 epochs is",MBGD.time, "secs")
FGD.time = FGD.end - FGD.start
cat("The run time of FGD for 20 epochs is",FGD.time, "secs")
cat("The time difference is",abs(FGD.time - MBGD.time), "secs")


