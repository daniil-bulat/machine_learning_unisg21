
################################################################################
## Filename: Machine Learning - Homework 3
## Topic: Homework 3
## Author: Jonas Huwyler...
## Matriculation Number: 16-610-958...
################################################################################

# removing all variables/functions
rm(list = ls())

# load library
library(ggplot2)
library(MASS)

# load Boston data set
attach(Boston)
Boston

# consider the variables 'medv' and 'lstat'
data <- Boston[c('medv', 'lstat')]

# shuffle the data
data <- data[sample(1:nrow(data)), ]

# learning rate
eta <- 0.000009

# initial weights (a smart guess)
weights = c(30,0)


# 1. ---------------------------------------------------------------------------

# number of iterations
num_it <- 20

# batch size
bat_size <- 32

# number of batches in the data
num_bat <- floor(nrow(data)/bat_size)

# lists to store results
Iteration <- rep(NA, num_it)
RSS       <- rep(NA, num_it)

# measure run time
MBGD_start <- Sys.time()

# mini-batch gradient descent algorithm
# loop over iterations of the algorithm
for (i in 1:num_it) {
  # loop over batches
  for (j in 1:num_bat) {
    # if last batch, add the remaining data to the batch
    if (j < num_bat) {
      data_GD <- data[((j-1)*bat_size+1):(j*bat_size), ]
    } else {
      data_GD <- data[((j-1)*bat_size+1):nrow(data), ]
    }
    
    # update the weights 
    yhat <- weights[1] + data_GD$lstat*weights[2]
    err  <- data_GD$medv - yhat
    weights[1] <- weights[1] + 2*eta*sum(err)
    weights[2] <- weights[2] + 2*eta*sum(err%*%data_GD$lstat)
  }
  
  print(weights)
  
  yhat <- weights[1] + data$lstat*weights[2]
  err  <- data$medv - yhat
  rss  <- err %*% err
    
  RSS[i]       <- rss
  Iteration[i] <- i
  }

# measure run time
MBGD_end <- Sys.time()

# print run time
time_MBGD <- MBGD_end - MBGD_start
print(time_MBGD)

# final output
yhat <- weights[1] + data$lstat*weights[2]
err  <- data$medv - yhat
rss  <- err %*% err

print(weights)
print(rss)



# 2. ---------------------------------------------------------------------------

plot(Iteration, RSS, type = "b", main = 'Learning Curve Mini-Batch Gradient Descent')



# 3. ---------------------------------------------------------------------------

# restore initial weights
weights = c(30,0)

# lists to store results
Iteration1 <- rep(NA, num_it)
RSS1       <- rep(NA, num_it)

# measure run time
GD_start <- Sys.time()

# gradient descent algorithm
# loop over iterations of the algorithm
for (i in 1:num_it) {
  # update the weights 
  yhat <- weights[1] + data$lstat*weights[2]
  err  <- data$medv - yhat
  weights[1] <- weights[1] + 2*eta*sum(err)
  weights[2] <- weights[2] + 2*eta*(err%*%data$lstat)
  
  # print weights
  print(weights)
  
  # calculate the rss after updating the weights
  yhat <- weights[1] + data$lstat*weights[2]
  err  <- data$medv - yhat
  rss  <- err %*% err
  
  # fill empty lists
  RSS1[i]       <- rss
  Iteration1[i] <- i
}

# measure run time
GD_end <- Sys.time()

# print run time
time_GD <- GD_end - GD_start
print(time_GD)

# final output
yhat <- weights[1] + data$lstat*weights[2]
err  <- data$medv - yhat
rss  <- err %*% err

print(weights)
print(rss)

plot(Iteration1, RSS1, type = "b", main = 'Learning Curve Gradient Descent')


# 5. ---------------------------------------------------------------------------
print(paste("Run time of Gradient Descent: ", round(time_GD, 3)))
print(paste("Run time of Mini Batch Gradient Descent: ", round(time_MBGD, 3)))
print(paste("Time difference: ", round(time_MBGD - time_GD, 3)))

