###############################################################################
## Filename: Machine Learning - Homework 3
## Topic: Homework 2
## Author: Jonas Huwyler, Haochen Li, Giovanni Magagnin, Daniil Bulat
## Matriculation Number: 16-610-958, 20-621-926, 17-300-914, 14-607-055
###############################################################################

#### Exercise 1

## Functions
gd = function(NumIt, weights){
  for (i in 1:NumIt) {
    yhat=Data$lstat*weights[2]+weights[1]
    error =  Data$medv  - yhat
    weights[1] = weights[1] + 2*eta*sum(error)
    weights[2] = weights[2] + 2*eta*(error%*%Data$lstat)
  }
  RSS = error%*%error
  RSE = sqrt(RSS/(leng-2))
  output_list = list("weights"=weights,"RSS"=RSS,"RSE"=RSE)
  return(output_list)
}

mbgd = function(NumIt, weights, batch, leng){
  for (j in 1:floor(leng/batch)){
    for (i in 1:NumIt) {
      yhat=Data$lstat[((j-1)*batch+1):(j*batch)]*weights[2]+weights[1]
      error =  Data$medv[((j-1)*batch+1):(j*batch)]  - yhat
      weights[1] = weights[1] + 2*eta*sum(error)
      weights[2] = weights[2] + 2*eta*(error%*%Data$lstat[((j-1)*batch+1):(j*batch)])
    }
  }
  RSS = error%*%error
  RSE = sqrt(RSS/(leng-2))
  output_list = list("weights"=weights,"RSS"=RSS,"RSE"=RSE)
  return(output_list)
}

gd_algo = function(NumIt,weights,batch,leng){
  NumOfIt = c(1:NumIt)
  rss_itr = c()
  for (i in 1:NumIt){
    output = mbgd(i, weights,batch,leng)
    rss_itr[i] = output$RSS
  }
}

gd_algo2 = function(NumIt,weights){
  NumOfIt = c(1:NumIt)
  rss_itr = c()
  for (i in 1:NumIt){
    output = gd(i, weights)
    rss_itr[i] = output$RSS
  }
}


## Data Preparation Boston Data Set
library(MASS)
Data = Boston[c("medv", "lstat")]

# Shuffle the data for better performance
Data = Data[sample(1:nrow(Data)),]

# Learning rate
eta = 0.000009

# Initial weight values
weights = c(30,0)

### 1. Write a mini-batch gradient descent algorithm for batch size 32 with 20 epochs/iterations for the
### Boston data set.

NumIt = 20 # Number of epochs
batch = 32 # Batch size
leng = nrow(Data)

mbgd(NumIt, weights, batch, leng)


### 2. Plot a learning curve for your mini-batch gradient descent algorithm, where the learning performance
### is measured by the RSS and the experience is given by the number of epochs/iterations, from 1 to 20.
NumOfIt = c(1:NumIt)
rss_itr = c()
for (i in 1:NumIt){
  output = mbgd(i,weights,batch,leng)
  rss_itr[i] = output$RSS
}

plot(NumOfIt, rss_itr, type="b", ylab="RSS")

### 3. Plot a learning curve for the full gradient descent algorithm, where the learning performance is measured
### by the RSS and the experience is given by the number of epochs/iterations, from 1 to 20.
NumOfIt = c(1:NumIt)
rss_itr = c()
batch = leng
for (i in 1:NumIt){
  output = mbgd(i, weights,batch,leng)
  rss_itr[i] = output$RSS
}

plot(NumOfIt, rss_itr, type="b", ylab="RSS")

### 4. Compare your results. Which combination of batch size (32 or 506) and number of epochs/iterations
# ##would you recommend?



### 5. Measure the run time for the full gradient descent algorithm (batch size 506) and the mini-batch
### gradient descent (batch size 32) for 20 epochs/iterations. Compare the two results.
### For those who want more challenge:Write a general algorithm with a variable batch size and number
### of epochs.

## Run Time Specific Algorithm
# Batch = 506, epochs = 20
start = Sys.time()  
gd(NumIt, weights)
finish = Sys.time()
run_time = finish - start
print(paste0("For Batch = 506, run time =", run_time))


# Batch = 32, epochs = 20
start = Sys.time()  
mbgd(NumIt, weights, 32, leng)
finish = Sys.time()
run_time = finish - start
print(paste0("For Batch = 32, run time =", run_time))


## Run Time General Algorithm
# Batch = 506, epochs = 1-20 (variable batch size)
start = Sys.time()  
gd_algo(NumIt,weights,506,leng)
finish = Sys.time()
run_time = finish - start
print(paste0("For Batch = 506, run time =", run_time))

# Batch = 506, epochs = 1-20 (fixed batch size)
start = Sys.time()  
gd_algo2(NumIt,weights)
finish = Sys.time()
run_time = finish - start
print(paste0("For Batch = 506, and epochs = 1-20, run time =", run_time))

# Batch = 32, epochs = 1-20
start = Sys.time()  
gd_algo(NumIt,weights,32,leng)
finish = Sys.time()
run_time = finish - start
print(paste0("For Batch = 32 and epochs = 1-20, run time =", run_time))




