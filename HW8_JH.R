###############################################################################
## Filename: Machine Learning
## Topic: Homework 8
## Author: Jonas Huwyler...
## Matriculation Number: 16-610-958...
###############################################################################

# Exercise 1

# removing all variables/functions
rm(list = ls())

# install packages
install.packages('e1071')

# load library
library(e1071) 

# set seed
set.seed(123)

# call paths
PATH <- "/Users/jones/Desktop/FS21/Machine Learning/Homeworks/HW8/"
setwd(dir = PATH)

#import data
# iris data subset 
data <- iris[(1:100),]

# shuffle the data set
data <- data[sample(nrow(data)),]

# run svm with cost equal to 10, 1, 0.1
fit1 <- svm(Species ~ Sepal.Length + Petal.Length, data = data, kernel = "linear", 
            cost = "10", scale = FALSE)
fit2 <- svm(Species ~ Sepal.Length + Petal.Length, data = data, kernel = "linear", 
            cost = "1", scale = FALSE)
fit3 <- svm(Species ~ Sepal.Length + Petal.Length, data = data, kernel = "linear", 
            cost = "0.1", scale = FALSE)

# plot results
plot(fit1, data = data[, c(1, 3, 5)])
plot(fit2, data = data[, c(1, 3, 5)])
plot(fit3, data = data[, c(1, 3, 5)])

# find support vectors for cost = 0.1
print(fit2$SV)

# find the equation of the separating line
fit2$coefs[1:2]%*%fit2$SV

# equation = print(paste("equation", "=", round(fit2$rho, 2), "+", round(fit2$coefs[1], 2),
#                        "*x1", "+", round(fit2$coefs[2], 2), "*x2", "+", 
#                        round(fit2$coefs[3], 2), "*x3", "+", round(fit2$coefs[4], 2),
#                        "*x4"))

# do the same for versicolor and virginica
# iris data subset with only petal.length and sepal.length
data <- iris[(51:150),]

# shuffle the data set
data <- data[sample(nrow(data)),]

# run svm with cost equal to 1
fit4 <- svm(Species ~ Sepal.Length + Petal.Length, data = data, kernel = "linear", 
            cost = "1", scale = FALSE)

# plot results
plot(fit4, data = data[, c(1, 3, 5)])

# find support vectors for cost = 0.1
nrow(fit4$SV)


# Exercise 2

# removing all variables/functions
rm(list = ls())

data <- data.frame("x1" = c(-1, 1, 0.5, 1, 3, 3.5, 3.5, 4), 
                   "x2" = c(2, 1, 3, 4, 2, 1, 4, 2), 
                   "class" = c(-1, -1, -1, -1, 1, 1, 1, 1))

plot(data[, 1], data[, 2], col=c("red","blue"), xlab = "x1", ylab = "x2",
     main = "Exercise 2")
abline(v = 2, col = "black") # maximal margin hyperplane
abline(v = 1, col = "black", lty = 2) # margin
abline(v = 3, col = "black", lty = 2) # margin
abline(v = 0.5, col = "black", lty = 3) # soft margin
abline(v = 3.5, col = "black", lty = 3) # soft margin
print(paste("The support vectors are (1, 1), (1, 4), (3, 2) and (3.5, 4)."))


# Exercise 3

#graph 1
y <- array(0, dim=c(20, 1))
for (i in seq(0, 20, 1)) {
    y[i] = ((i**2)/2) + ((i**2)/4) - 1
}
plot(y, type = "l", main = "Graph 1")

#graph 2
y <- array(0, dim=c(20, 1))
for (i in seq(0, 20, 1)) {
  y[i] = ((i**2)/2) - ((i**2)/1) - 2
}
plot(y, type = "l", main = "Graph 2")

#graph 3
y <- array(0,dim=c(20, 1))
for (i in seq(0, 20, 1)) {
  y[i] = i + i - 2
}
plot(y, type = "l", main = "Graph 3")

#graph 4
y <- array(0,dim=c(20, 1))
for (i in seq(0, 20, 1)) {
  y[i] = i - i + i*i
}
plot(y, type = "l", main = "Graph 4")


