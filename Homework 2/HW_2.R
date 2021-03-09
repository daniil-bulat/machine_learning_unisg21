
###############################################################################
## Filename: Machine Learning - Homework 2
## Topic: Homework 2
## Author: Jonas Huwyler...
## Matriculation Number: 16-610-958...
###############################################################################

# Set seed
set.seed(123)

# Exercise 1

# Shuffle the iris data...
data = iris[sample(1:150),]

# Split data into train and test set (80% - 20%)
data.train = data[1:120,]
data.test  = data[121:150,]

# 1.) 
model_1 <- lm(Sepal.Length ~ Petal.Length, data = data.train)
MS1     <- summary(model_1)
print(paste("The RSE of the training data is: ", round(MS1$sigma, 3)))
print(paste("The R^2 of the training data is: ", round(MS1$r.squared, 3)))

#The R-squared is relatively high considering we just use one explanatory variable,
#there is obviously still room for improvement but the model seems to work well.
#The influence of Petal.Length on Sepal.Length is highly significant since 
#the p-value of the variable is smaller than 0.01. 

# 2.) 
model_2 <- lm(Sepal.Length ~ Petal.Length + Petal.Width, data = data.train)
MS2     <- summary(model_2)
print(paste("The RSE of the training data is: ", round(MS2$sigma, 3)))
print(paste("The R^2 of the training data is: ", round(MS2$r.squared, 3)))

model_3 <- lm(Sepal.Length ~ Petal.Length + Petal.Width + Sepal.Width, data = data.train)
MS3     <- summary(model_3)
print(paste("The RSE of the training data is: ", round(MS3$sigma, 3)))
print(paste("The R^2 of the training data is: ", round(MS3$r.squared, 3)))

# 3.) 
# Predict the sepal length and calculate RSE and R^2
pred <- predict.lm(model_1, newdata = data.test)
err  <- data.test$Sepal.Length - pred
RSS  <- err %*% err
RSE  <- sqrt(RSS/(nrow(data.test) - 2))
TSS  <- sum((data.test$Sepal.Length - mean(data.test$Sepal.Length))^2)
R2   <- (TSS - RSS)/(TSS)

# The model performs very well on the test data. The RSE is even smaller on the
# test than on the train data. This is also due to a lower TSS on the test
# data. Th R^2 is a little bit lower than in the train data but still high.


# Exercise 2

# Data
L.olympics=t(data.frame(c(1896,12),c(1900,11),c(1904,11),c(1906,11.2),c(1908,10.8),
                        c(1912,10.8),c(1920,10.8),c(1924,10.6),c(1928,10.8),c(1932,10.3),
                        c(1936,10.3),c(1948,10.3),c(1952,10.4),c(1956,10.5),c(1960,10.2),
                        c(1964,10),c(1968,9.95),c(1972,10.14),c(1980,10.25),c(1984,9.99),
                        c(1988,9.92),c(1992, 9.96),c(1996, 9.84),c(2000, 9.87),c(2004,9.85),
                        c(2008, 9.69),c(2012, 9.61),c(2016, 9.83)))
colnames(L.olympics)=c("year","time")
rownames(L.olympics)=1:nrow(L.olympics)
L.olympics

# 1.)
train1 <- data.frame(L.olympics[1:7,])
test1  <- data.frame(L.olympics[8:14,])

f1    <- lm(time ~ year, data = train1)
pred1 <- predict.lm(f1, newdata = test1)
err1  <- test1$time - pred1
RSS1  <- err1 %*% err1

# 2.)
train2 <- data.frame(L.olympics[1:14,])
test2  <- data.frame(L.olympics[15:21,])

f2    <- lm(time ~ year, data = train2)
pred2 <- predict.lm(f2, newdata = test2)
err2  <- test2$time - pred2
RSS2  <- err2 %*% err2

# 3.)
train3 <- data.frame(L.olympics[1:21,])
test3  <- data.frame(L.olympics[22:28,])

f3    <- lm(time ~ year, data = train3)
pred3 <- predict.lm(f3, newdata = test3)
err3  <- test3$time - pred3
RSS3  <- err3 %*% err3

# 4.)
RSS_av <- (1/3)*(RSS1 + RSS2 + RSS3)
