################################################################################

##### MACHINE LEARNING - HOMEWORK 2 ############################################

################################################################################



##### Exercise 1 #####

### Set up
rm(list = ls())
set.seed(123)

### Shuffle the Iris data
Data = iris[sample(1:150),]

### Split into train and test data (80-20)
Data.Train = Data[1:120,]
Data.Test = Data[121:150,]


### 1. Simple linear regression x=Petal.Length and y=Sepal.Length
regre_1 = lm(Sepal.Length ~ Petal.Length, data = Data.Train)
# Using the summary command to get RSE and R^2
summary_1 = summary(regre_1)
# Commenting on RSE and R^2
#


### 2. Simple linear regression with 2 and 3 covariates
# 2 covariates
regre_2 = lm(Sepal.Length ~ Petal.Length + Petal.Width, data = Data.Train)
summary_2 = summary(regre_2)
# 3 covariates
regre_3 = lm(Sepal.Length ~ Petal.Length + Petal.Width + Sepal.Width, data = Data.Train)
summary_3 = summary(regre_3)
# summaries
summary_1
summary_2
summary_3
# comments
#


### 3. Prediction with 1 covariate model
pred = as.data.frame(regre_1$coefficients[1] + regre_1$coefficients[2]*Data.Test$Petal.Length)
# RSS
rss_pred = sum((Data.Test$Sepal.Length - pred)^2)
# TSS
mean_test = mean(Data.Test$Sepal.Length)
tss_pred = sum((pred - mean_test)^2)
# RSE 
rse_pred = sqrt(rss_1/(30-2))
# R^2
R2 = (tss_pred - rss_pred)/tss_pred
# How would you rate the model's performance?
#



##### Exercise 2 #####

### Set up
L.olympics=t(data.frame(c(1896,12),c(1900,11),c(1904,11),c(1906,11.2),c(1908,10.8),c(1912,10.8),c(1920,10.8),c(1924,10.6),c(1928,10.8),c(1932,10.3),c(1936,10.3),c(1948,10.3),c(1952,10.4),c(1956,10.5),c(1960,10.2),c(1964,10),c(1968,9.95),c(1972,10.14),c(1980,10.25),c(1984,9.99),c(1988,9.92),c(1992, 9.96),c(1996, 9.84),c(2000, 9.87),c(2004,9.85),c(2008, 9.69),c(2012, 9.61),c(2016, 9.83)))
colnames(L.olympics)=c("year","time")
rownames(L.olympics)=1:nrow(L.olympics)

### dataframe
olympics = as.data.frame(L.olympics)


### 1. First 7 data points as training
# split into train and test
olympics_train_1 = olympics[1:7,]
olympics_test_1 = olympics[8:14,]
# linear regression
f_1 = lm(time ~ year, data = olympics_train_1)
summary(f_1)
# prediction
olympics_pred_1 = as.data.frame(f_1$coefficients[1] + f_1$coefficients[2]*olympics_test_1$year)
# RSS
olympics_rss_1 = sum((olympics_test_1$time - olympics_pred_1)^2)


### 2. First 14 data points as training
# split into train and test
olympics_train_2 = olympics[1:14,]
olympics_test_2 = olympics[15:21,]
# linear regression
f_2 = lm(time ~ year, data = olympics_train_2)
summary(f_2)
# prediction
olympics_pred_2 = as.data.frame(f_2$coefficients[1] + f_2$coefficients[2]*olympics_test_2$year)
# RSS
olympics_rss_2 = sum((olympics_test_2$time - olympics_pred_2)^2)


### 3. First 21 data points as training
# split into train and test
olympics_train_3 = olympics[1:21,]
olympics_test_3 = olympics[22:28,]
# linear regression
f_3 = lm(time ~ year, data = olympics_train_3)
summary(f_3)
# prediction
olympics_pred_3 = as.data.frame(f_3$coefficients[1] + f_3$coefficients[2]*olympics_test_3$year)
# RSS
olympics_rss_3 = sum((olympics_test_3$time - olympics_pred_3)^2)


### 4. Average RSS of the three models
olympics_rss_mean = (olympics_rss_1 + olympics_rss_2 + olympics_rss_3)/3




