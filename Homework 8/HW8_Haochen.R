# Univeristy of St.Gallen
# 8330 Mahcine Learning, 2021 Spring
# Homework 8
# May 5, 2021

# Set up
rm(list = ls())
set.seed(123)

mainDir = "C:/Labs/Machine Learning/HW8"
setwd(mainDir)
getwd()


# Exercise 1 --------------------------------------------------------------

# Package space 
# install.packages("e1071")
library(e1071)
library(ggplot2)

# Data Preparation
Data = iris[sample(1:100),c(1,3,5)]
#Data$Species = ifelse(Data$Species == "setosa", -1, 1)
#Data$Species = factor(Data$Species,levels = c(-1,1), labels = c("setosa","versicolor"))

# * Question 1 ------------------------------------------------------------

# Run svm on the data (without scaling) with cost equal to 10, 1, 0.1
# plot the corresponding outcome.

# svm with cost = 0.1
fit1 = svm(Species ~., data= Data, kernel = "linear", cost = "0.1",scale = FALSE)
# plot svm model 1
plot(fit1,data = Data)

# svm with cost = 1
fit2 = svm(Species ~., data= Data, kernel = "linear", cost = "1",scale = FALSE)
# plot svm model 2
plot(fit2,data = Data)

# svm with cost = 10
fit3 = svm(Species ~., data= Data, kernel = "linear", cost = "10",scale = FALSE)
# plot svm model 3
plot(fit3,data = Data)


# * Question 2 ------------------------------------------------------------

# For cost = 0.1 list the support vectors.
print(fit1$SV)

print(fit2$SV)



# * Question 3 ------------------------------------------------------------

# For cost = 0.1 find the equation of separating line.
fit1$rho
print(fit1$SV)t(fit1$coefs)%*%fit1$SV



# * Question 4 ------------------------------------------------------------

# Compare the data samples for versicolor and virginica.
# run svm on the data (w.o scaling) with cost equal to 1.

# Data Preparationg
Data2 = iris[sample(51:150),c(1,3,5)]
#Data2$Species = ifelse(Data2$Species == "versicolor", -1, 1)
#Data2$Species = factor(Data2$Species,levels = c(-1,1), labels = c("versicolor","virginica"))
#Data2$Species = as.factor(Data2$Species)

# SVM with cost = 0.1
fit4 = svm(Species ~., data= Data2, kernel = "linear", cost = "0.1",scale = FALSE)
fit4$SV

# plot svm model 4
plot(fit4,data = Data2)


# Exercise 2 --------------------------------------------------------------

# Data Preparation
Data.Ex2 = t(data.frame(c(-1,2),c(1,1),c(0.5,3),c(1,4),c(3,2),c(3.5,1),c(3.5,4),c(4,2)))
Data.Ex2 = data.frame(Data.Ex2)
colnames(Data.Ex2) = c("x","y")
rownames(Data.Ex2) = 1:nrow(Data.Ex2)

# * Question 1 ------------------------------------------------------------ 

# Plot the hyperplane and the margin.
fig1 = ggplot(Data.Ex2,aes(x,y)) + 
  geom_point() +
  geom_vline(xintercept = 2, col = "red") +
  geom_vline(xintercept = 1, col = "blue", lty = 2) +
  geom_vline(xintercept = 3, col = "blue", lty = 2)

# save the plot
ggsave(filename = "fig1.png", plot = fig1)

# show the plot
print(fig1)

# * Question 2 ------------------------------------------------------------ 

# Plot the hyperplane and the soft margin.
fig2 = ggplot(Data.Ex2,aes(x,y)) + 
  geom_point() +
  geom_vline(xintercept = 2, col = "red") +
  geom_vline(xintercept = 0.5, col = "blue", lty = 2) +
  geom_vline(xintercept = 3.5, col = "blue", lty = 2)

# save the plot
ggsave(filename = "fig2.png", plot = fig2)

# show the plot
print(fig2)


# Exercise 3 --------------------------------------------------------------

# # * Question 1 ------------------------------------------------------------ 
# curve(sqrt(2*(1-x^2/4)),from = -2, to = 2)
# curve(sqrt(4*(1-x^2/2)),from = -sqrt(2), to = sqrt(2))
# 
# eq1a = function(x){sqrt(2*(1-x^2/4))}
# eq1b = function(x){-sqrt(2*(1-x^2/4))}
# ggplot(data.frame(x = c(-2,2)),aes(x)) +
#   stat_function(fun = eq1a) +
#   stat_function(fun = eq1b) +
#   xlim(-2,2)+
#   ylim(-2,2)+
#   coord_flip() +
#   labs(
#     x = "x2",
#     y = "x1"
#   )
# 
# # * Question 2 ------------------------------------------------------------ 
# curve(sqrt(2*(2+x^2)), from = -2, to = 2)
# curve(eq2b,from = -2, to = 2 )
# 
# eq2a = function(x){sqrt(2*(2+x^2))}
# eq2b = function(x){-sqrt(2*(2+x^2))}
# ggplot(data.frame(x = c(-2,2)),aes(x)) +
#   #stat_function(fun = eq2a) +
#   stat_function(fun = eq2b) 
#   # xlim(-2,2)+
#   # ylim(-2,2)+
#   # coord_flip() +
#   # labs(
#   #   x = "x2",
#   #   y = "x1"
#   # )
