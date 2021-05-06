################################################################################

###### G - MAGAGNIN: MAcHINE LEARNING, HW8  ####################################

################################################################################


###### Exercise 1 ######

library(e1071)
library(ggplot2)

# Loading data (100 observations, sepal_lenght, petal_length)
irisdata <- iris[(1:100), c(1, 3, 5)]
names(irisdata) <- c("sepal", "petal", "species")

### 1. Run svm on the data (without scaling) with cost equal to 10, 1, 0.1, and plot the outcomes

# Fitting the models
# cost = 10
fit_10 = svm(species ~ sepal + petal, data = irisdata, kernel = "linear",
             cost = "10", scale = FALSE)
# cost = 1
fit_1 = svm(species ~ sepal + petal, data = irisdata, kernel = "linear",
            cost = "1", scale = FALSE)
# cost = 0.1
fit_0.1 = svm(species ~ sepal + petal, data = irisdata, kernel = "linear",
              cost = "0.1", scale = FALSE)

# Plotting the outcomes 
plot(fit_10, data = irisdata)
plot(fit_1, data = irisdata)
plot(fit_0.1, data = irisdata)

# What differences do you see, and how are they related to the cost values?
# The main difference is in the support vectors, and hence, in the margin classifier
# The higher the costs, the smaller the support vectors


### 2. For cost = 0.1 list the support vectors
support_vectors_0.1 = fit_0.1$SV


### 3. For cost = 0.1 find the equation of the separating line
coefs_0.1 = fit_0.1$coefs[1:2]
# Should be like this
# coefs_0.1%*%fit_0.1$SV


### 4. Now consider data for versicolor and virginica, run svm with cost = 1
irisdata_2 <- iris[(51:150), c(1, 3, 5)]
names(irisdata_2) <- c("sepal", "petal", "species")
# fitting the model
fit_2_1 = svm(species ~ sepal + petal, data = irisdata_2, kernel = "linear",
              cost = "1", scale = FALSE)
# how many support vectors do you get?
support_vectors_2_1 = fit_2_1$SV
# how do they compare to the results of part 1?
support_vectors_1 = fit_1$SV
# For versicolor and virginica we have a much larger number of vectors for the same cost
# Can you explain this beahviour with respect to the different data sets (and the position of their points?)
plot(fit_2_1, data = irisdata_2)
# versicolor and virginica are much closer to each other, while setosa and versicolor are much further away,
# we need less vectors to create a good separating line when the points are by nature further away.



##### Exercise 2 ######

# set up
data = data.frame("x" = c(-1, 1, 0.5, 1, 3, 3.5, 3.5, 4),
                  "y" = c(2, 1, 3, 4, 2, 1, 4, 2),
                  "class" = c(1, 1, 1, 1, -1, -1, -1, -1))

### 1. Plot the hyperplane and the margin. What are the support vectors and how large is the margin
ggplot(data, aes(x,y)) + geom_point() + geom_vline(xintercept = 2, col = "red") +
  geom_vline(xintercept = 1) + geom_vline(xintercept = 3)
# the support vectors are x = 1 and x = 3, the margin is 1

### 2. If we leave the separating hyperplane as it is, but increase the margin to 1.5
ggplot(data, aes(x,y)) + geom_point() + geom_vline(xintercept = 2, col = "red") +
  geom_vline(xintercept = 0.5) + geom_vline(xintercept = 3.5)
# the support vectors are x = 0.5 and x = 3.5, the margin is 1.5



##### Exercise 3 #####
# using different software 
