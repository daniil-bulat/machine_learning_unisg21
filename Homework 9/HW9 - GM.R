################################################################################

###########       ML HW9 - GIOVANNI MAGAGNIN       #############################

################################################################################

# pacakages 
library(tree)
library(MASS)
library(rpart)
library(randomForest)

# set up
Data = Boston
set.seed(123)
Data =Data[sample(nrow(Data)),]
ntrain = floor(nrow(Data)*0.7)
Data.Train = Data[1:ntrain,]
Data.Test = Data[(ntrain+1):nrow(Data),]

##### Exercise 1 ######


### 1. Constructing a tree for the given complete data, plot it with the plot and text command

# Constructing the tree
tree_1 = tree(medv ~., data = Data.Train)
tree_1

# Plotting with the plot and text command
plot(tree_1)
text(tree_1)


### 2. How many terminal nodes do you get? Which of the input variables have been used when building the tree?
summary_tree = summary(tree_1)

# Terminal nodes
nodes = summary_tree$size
nodes

# Input variables
variables = summary_tree$used
variables


### 3. Use the predict command to do the predictions on the test set, and compute the RSS

# Predictions
pred_1 = predict(tree_1, newdata = Data.Test)
pred_1

# RSS
rss_1 = (Data.Test$medv - pred_1)%*%(Data.Test$medv - pred_1)
rss_1


### 4. Prune the tree and plot it. What predictors have been used?

# Prune
tree_2 = prune.tree(tree_1, best = 5)

# Plotting
plot(tree_2)
text(tree_2)

# Predictors that have been used
summary_2 = summary(tree_2)
variables_2 = summary_2$used
variables_2


### 5. Predict and compute the RSS for this smaller tree

# Predictions
pred_2 = predict(tree_2, newdata = Data.Test)
pred_2

# RSS
rss_2 = (Data.Test$medv - pred_2)%*%(Data.Test$medv - pred_2)
rss_2


### 6. Run the randomForest command, how many trees, how many variablse, MSE, why do we get different results?

# Running the tree
tree_3 = randomForest(medv ~., data = Data.Train)
tree_3

# How many trees
500

# How many variables
4

# MSE
11.378

# Run again, why different results?
randomForest(medv ~., data = Data.Train)
# due to the randomness


### 7. Predict and compute the RSS

# predict
pred_3 = predict(tree_3, newdata = Data.Test)
pred_3

# RSS
rss_3 = (Data.Test$medv - pred_3)%*%(Data.Test$medv - pred_3)
rss_3
