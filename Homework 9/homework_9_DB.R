# Homework 9
# Machine Learning

library(MASS)
library(tree)
Data = Boston
set.seed(123)
Data = Data[sample(nrow(Data)),]

# Complete Tree ----------------------------------------------------------------
tree.comp = tree(medv~.,Data)
summary(tree.comp)
plot(tree.comp)
text(tree.comp)

# Training Set Tree ------------------------------------------------------------
ntrain = floor(nrow(Data)*0.7)
Data.Train = Data[1:ntrain,]
Data.Test = Data[(ntrain+1):nrow(Data),]

tree.boston = tree(medv~.,data=Data.Train)
summary(tree.boston)
plot(tree.boston)
text(tree.boston)

# Question 2 -------------------------------------------------------------------

# Complete: We get 9 terminal nodes with the following features: rm, lstat, dis,
# crim, ptratio.

# Training Set: We get 8 terminal nodes with the following features: lstat, rm,
# tax, dis.


# Question 3 -------------------------------------------------------------------
y_hat1 = predict(tree.boston, newdata = Data.Test)

RSS1 = (Data.Test$medv - y_hat1)%*%(Data.Test$medv - y_hat1)
RSS1

# Question 4 -------------------------------------------------------------------
prune_train = prune.tree(tree.boston,best=5)
summary(prune_train)
plot(prune_train)
text(prune_train)

# lstat, rm and dis were used as predictors. (one less than the original)


# Question 5 -------------------------------------------------------------------
y_hat2 = predict(prune_train, newdata = Data.Test)

RSS2 = (Data.Test$medv - y_hat2)%*%(Data.Test$medv - y_hat2)
RSS2

# Question 6 -------------------------------------------------------------------
library(randomForest)

rf = randomForest(medv~., data=Data.Train)

# R used 500 trees by default
# 4 variables were chosen at random for each split
# MSE = 11.37757

# Run the command again. Why do you get a slightly different result?
# Every time we run the command we get the mse of that specific random forest.
# Each tree is built on a random part of the data (bootsrap) and therefore the
# result changes every time.

# Question 7 -------------------------------------------------------------------
y_hat3 = predict(rf, newdata = Data.Test)

RSS3 = (Data.Test$medv - y_hat3)%*%(Data.Test$medv - y_hat3)

cat("RSS 1 = ", RSS1)
cat("RSS 2 = ", RSS2)
cat("RSS 3 = ", RSS3)

# The normal tree has less variation than the pruned tree and the random forest
# has the least variation of them all. It makes sense that the random forest
# would have the smallest RSS as the model can learn from many different trees,
# instead of one.
# The normal tree has a smaller RSS than the pruned tree. We prune trees to reduce
# the risk of overfitting. A possible explanation for why the normal tree has
# a smaller RSS is that it was overfitting. 






