################################################################################

##### MACHINE LEARNING HOMEWORK 6

################################################################################

### SET UP

#loading data
load("C:/Users/magag/Documents/UNIVERSITA/_St.GALLEN - Master in Quant Economics and Finance/Subjects/2^ Semester/Machine Learning/HWs/wdbcData.Rdata")
# setting the seed
set.seed(123)
# sampling data
Data = Data[sample(nrow(Data)),]
# train data set
ntrain = floor(nrow(Data)*0.7)
Data.train = Data[1:ntrain,]
Data.test = Data[(ntrain+1):nrow(Data),]


###### Exercise 1 #####

### 1. Run a logistic regression on the training data with respect to perimeter_mean and concaveP_mean
log_regr_1 = glm(diagnosis ~ perimeter_mean + concaveP_mean, family = binomial, data = Data.train)
summary(log_regr_1)


### 2. Get a list of prediction and interpret the data
# making predictions
pred_1 = data.frame(predict(log_regr_1, type = "response"))
colnames(pred_1) = "Predictions"
# how do you interpret these as probabilities?
# these numbers are the probabilities of the cancer being what?
# probabilities of the cancer to be malign
# the higher the value is, the higher the probability of having a malign cancer.


### 3. Predict the actual class label (B or M), based on whether the probability is greater or equal to 50%
# selecting all the values that are larger than 0.5
malign = data.frame(pred_1 >= 0.5)
# if larger than 0.5, M, otherwise B
malign$Predictions[malign$Predictions == "TRUE"] = "M"
malign$Predictions[malign$Predictions == "FALSE"] = "B"
colnames(malign) = "class_predictions"
# extracting the diagnosis from the train set
diagnosis = data.frame(Data.train[,"diagnosis"])
colnames(diagnosis) = "class_diagnosis"
# creating data frame with the predictions and the actual 
predictions = data.frame(c(pred_1, malign, diagnosis))


### 4. Compute the number of misclassifications, as well as the number of false positives and false negatives
# misclassification ratio
misclassification_rate = mean(predictions$class_predictions != predictions$class_diagnosis)
# misclassification number
misclassification_number = misclassification_rate * 398
# false positives and false negatives
table =  data.frame(table(predictions$class_predictions, predictions$class_diagnosis))
colnames(table) = c("predictions", "diagnosis", 'number')
# false positives
false_positives = 10
# false negatives
false_negatives = 17
# summary
summary = data.frame(misclassification_number, false_positives, false_negatives)


##### Exercise 2 #####

### 1. Summarize what we are predicting based on which data in this example (from the book)
# In this example, we are trying to predict the direction of the market (whether it goes up or down)
# Based on the percentage of the returns in the previous 5 days and the volume traded in billion $.
# The method we used is the logistic regression.


### 2. What is the outcome, with 2 or 6 features, respectively?
### How does our prediction compare to predicting that the market goes up all time?
# 6 Factors:
# Error rate for the model with 6 factors = 0.52, worse than guessing.
# Only 48% of the daily movements have been correctly predicted.
# 2 Factors:
# Error rate for the model with 2 factors = 0.44, better than guessing.
# 56% of the daily movements have been correctly predicted.
# Comparing to predicting that the market only goes up:
# Assuming that the probability of going randomly up is 50%, we would get an error rate of 0.5
# Our predictions are better only with the 2 factor model, the 6 factor model performs worse.


### 3. Run logistic regression with Lag1 as input variable, use data from 2001-2004 as train and 2005 test.
# SET UP
library("ISLR")
data_market = Smarket
########## I have a doubt, shall we set the seed?
# setting train and test
train_market = data_market[1:998,]
test_market = data_market[999:nrow(data_market),]
# running the regression only with Lag1
log_regr_2 = glm(Direction ~ Lag1, data = train_market, family = binomial)
summary(log_regr_2)
# making predictions
pred_2 = data.frame(predict(log_regr_2, type = "response"))
colnames(pred_2) = "Predictions"
# selecting all the values that are larger than 0.5
up = data.frame(pred_2 >= 0.5)
# if larger than 0.5, M, otherwise B
up$Predictions[up$Predictions == "TRUE"] = "Up"
up$Predictions[up$Predictions == "FALSE"] = "Down"
colnames(up) = "class_predictions"
# extracting the diagnosis from the train set
direction = data.frame(train_market[,"Direction"])
colnames(direction) = "class_directions"
# creating data frame with the predictions and the actual 
predictions_2 = data.frame(c(pred_2, up, direction))
# accuracy rate:
accuracy_rate = mean(predictions_2$class_predictions == predictions_2$class_directions)
print(accuracy_rate)
# The accuracy rate is approximately 0.534. This is greater than the model with 6 features,
# but smaller than the model with 2 features.
# Still it is better than randomly picking with probability of 0.5




