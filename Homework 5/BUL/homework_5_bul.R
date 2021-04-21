rm(list = ls())
mainDir = "/Users/danielbulat/Desktop/Uni/FS21/Machine Learning/ML_Git/Homework 5/Course Resources"
setwd(mainDir)

# Parameters

fracTest = 0.3

decCrit = 1

eta = 0.01

seedVal = 7

# Load Data
library(datasets)
library(dplyr)

data(iris)

iris = iris[,3:5] # remove unnecessary features

datasz = iris





# drop third species
species = c("setosa", "versicolor", "virginica")

Data = filter(datasz, Species != "versicolor")



# Assign numerical value to labels
Data$Label = ifelse(Data$Species == "setosa", -1, 1)

# Give generic name to features
features = c(names(Data)[1], names(Data)[2])


# reshuffle dataset; data set may be ordered by target. 
# This would make learning extremely slow
# Change the see seed value (seedVal) if you want to get a 
# different order of the cases

set.seed(seedVal)
resh = sample(1:nrow(Data))
Data = Data[resh,]

# Standardize data, 

standFun = function(x){
  out = (x - mean(x))/sd(x)
  return(out)
}

Data[[features[1]]] = standFun(Data[[features[1]]])
Data[[features[2]]] = standFun(Data[[features[2]]])



#################################
# Perceptron Learning Algorithm #
#################################

# Copying data to new object
D = Data
n = nrow(Data)


# Initializations
#################

# The initial weights, initialized to 0 (= completely ignorant)
w = rep(0, length(features) + 1)





# ITERATIVE LEARNING STARTS HERE
################################

# NOTE: i is both the running index for the iteration and
# also the index for cases/observations

for (i in 1:n){
  
  xi = as.numeric(D[features][i, ])  
  # The x data from obs i, note that this is a vector with two elements
  
  yi = D$Label[i]                      
  # The y value from obs i
  
  index = w[1] + w[2]*xi[1] + w[3]*xi[2]
  # The index, dont'get confused about the weird indexing, R has no index 0.
  
  # Prediction, as a function of the index value
  # NOTE the value decCrit! Not necessarily equal to 0!
  pred = ifelse(index>=decCrit, 1, -1)  
  
  # bookkeeping: Enter current values of index, prediction, and weights
  # into the D data frame (this is not really necessary, 
  # just in case we want the information)
  
  D$index_i[i] = index
  D$prediction_i[i] = pred
  D$bias_i[i] = w[1]
  D$w_SLength_i[i] = w[2]
  D$w_PLength_i[i] = w[3]
  
  
  
  # THE FOLLOWING LINES ARE THE ENGINE OF THE WHOLE THING !!!
  # THIS IS WHERE THE LEARNING IS GOING ON!
  
  update = eta*(yi - pred)      # The updating factor
  w[-1] = w[-1] + update * xi   # updating the weights for the two features
  w[1] = w[1] + update          # updating the constant/bias
  
  
  # Again for bookkeeping (pretty self-explaining)
  
  error  = yi - pred  
  D$error_i[i] = error
  D$update_i[i] = update
  
  
}

# Now we calculate predictions for all i based on the LAST ITERATION
# for the weights

D$lastIndex = w[1] + w[2]*D[[features[1]]] + w[3]*D[[features[2]]]

D$lastPrediction = ifelse(D$lastIndex >=decCrit, 1, -1)

D$lastError  = D$Label - D$lastPrediction



# Plotting Section (less interesting for you) -----------------------------
###########################################################################

# Colors for plotting

posCol = "red"     # color for data points with label +1 
negCol = "blue"    # color for data points with label -1 in data

# First an empty plot as "canvas", to add the rest
plot(Data[[features[1]]], Data[[features[2]]], type = "n",
     xlab = "Sepal Length", ylab = "Sepal Width", cex = 1,
     main = paste0("Multi-class Prediction"),
     cex.main = 1.1)



#Get xlims and ylims from the empty plot

xlims = par("usr")[1:2]; ylims = par("usr")[3:4]

# The "positive" data points
points(Data[[features[1]]][Data$Label == 1], Data[[features[2]]][Data$Label == 1], 
       pch=16, col=posCol, cex = dataptsize)

# The "negative" data points
points(Data[[features[1]]][Data$Label == -1], Data[[features[2]]][Data$Label == -1], 
       pch=16, col=negCol,  cex = dataptsize)



# The decision border
xx = seq(xlims[1], xlims[2], 0.01)
yy = decCrit/w[3] - w[1]/w[3] - w[2]/w[3]*xx
lines(xx,yy, lty = 2, col = "black", lwd = 2)

