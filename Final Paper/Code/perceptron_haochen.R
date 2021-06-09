# Header
########

## PLEASE ADJUST YOUR WORKING DIRECTORY
rm(list = ls())
mainDir = "C:/Labs/Machine Learning/Final Paper/Code"
setwd(mainDir)


# Set parameters and Prepare data
#################################


# Parameters

fracTest = 0.3

decCrit = 0

eta = 0.01

seedVal = 1

# The data (Make sure they are in the proper working directory)

Data = read.csv("C:/Labs/Machine Learning/Final Paper/Data/analysis data/combined_data_5_features.csv")

Data$Date = as.Date(Data$Date)
Data = Data[,-(1:4)]



# Standardize Features
standFun = function(x){
  out = (x - mean(x))/sd(x)
  return(out)
}

features = names(Data)[2:6]

# Keep only the columns in the data frame that we need for the learning task
keepList = c(features, "direction")

Data = Data[keepList]

for (i in 1:length(features)){
  Data[[features[i]]] = standFun(Data[[features[i]]])
}


# Split the data into training and test
# Shuffle and Split Data (70-30)
set.seed(123)
Data = Data[sample(nrow(Data)),]
ntrain = floor(nrow(Data)*0.7)
Data.Train = Data[1:ntrain,]
Data.Test = Data[(ntrain+1):nrow(Data),]


#################################
# Perceptron learning: TRAINING #
#################################

# Copying Data.Train to new object where all the learning 
# iterations are calculated. Only the results from the last 
# iteration are then added to Data.Train
D = Data.Train
n = nrow(Data.Train)

# Initializations
#################

# The initial weights, initialized to 0 (= completely ignorant)
w = rep(0, length(features) + 1)


# ITERATIVE LEARNING STARTS HERE
################################


for (i in 1:n){
  
  xi = as.numeric(D[features][i, ])  
  # The x data from obs i, note that this is a vector with two elements
  
  yi = D$direction[i]                      
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
  
  update = eta*(yi - index)      # The updating factor
  w[-1] = w[-1] + update * xi   # updating the weights for the two features
  w[1] = w[1] + update          # updating the constant/bias
  
  
  # Again for bookkeeping (pretty self-explaining)
  
  error  = yi - index
  D$error_i[i] = error
  D$update_i[i] = update
  
  
  
}




# Errors in TRAINING data
#########################

# Now we calculate predictions and errors for all i based on the 
# LAST TRAINING ITERATION for the weights for the TRAINING DATA

Data.Train$lastIndex = w[1] + 
  w[2]*Data.Train[[features[1]]] + 
  w[3]*Data.Train[[features[2]]]  +
  w[4]*Data.Train[[features[3]]] # +
  # w[5]*Data.Train[[features[4]]] +
  # w[6]*Data.Train[[features[5]]] +
  # w[7]*Data.Train[[features[6]]] +
  # w[8]*Data.Train[[features[7]]] +
  # w[9]*Data.Train[[features[8]]] +
  # w[10]*Data.Train[[features[9]]] +
  # w[11]*Data.Train[[features[10]]] 

Data.Train$lastPrediction = ifelse(Data.Train$lastIndex >=decCrit, 1, -1)

Data.Train$lastError  = Data.Train$direction - Data.Train$lastPrediction

# Number of misclassified cases (in % of total sample)
misclasTrain = round(sum(Data.Train$lastError!=0)/nrow(Data.Train)*100, digits = 1)

# False positives (in % of negatives)
x = ifelse(Data.Train$lastPrediction == 1 & Data.Train$direction == -1, 1, 0)
falPosTrain = round(sum(x)/length(Data.Train$direction[Data.Train$direction == -1])*100, digits = 1)

# False negatives (in % of positives)
x = ifelse(Data.Train$lastPrediction == -1 & Data.Train$direction == 1, 1, 0)
falNegTrain = round(sum(x)/length(Data.Train$direction[Data.Train$direction == 1])*100, digits = 1)


# Errors in TEST data
#####################


# And now we calculate predictions and errors for all i based on the 
# LAST TRAINING ITERATION for the weights for the TEST DATA


Data.Test$lastIndex = w[1] + 
  w[2]*Data.Test[[features[1]]] + 
  w[3]*Data.Test[[features[2]]] +
  w[4]*Data.Test[[features[3]]] # +
  # w[5]*Data.Test[[features[4]]] +
  # w[6]*Data.Test[[features[5]]] +
  # w[7]*Data.Test[[features[6]]] +
  # w[8]*Data.Test[[features[7]]] +
  # w[9]*Data.Test[[features[8]]] +
  # w[10]*Data.Test[[features[9]]] +
  # w[11]*Data.Test[[features[10]]] 

Data.Test$lastPrediction = ifelse(Data.Test$lastIndex >=decCrit, 1, -1)

Data.Test$lastError  = Data.Test$direction - Data.Test$lastPrediction

# Number of misclassified cases (in % of total sample)
misclasTest = round(sum(Data.Test$lastError!=0)/nrow(Data.Test)*100, digits = 1)

# False positives (in % of negatives)
x = ifelse(Data.Test$lastPrediction == 1 & Data.Test$direction == -1, 1, 0)
falPosTest = round(sum(x)/length(Data.Test$direction[Data.Test$direction == -1])*100, digits = 1)

# False negatives (in % of positives)
x = ifelse(Data.Test$lastPrediction == -1 & Data.Test$direction == 1, 1, 0)
falNegTest = round(sum(x)/length(Data.Test$direction[Data.Test$direction == 1])*100, digits = 1)

# accuracy
mean(Data.Test$direction == Data.Test$lastPrediction)

mean(Data.Train$direction == Data.Train$lastPrediction)



# Plotting Section (less interesting for you) -----------------------------
###########################################################################


# Colors for plotting

posCol = "red"     # color for data points with label +1 
negCol = "blue"    # color for data points with label -1 in data
posColbg = "#f79999"  # Background color for area predicted +1
negColbg = "#9ed9f7"  # Background color for area predicted -1


# Parameters for arranging to plots vertically on top of each other
# Also setting proper margins, so we can add titles to each
# individual plot without this being overlapped by and axis label
# or whatever. All this is pretty nitty-gritty...
# No need to study this in detail.

# Type "?par" in the console to get more information about all this

par(mfrow = c(2,1)) 
par(tcl = -0.25)          # length of ticks on axes
par(mgp=c(2, 0.5, 0))     
par(oma = c(1,1,1,6.5) )   # outer margin, default is c(0,0,0,0)
par(mar = c(3, 3, 3, 0) )  # inner margin, default c(5.1, 4.1, 4.1, 2.1)
par(xpd = NA)              # make outer margin usable



dataptsize = 0.7  # Size of data points in scatterplots
bgptsize = 0.5    # Size of dots for background

subtsize = 1           # Size of titles of single plots
subtcol = "#888888"    # Color of titles of single plots


# First an empty plot as "canvas", to add the rest
plot(Data.Train[[features[1]]], Data.Train[[features[2]]], type = "n",
     xlab = features[1], ylab = features[2], cex = 1,
     main = paste0("Training sample (", (1-fracTest)*100, "\u0025 of data)"),
     cex.main = subtsize, col.main = subtcol)


#Get xlims and ylims from the empty plot

xlims = par("usr")[1:2]; ylims = par("usr")[3:4]

# A grid of (x,y) combinations that is used for coloring prediction areas
x = seq(xlims[1], xlims[2], 0.01)
y = -w[1]/w[3] - w[2]/w[3]*x

xgrid =  seq(xlims[1], xlims[2], (xlims[2]-xlims[1])/300)
ygrid =  seq(ylims[1], ylims[2], (ylims[2]-ylims[1])/300)
nx = length(xgrid); ny = length(ygrid)

xgrid = rep(xgrid, times = ny)
ygrid = rep(ygrid, each = nx)
toPaint = data.frame(xgrid = xgrid, ygrid = ygrid)
# toPaint is the data frame that is used for coloring the prediction areas


# Put background colors for prediction areas onto canvas


toPaint$index = w[1] + w[2]*xgrid + w[3]*ygrid
toPaint$prediction = ifelse(toPaint$index >=decCrit, 1, -1 )


# First, add packground coloring for areas that are predicted to be "positive"
points(toPaint$xgrid[toPaint$prediction == 1], toPaint$ygrid[toPaint$prediction == 1],
       pch=c(16), col=posColbg, cex =bgptsize)

# Add packground coloring for areas that are predicted to be "negative"
points(toPaint$xgrid[toPaint$prediction == -1], toPaint$ygrid[toPaint$prediction == -1],
       pch=c(16), col=negColbg, cex =bgptsize)

# The "positive" data points
points(Data.Train[[features[1]]][Data.Train$direction == 1], Data.Train[[features[2]]][Data.Train$direction == 1], 
       pch=16, col=posCol, cex = dataptsize)

# The "negative" data points
points(Data.Train[[features[1]]][Data.Train$direction == -1], Data.Train[[features[2]]][Data.Train$direction == -1], 
       pch=16, col=negCol,  cex = dataptsize)




# The decision border
xx = seq(xlims[1], xlims[2], 0.01)

yy = decCrit/w[3] - w[1]/w[3] - w[2]/w[3]*xx
yy[yy<ylims[1] | yy>ylims[2]] = NA


lines(xx,yy, lty = 2, col = "black", lwd = 2)


mtext(line = -1, 
      paste0(misclasTrain, "\u0025 misspecified cases"), col = "white")
mtext(line = -2, 
      paste0(falPosTrain,"\u0025 false positives -- ",
             falNegTrain,"\u0025 false negatives"), 
      col = "white")




######

# Again an empty plot as "canvas"
# Make sure that the axis ranges are the same as for the first plot, otherwise
# it will look ugly. Therefore, for the empty canvas plot, take the variables
# from the TRAINING data frame, NOT the test!!!
plot(Data.Train[[features[1]]], Data.Train[[features[2]]], type = "n",
     xlab = features[1], ylab = features[2], cex = 1,
     main = paste0("Testing sample (", fracTest*100, "\u0025 of data)"),
     cex.main = subtsize, col.main = subtcol)


# First, add packground coloring for areas that are predicted to be "positive"
points(toPaint$xgrid[toPaint$prediction == 1], toPaint$ygrid[toPaint$prediction == 1],
       pch=c(16), col=posColbg, cex =bgptsize)

# Add packground coloring for areas that are predicted to be "negative"
points(toPaint$xgrid[toPaint$prediction == -1], toPaint$ygrid[toPaint$prediction == -1],
       pch=c(16), col=negColbg, cex =bgptsize)

# The "positive" data points
points(Data.Test[[features[1]]][Data.Test$direction == 1], Data.Test[[features[2]]][Data.Test$direction == 1], 
       pch=16, col=posCol, cex = dataptsize)

# The "negative" data points
points(Data.Test[[features[1]]][Data.Test$direction == -1], Data.Test[[features[2]]][Data.Test$direction == -1], 
       pch=16, col=negCol,  cex = dataptsize)




# The decision border
xx = seq(xlims[1], xlims[2], 0.01)
yy = decCrit/w[3] - w[1]/w[3] - w[2]/w[3]*xx
yy[yy<ylims[1] | yy>ylims[2]] = NA
lines(xx,yy, lty = 2, col = "black", lwd = 2)


mtext(line = -1, 
      paste0(misclasTest, "\u0025 misspecified cases"), col = "white")
mtext(line = -2, 
      paste0(falPosTest,"\u0025 false positives -- ",
             falNegTest,"\u0025 false negatives"), 
      col = "white")

title(main = paste0("Perceptron learning (decision threshold of ", decCrit, ")"),
      col.main ="#7a1818", 
      outer = TRUE,
      line = 0)


