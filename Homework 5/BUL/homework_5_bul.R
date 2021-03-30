set.seed(777)
s = seq(from = -5, to = 5, by = 0.01)
x1 = sample(s,200, replace = TRUE)
x2 = sample(s,200, replace = TRUE)


class = data.frame(x1,x2)

class_1 = subset(class , (class$x1 < -4) & (class$x2 < -4))
class_1$y = 1
class_2 = subset(class , (class$x1 > -4) & (class$x1 < -2) & (class$x2 > -4) & (class$x2 < -2))
class_2$y = 2
class_3 = subset(class , (class$x1 > -2) & (class$x1 < 0) & (class$x2 > -2) & (class$x2 < 0))
class_3$y = 3
class_4 = subset(class , (class$x1 > 0) & (class$x1 < 2) & (class$x2 > 0) & (class$x2 < 2))
class_4$y = 4
class_5 = subset(class , (class$x1 > 2) & (class$x1 < 5) & (class$x2 > 2) & (class$x2 < 5))
class_5$y = 5

data = rbind(class_1,class_2,class_3,class_4,class_5)











# Set parameters and Prepare data
#################################


# Parameters

eta = 0.1

decCrit = -5

seedVal = 12


Data = data


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

Data$x1 = standFun(Data$x1)
Data$x2 = standFun(Data$x2)



#################################
# Perceptron Learning Algorithm #
#################################

# Copying data to new object
D = Data
n = nrow(Data)


# Initializations
#################

# The initial weights, initialized to 0 (= completely ignorant)
w = rep(0, 2 + 1)





# ITERATIVE LEARNING STARTS HERE
################################

# NOTE: i is both the running index for the iteration and
# also the index for cases/observations

for (i in 1:n){
  
  xi = as.numeric(D[i,1:2])  
  # The x data from obs i, note that this is a vector with two elements
  
  yi = D$y[i]                      
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

D$lastIndex = w[1] + w[2]*D$x1 + w[3]*D$x2

D$lastPrediction = ifelse(D$lastIndex >=decCrit, 1, -1)

D$lastError  = D$y - D$lastPrediction


# Number of misclassified cases (in % of total sample)
misclas = round(sum(D$lastError!=0)/nrow(D)*100, digits = 1)

# False positives (in % of negatives)
x = ifelse(D$lastPrediction == 1 & D$y == -1, 1, 0)
falPos = round(sum(x)/length(D$y[D$y == -1])*100, digits = 1)

# False negatives (in % of positives)
x = ifelse(D$lastPrediction == -1 & D$y == 1, 1, 0)
falNeg = round(sum(x)/length(D$y[D$y == 1])*100, digits = 1)



# Plotting Section (less interesting for you) -----------------------------
###########################################################################

# Colors for plotting

posCol = "red"     # color for data points with label +1 
negCol = "blue"    # color for data points with label -1 in data
posColbg = "#f79999"  # Background color for area predicted +1
negColbg = "#9ed9f7"  # Background color for area predicted -1


# First an empty plot as "canvas", to add the rest
plot(Data$x1, Data$x2, type = "n",
     xlab = "Attribute 1", ylab = "Attribute 2", cex = 1,
     main = paste0("Linear Seperation"),
     cex.main = 1.1)



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

dataptsize = 0.7
bgptsize = 0.5

toPaint$index = w[1] + w[2]*xgrid + w[3]*ygrid
toPaint$prediction = ifelse(toPaint$index >=decCrit, 1, -1 )


# First, add packground coloring for areas that are predicted to be "positive"
points(toPaint$xgrid[toPaint$prediction == 1], toPaint$ygrid[toPaint$prediction == 1],
       pch=c(16), col=posColbg, cex =bgptsize)

# Add packground coloring for areas that are predicted to be "negative"
points(toPaint$xgrid[toPaint$prediction == -1], toPaint$ygrid[toPaint$prediction == -1],
       pch=c(16), col=negColbg, cex =bgptsize)

# The "positive" data points
points(Data$x1[Data$y == 1], Data$x2[Data$y == 1], 
       pch=16, col=posCol, cex = dataptsize)

# The "negative" data points
points(Data$x1[Data$y == -1], Data$x2[Data$y == -1], 
       pch=16, col=negCol,  cex = dataptsize)




# The decision border
xx = seq(xlims[1], xlims[2], 0.01)
yy = decCrit/w[3] - w[1]/w[3] - w[2]/w[3]*xx
lines(xx,yy, lty = 2, col = "black", lwd = 2)


mtext(line = -1.5, 
      paste0(misclas, "\u0025 misspecified cases"), col = "white")
mtext(line = -3, 
      paste0(falPos,"\u0025 false positives -- ",
             falNeg,"\u0025 false negatives"), 
      col = "white")

