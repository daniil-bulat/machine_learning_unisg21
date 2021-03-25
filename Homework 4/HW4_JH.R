
################################################################################
## Filename: Machine Learning - Homework 3
## Topic: Homework 3
## Author: Jonas Huwyler...
## Matriculation Number: 16-610-958...
################################################################################

# removing all variables/functions
rm(list = ls())


# 1. ---------------------------------------------------------------------------

# a)
# define variables
w0 <- 0.5
w1 <- 1
w2 <- -1.5
vec <- c(w1, w2)
x1 <- seq(-2, 2, 0.01)

# model
x2 <- -(w0/w2) - (w1/w2)*x1

# draw the plot
plot(x1, x2, type = "l", main = "Plotting x2 against x1", 
     xlim = c(-2, 2), ylim = c(-2, 2)) 
arrows(-vec[1], -vec[2], vec[1], vec[2])

# plot a grid for better visuals
grid(nx = NULL, ny = NULL, col = "lightgray", lty = "dotted",
     lwd = par("lwd"), equilogs = TRUE)


# b)
# the intercept of the line shifts upwards while everything else (steepness etc.) 
# stays the same


# c) 
# the line gets steeper while everything else (intercept etc.) stays the same


# d)
# the intercept and the steepness of the line changes. both decreases as we divide
# by w2


# 2. ---------------------------------------------------------------------------

# a)
# A false positive in that case is to predict a cancer to be malignant (positive) 
# but it is really a benign cancer (negative). This means that we diagnosed a 
# patient to have a "good" cancer but she actually has a "bad" cancer. This is 
# fatal. The malagnin (positive) outcome takes the value 1. The begnin (negative) 
# outcome takes the value -1.


# b )
# I would probably minimize the probability of a false positive. If a patient has 
# cancer but is not diagnosed with it, this is in most cases fatal. I consider a 
# person being diagnosed with cancer despite  not having one also everything but 
# good because you maybe start taking medicine you would not have to YET this will 
# not end fatal!


# c) 
# When we increase the decCrit value, the boundary to classify a value as 1 (malgning),
# gets higher. So we would need very strong evidence to classify a cancer as malagning.
# A diagnosis of begning cancer, on the other hand, is more easily to happen since
# the boundary to classify a cancer as -1 (begning) is quite low. It is more likely 
# to get a prediction lower than the decCrit value if the decCrit value is high.
# Hence, a higher decCrit value will make it less likely to have a false positive 
# and vice versa.


# d)
# As already discussed, we value false negatives as less severe since in the worst case,
# a patient starts a cancer treatment without having cancer. Whereas we strongly want 
# to avoid making the mistake of diagnosing a begnin cancer as malagning (false positive)
# because that would imply that a patient would miss out to start a treatment. Therefore, 
# we want to strongly reduce the risk of having a false positive In that case we choose 
# a high decCrit value. The value is at -5 but we believe that it can be set a bit higher
# for example at +5. The chances of having a false positive would then be around 0.5%. 


# 3. ---------------------------------------------------------------------------

# take vectors
x1 <- c(13, -21, 7, 11)
x2 <- c(-2, -1.2, 2, 0.5)
x3 <- c(-1.3, 2.4, 2, -1)

# calculate standard deviation and mean
sd1 <- sd(x1)
me1 <- mean(x1)
sd2 <- sd(x2)
me2 <- mean(x2)
sd3 <- sd(x3)
me3 <- mean(x3)

# normalise numbers
norm_x1 <- (x1 - me1) / sd1
norm_x2 <- (x2 - me2) / sd2
norm_x3 <- (x3 - me3) / sd3

# prepare vectors for dataframe
variable <- c("x1", "x2", "x3")
D1 <- c(norm_x1[1], norm_x2[1], norm_x3[1])
D2 <- c(norm_x1[2], norm_x2[2], norm_x3[2])
D3 <- c(norm_x1[3], norm_x2[3], norm_x3[3])
D4 <- c(norm_x1[4], norm_x2[4], norm_x3[4])

# create dataframe
D <- data.frame(variable, D1, D2, D3, D4)
D



