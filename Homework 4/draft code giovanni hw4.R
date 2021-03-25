###############################################################################

##### MACHINE LEARNING HOMEWORK4 ##############################################

###############################################################################



##### Exercise 1 ##### 

# Set up
x1 = seq(-10, 10, by = 0.1)
w0 = 5
w1 = 10
w2 = -2
x2 = -(w0/w2) -(w1/w2)*x1
nv1 = x1
nv2 = (w2/w1)*nv1

### 1. Drawing a graph for this setting
df = data.frame(x1, x2, nv1, nv2)
plot(df$x1, df$x2, type = 'l')
lines(df$nv1, df$nv2, lty = 2)

### 2. What happens to the line if we increase w0 while leaving the rest unchanged?
w0.2 = 30
x2.2 = -(w0.2/w2) -(w1/w2)*x1

plot(df$x1, df$x2, type = 'l')
lines(df$nv1, df$nv2)
lines(x1, x2.2, col = 'blue')
# the x2 line shifted left in the case that w0 increases

### 3. what happens to the line if we increase w1 and we leave the rest unchanged?
w1.3 = 30
x2.3 = -(w0/w2) -(w1.3/w2)*x1
nv2.3 = (w2/w1.3)*nv1

plot(df$x1, df$x2, type = 'l')
lines(df$nv1, df$nv2)
lines(x1, x2.2, col = 'blue')
lines(x1, x2.3, col = 'red')
lines(df$nv1, nv2.3, col = 'red')
# the x2 line tilted, the normal vectors as well. Also the intersection changes, x1 at the intercept is larger now

### 4. what happens to the line if we increase w2 and leave all the rest unchanged?
w2.4 = -1
x2.4 = -(w0/w2) -(w1/w2.4)*x1
nv2.4 = (w2.4/w1)*nv1

plot(df$x1, df$x2, type = 'l')
lines(df$nv1, df$nv2)
lines(x1, x2.2, col = 'blue')
lines(x1, x2.3, col = 'red')
lines(df$nv1, nv2.3, col = 'red')
lines(x1, x2.4, col = 'purple')
lines(df$nv1, nv2.4, col = 'purple')
# the x2 line tilted, the normal vector as well, similarly to what happened when the w1 increased


##### Exercise 2 #####

### 1. what is a false positive in this setting?
# A false positive in this setting is a patient to whom it gets diagnosed a 
# malign breast cancer tumor while it is benign instead
# I am not sure what is meant by which numerical values does it take on in the algorithm
# In the example from the professor, with critical index value of -5, 
# the percentage of false positives in terms of negatives is 3.9%

### 2. What error would you minimize?
# I would minimize the false negative errors.
# I believe is more important to be sure that the malign is diagnosed precisely
# and I would trade off some accuracy in the false positives.
# The utility i get from diagnosing precisely the malign cases is larger.
# By decreasing the number of false negatives, we increase the possibility of saving more lives
# since malign cancer is considerably more dangerous than benign.
# The downsides of having more patients doing wrong treatments in the case they are benign
# are not as bad as having more people risking their lives

### 3. Can you fine tune the trade-off between false positives and false negatives with parameter decCrit?
# Yes, you can fine tune the trade-off.
# According to the value of the critical index, the decision boundary shifts/tilts upwards or downwards.
# The smaller the critical index, the smaller the percentage of false negatives
# The larger the critical index, the smaller the percentage of false positives

### 4. Which vale of decCrit would you advise?
# The optimal value for decCrit in my opinion is -7.5
# As I said earlier in the exercise, I think that the priority is to avoid false negatives
# However, you do not want to sacrifice the integrity of the experiment by only focusing on false negatives
# The value of -7.5 gives a good compromise of overall performances.
# Getting: 14.6% of false negative, 5.3% of false positives and 8.8% of misspecified cases
# Compared to other solutions, this values offers very good results according to what we are looking for


##### Exercise 3 #####

# set up
# for the ease of future computation I will invert the columns and rows of the table presented in the hw

x1 = c(13, -21, 7, 11)
x2 = c(-2, -1.2, 2, 0.5)
x3 = c(-1.3, 2.4, 2, -1)

table_inverted = data.frame(x1, x2, x3)
rownames(table_inverted) = c('D1', 'D2', 'D3', 'D4')

### computing the means and the sample standard deviations of x1, x2, x3
# means
mean_x1 = mean(x1)
mean_x2 = mean(x2)
mean_x3 = mean(x3)
means = c(mean_x1, mean_x2, mean_x3)

# standard deviations
std_x1 = sd(x1)
std_x2 = sd(x2)
std_x3 = sd(x3)
standard_deviations = c(std_x1, std_x2, std_x3)

# summing them up in a table
statistics = data.frame(means, standard_deviations)
rownames(statistics) = c('x1', 'x2', 'x3')
colnames(statistics) = c('mean', 'standard deviation')

# standardizing the data
table_standard = data.frame(scale(table_inverted))
