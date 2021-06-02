# Explorative Data Analysis

setwd("~/Desktop/Uni/FS21/Machine Learning/machine_learning_unisg21/Final Paper")

# Data
data = read.csv("Data/sentiment/reddit_sentiment.csv")
data = data[,2:8]
names(data) = c("text","score","num_comments","timestamp","sentences","scores","score_sum")



#################################### Scores ####################################
# Scores: 0 - 200
ggplot(data=data, aes(score)) + 
  geom_histogram(breaks=seq(0, 2000, by=1), 
                 col="red", 
                 fill="green", 
                 alpha = .5) + 
  labs(title="Histogram of Scores, 0 - 200", x="Score", y="Count") + 
  xlim(c(0,200)) + 
  ylim(c(0,2000))

# Scores: 0 - 1000
ggplot(data=data, aes(score)) + 
  geom_histogram(breaks=seq(0, 2000, by=1), 
                 col="red", 
                 fill="green", 
                 alpha = .5) + 
  labs(title="Histogram of Scores, 0 - 1000", x="Score", y="Count") + 
  xlim(c(0,1000)) + 
  ylim(c(0,2000))

#################################### Comments ##################################
# Comments: 0 - 200
ggplot(data=data, aes(num_comments)) + 
  geom_histogram(breaks=seq(0, 2000, by=1), 
                 col="blue", 
                 fill="green", 
                 alpha = .5) + 
  labs(title="Histogram of Comments, 0 - 200", x="Comments", y="Count") + 
  xlim(c(0,200)) + 
  ylim(c(0,2000))

# Comments: 0 - 1000
ggplot(data=data, aes(num_comments)) + 
  geom_histogram(breaks=seq(0, 2000, by=1), 
                 col="blue", 
                 fill="green", 
                 alpha = .5) + 
  labs(title="Histogram of Comments, 0 - 1000", x="Comments", y="Count") + 
  xlim(c(0,1000)) + 
  ylim(c(0,2000))

#################################### Score Sum #################################
ggplot(data=data, aes(score_sum)) + 
  geom_histogram(breaks=seq(-1, 1, by=0.01), 
                 col="green", 
                 fill="blue", 
                 alpha = .5) + 
  labs(title="Histogram of Sentiments", x="Sentiment", y="Count") + 
  xlim(c(-1,1)) + 
  ylim(c(0,11000))

#################################### Text Length ###############################
data$texlen = nchar(data$text)

# Text Length: 0 - 200
ggplot(data=data, aes(texlen)) + 
  geom_histogram(breaks=seq(0, 200, by=1), 
                 col="green", 
                 fill="blue", 
                 alpha = .5) + 
  labs(title="Histogram of Text Length", x="Text Length", y="Count") + 
  xlim(c(0,200)) + 
  ylim(c(0,1000))


# Text Length: 0 - max
ggplot(data=data, aes(texlen)) + 
  geom_histogram(breaks=seq(0, max(data$texlen), by=1), 
                 col="green", 
                 fill="blue", 
                 alpha = .5) + 
  labs(title="Histogram of Text Length", x="Text Length", y="Count") + 
  xlim(c(0,max(data$texlen))) + 
  ylim(c(0,1000))





