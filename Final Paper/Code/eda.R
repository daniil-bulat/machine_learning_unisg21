# Explorative Data Analysis

setwd("~/Desktop/Uni/FS21/Machine Learning/machine_learning_unisg21/Final Paper")

# Data
data = read.csv("Data/sentiment/reddit_sentiment.csv")
data = data[,2:8]

plot_data = data

#################################### Scores ####################################
# Scores: 0 - 200
ggplot(data=plot_data, aes(score)) + 
  geom_histogram(col="red", 
                 fill="green", 
                 alpha = .5) + 
  labs(title="Histogram of Scores, 0 - 200", x="Score", y="Count") + 
  xlim(c(0,200)) + 
  ylim(c(0,2000))

# Scores: 0 - max
ggplot(data=plot_data, aes(score)) + 
  geom_histogram(col="red", 
                 fill="green", 
                 alpha = .5) + 
  labs(title="Histogram of Scores", x="Score", y="Count") + 
  xlim(c(0,350000)) + 
  ylim(c(0,1500))

#################################### Comments ##################################
# Comments: 0 - 200
ggplot(data=plot_data, aes(num_comments)) + 
  geom_histogram(breaks=seq(0, 2000, by=1), 
                 col="blue", 
                 fill="green", 
                 alpha = .5) + 
  labs(title="Histogram of Comments, 0 - 200", x="Comments", y="Count") + 
  xlim(c(0,200)) + 
  ylim(c(0,2000))

# Comments: 0 - 10000
ggplot(data=plot_data, aes(num_comments)) + 
  geom_histogram(#breaks=seq(0, 2000, by=1), 
    col="blue", 
    fill="green", 
    alpha = .5) + 
  labs(title="Histogram of Comments, 0 - 10'000", x="Comments", y="Count") + 
  xlim(c(0,10000)) + 
  ylim(c(0,2000))

#################################### Score Mean #################################
ggplot(data=plot_data, aes(scores_avg)) + 
  geom_histogram(col="green", 
                 fill="blue", 
                 alpha = .5) + 
  labs(title="Histogram of Sentiments", x="Sentiment", y="Count")

#################################### Text Length ###############################
plot_data$texlen = nchar(plot_data$text)

# Text Length: 0 - 200
ggplot(data=plot_data, aes(texlen)) + 
  geom_histogram(col="green", 
                 fill="blue", 
                 alpha = .5) + 
  labs(title="Histogram of Text Length", x="Text Length", y="Count") +
  xlim(c(0,200))


# Text Length: 0 - max
ggplot(data=plot_data, aes(texlen)) + 
  geom_histogram(breaks=seq(0, 41000, by=10),
                 col="green", 
                 fill="blue", 
                 alpha = .5) + 
  labs(title="Histogram of Text Length", x="Text Length", y="Count") + 
  xlim(c(0,41000)) + 
  ylim(c(0,1000))

########################### Day / Sentiment ####################################
weekdays = c("Montag", "Dienstag", "Mittwoch", "Donnerstag", "Freitag","Samstag", "Sonntag")
plot_data$day = weekdays(as.Date(plot_data$timestamp))
mean(plot_data$scores_avg)

# Boxplot
p = ggplot(data = plot_data, aes(x=day, y=scores_avg, fill=day)) + 
  geom_boxplot() + 
  ylim(c(-1,1))
p + facet_wrap( ~ day, scales="free")

# Boxplot with correct order
plot_data$day = factor(plot_data$day , levels=weekdays)

boxplot(plot_data$scores_avg ~ plot_data$day , col=rgb(0.2,0.8,0.6,0.8,1) , ylab="mean sent score" , 
        xlab="Weekdays")

################################################################################
# Time Series of Sentiment over Weekdays
plot_data$sent = ifelse(plot_data$scores_avg > 0,"positiv", ifelse(plot_data$scores_avg==0, "neutral", "negativ"))

day_score = data.frame(matrix(ncol = 3, nrow = 0))
x = c("pos", "neg", "day")
colnames(day_score) = x

# dataframe of sentiment (pos/neg) for each weekday
for(i in weekdays){
  # positiv
  positiv = subset(plot_data, day==i & sent=="positiv",
                   select=scores_avg)
  pos = round(mean(positiv$scores_avg),3)
  # negativ
  negativ = subset(plot_data, day==i & sent=="negativ",
                   select=scores_avg)
  neg = round(mean(negativ$scores_avg),3)
  # append
  day_score[nrow(day_score) + 1,] = c(pos,neg,i)
}

# correct ordering of weekdays
day_score$day = factor(day_score$day,levels = weekdays)

# timeseires plot of sentiment over weekdays
ggplot() + 
  geom_line(data = day_score, aes(x = day, y = pos, group=1), color = rgb(0.2,0.8,0.6,0.8,1)) +
  geom_line(data = day_score, aes(x = day, y = neg, group=1), color = rgb(1,0.3,0.5,0.6,1)) +
  xlab('') +
  ylab('Sentiment')







