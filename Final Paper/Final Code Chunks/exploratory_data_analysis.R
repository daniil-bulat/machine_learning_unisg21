# Explorative Data Analysis
library(ggplot2)
library(ggthemes)
library(tidyr)
library(dplyr)
library(xts)
Sys.setlocale("LC_TIME", "C")

setwd("~/Desktop/Uni/FS21/Machine Learning/machine_learning_unisg21/Final Paper")

### PART I - EDA of the complete initial data set.
# Data
data = read.csv("Data/sentiment/reddit_sentiment.csv")

plot_data = data[,(2:8)]
plot_data$texlen = nchar(plot_data$text)
plot_data_2 = plot_data[,-(4:7)]
plot_data_2 = plot_data_2[,-(1)]

#################### Histograms Comments / Score / Text Length #################
colnames(plot_data_2) = c("Scores", "Comments", "Text Length")

# Chart and data transformations
plot_data_2 %>%
  # Reshape
  gather(key = indicator, value = val) %>%
  # Basic chart
  ggplot(aes(x = val)) +
  geom_histogram(colour = "darkgreen", fill = "gray") +
  scale_y_continuous(trans='log10') +
  facet_wrap(~indicator, nrow = 3) +
  ## Theme and looks 
  theme_economist() +
  ggtitle("Histograms") +
  theme(strip.background = element_rect(fill = "gray80", colour = "black",
                                        size = 0.5, linetype = "solid"),
        strip.text = element_text(face = "bold"))

#################################### Sentiment ################################
ggplot(data=plot_data, aes(scores_avg)) + 
  geom_histogram(col="darkgreen", 
                 fill="gray") + 
  labs(title="Histogram of Sentiments", x="Sentiment", y="Count") +
  theme_economist() +
  ggtitle("Histograms") +
  theme(strip.background = element_rect(fill = "gray80", colour = "black",
                                        size = 0.5, linetype = "solid"),
        strip.text = element_text(face = "bold"))


########################### Day / Sentiment ####################################
weekdays = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday","Saturday", "Sunday")
plot_data$day = weekdays(as.Date(plot_data$timestamp))
mean(plot_data$scores_avg)

# Boxplot with correct order
plot_data$day = factor(plot_data$day , levels=weekdays)

boxplot(plot_data$scores_avg ~ plot_data$day , col=rgb(0.2,0.8,0.6,0.8,1) , ylab="Mean Sentiment Score" , 
        xlab="Weekdays")

############################## Time Series #####################################
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
  geom_line(data = day_score, aes(x = day, y = pos,group=1, color="Positive")) +
  geom_line(data = day_score, aes(x = day, y = neg,group=1, color="Negative")) +
  scale_color_manual(name = "", values = c("Positive" = "darkgreen", "Negative" = "black")) + 
  xlab('') +
  ylab('Sentiment') +
  theme_economist() +
  ggtitle("Sentiments over Weekdays") +
  theme(strip.background = element_rect(fill = "gray80", colour = "black",
                                        size = 0.5, linetype = "solid"),
        strip.text = element_text(face = "bold")) +
  theme(axis.text.x = element_text(size = 10))



### PART II - EDA of combined data set 5 features
# Top 5 Data
data = read.csv("~/Desktop/Uni/FS21/Machine Learning/machine_learning_unisg21/Final Paper/Data/sentiment/reddit_sentiment.csv")
data = data[,2:8]
data$texlen = nchar(data$text)

data_5f = data[,-(5:6)]
data_5f = data_5f[,-(1)]

data_5f = arrange(data_5f,timestamp, -num_comments)
data_5f = data_5f[-1,]
data_5f = data_5f %>% 
  group_by(timestamp) %>% 
  mutate(rank = row_number()) %>% 
  select(timestamp,rank, scores_avg,texlen,score,num_comments) %>% 
  filter(rank <= 5)

#################### Histograms Comments / Score / Text Length #################
data_5f_2 = data_5f[,4:6]
colnames(data_5f_2) = c("Text Length", "Scores", "Comments")

# Chart and data transformations
data_5f_2 %>%
  # Reshape
  gather(key = indicator, value = val) %>%
  # Basic chart
  ggplot(aes(x = val)) +
  geom_histogram(colour = "darkgreen", fill = "gray") +
  scale_y_continuous(trans='log10') +
  facet_wrap(~indicator, nrow = 3) +
  ## Theme and looks 
  theme_economist() +
  ggtitle("Histograms") +
  theme(strip.background = element_rect(fill = "gray80", colour = "black",
                                        size = 0.5, linetype = "solid"),
        strip.text = element_text(face = "bold"))

#################################### Sentiment ################################
ggplot(data=data_5f, aes(scores_avg)) + 
  geom_histogram(col="darkgreen", 
                 fill="gray") + 
  labs(title="Histogram of Sentiments", x="Sentiment", y="Count") +
  theme_economist() +
  ggtitle("Histograms") +
  theme(strip.background = element_rect(fill = "gray80", colour = "black",
                                        size = 0.5, linetype = "solid"),
        strip.text = element_text(face = "bold"))

########################### Day / Sentiment ####################################
data_5f$day = weekdays(as.Date(data_5f$timestamp))
mean(data_5f$scores_avg)

# Boxplot with correct order
data_5f$day = factor(data_5f$day , levels=weekdays)

boxplot(data_5f$scores_avg ~ data_5f$day , col=rgb(0.2,0.8,0.6,0.8,1) , ylab="Mean Sentiment Score" , 
        xlab="Weekdays")

############################## Time Series #####################################
# Time Series of Sentiment over Weekdays
data_5f$sent = ifelse(data_5f$scores_avg > 0,"positiv", ifelse(data_5f$scores_avg==0, "neutral", "negativ"))

day_score = data.frame(matrix(ncol = 3, nrow = 0))
x = c("pos", "neg", "day")
colnames(day_score) = x

# dataframe of sentiment (pos/neg) for each weekday
for(i in weekdays){
  # positiv
  positiv = subset(data_5f, day==i & sent=="positiv",
                   select=scores_avg)
  pos = round(mean(positiv$scores_avg),3)
  # negativ
  negativ = subset(data_5f, day==i & sent=="negativ",
                   select=scores_avg)
  neg = round(mean(negativ$scores_avg),3)
  # append
  day_score[nrow(day_score) + 1,] = c(pos,neg,i)
}

# correct ordering of weekdays
day_score$day = factor(day_score$day,levels = weekdays)

# timeseires plot of sentiment over weekdays
ggplot() + 
  geom_line(data = day_score, aes(x = day, y = pos,group=1, color="Positive")) +
  geom_line(data = day_score, aes(x = day, y = neg,group=1, color="Negative")) +
  scale_color_manual(name = "", values = c("Positive" = "darkgreen", "Negative" = "black")) + 
  xlab('') +
  ylab('Sentiment') +
  theme_economist() +
  ggtitle("Sentiments over Weekdays") +
  theme(strip.background = element_rect(fill = "gray80", colour = "black",
                                        size = 0.5, linetype = "solid"),
        strip.text = element_text(face = "bold")) +
  theme(axis.text.x = element_text(size = 10))

