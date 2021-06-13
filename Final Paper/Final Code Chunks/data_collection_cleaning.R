# Machine Learning Final Project
# Reddit — WallStreetBets Sentiment Analysis / Machine Learning
library(dplyr)
library(quanteda)
library(readtext)
library(stringr)
library(tidyverse)


setwd("~/Desktop/Uni/FS21/Machine Learning/machine_learning_unisg21/Final Paper")

# Data
data = read.csv("Data/sentiment/reddit_sentiment.csv")
data = data[,2:8]
data$texlen = nchar(data$text)

#######
# data_avg = data_avg[,-7]
# # Split data by ",", resulting in a list
# data_avg$scores = gsub("\\[|\\]", "", data_avg$scores)
# data_split = strsplit(data_avg$scores, split = ",", fixed = TRUE)
# 
# 
# # Turn numeric and take middle point of group, sapply turns back into vector
# data_avg$scores_avg = sapply(data_split, function(x) mean(as.numeric(x)))
# data_avg$scores_avg = round(data_avg$scores_avg,3)
# 
# write.csv(data_avg, file="Data/sentiment/reddit_sentiment.csv")
#######

#######
# Data Cleaning
#data$body = recode(data$body, "[removed]"="", .default = data$body)
#data$body = recode(data$body, "[deleted]"="", .default = data$body)
#data$body = recode(data$body, "nan"="", .default = data$body)
#
## mutate Text column combining title and body
#data = data %>%
#  mutate(Text = paste(title, body, sep = " "))
#
## remove unnecessary columns
#data = data[,-(7)]
#data = data[,-(4:5)]
#data = data[,-(1:2)]
#data = data[,c(4,1,2,3)]
#
#write.csv(data, file="Data/wsb_reddit_complete_clean.csv")
#######

############################## Filter for Stock ################################
gme_data = data %>%
  select(text, score, num_comments, timestamp, sentences, scores, scores_avg,texlen) %>%   
  filter(str_detect(text, "GME|game stop|gme|Game Stop"))

tsla_data = data %>%
  select(text, score, num_comments, timestamp, sentences, scores, scores_avg,texlen) %>%   
  filter(str_detect(text, "TSLA|Tesla|tesla|tsla|musk|elon"))

aapl_data = data %>%
  select(text, score, num_comments, timestamp, sentences, scores, scores_avg,texlen) %>%   
  filter(str_detect(text, "AAPL|Apple|apple|mac"))

amc_data = data %>%
  select(text, score, num_comments, timestamp, sentences, scores, scores_avg,texlen) %>%   
  filter(str_detect(text, "AMC|amc|Theaters|Entertainment"))

bb_data = data %>%
  select(text, score, num_comments, timestamp, sentences, scores, scores_avg,texlen) %>%   
  filter(str_detect(text, "BB|bb|Blackberry|BlackBerry|black berry"))

clne_data = data %>%
  select(text, score, num_comments, timestamp, sentences, scores, scores_avg,texlen) %>%   
  filter(str_detect(text, "CLNE|clne|Clean Energy Fuels"))

clov_data = data %>%
  select(text, score, num_comments, timestamp, sentences, scores, scores_avg,texlen) %>%   
  filter(str_detect(text, "CLOV|clov|Clover Health Investments"))

tlry_data = data %>%
  select(text, score, num_comments, timestamp, sentences, scores, scores_avg,texlen) %>%   
  filter(str_detect(text, "TLRY|tlry|Tilary"))

ocgn_data = data %>%
  select(text, score, num_comments, timestamp, sentences, scores, scores_avg,texlen) %>%   
  filter(str_detect(text, "OCGN|ocgn|Ocugen"))

############################## Load Stock Prices ###############################
# GME closing values
gme = read.csv("Data/stocks/GME_jan_may_21.csv")
gme = gme[,-(6:7)]
gme = gme[,-(2:4)]

# percentage change for gme stock
gme = gme %>%
  mutate(pct_change = (Close/lag(Close) - 1) * 100)

tsla = read.csv("Data/stocks/TSLA.csv")
aapl = read.csv("Data/stocks/AAPL.csv")
amc = read.csv("Data/stocks/AMC.csv")
bb = read.csv("Data/stocks/BB.csv")
clne = read.csv("Data/stocks/CLNE.csv")
clov = read.csv("Data/stocks/CLOV.csv")
tlry = read.csv("Data/stocks/TLRY.csv")
ocgn = read.csv("Data/stocks/OCGN.csv")

tsla = tsla[,-(6:7)]
tsla = tsla[,-(2:4)]
aapl = aapl[,-(6:7)]
aapl = aapl[,-(2:4)]
amc = amc[,-(6:7)]
amc = amc[,-(2:4)]
bb = bb[,-(6:7)]
bb = bb[,-(2:4)]
clne = clne[,-(6:7)]
clne = clne[,-(2:4)]
clov = clov[,-(6:7)]
clov = clov[,-(2:4)]
tlry = tlry[,-(6:7)]
tlry = tlry[,-(2:4)]
ocgn = ocgn[,-(6:7)]
ocgn = ocgn[,-(2:4)]


tsla = tsla %>%
  mutate(pct_change = (Close/lag(Close) - 1) * 100)
aapl = aapl %>%
  mutate(pct_change = (Close/lag(Close) - 1) * 100)
amc = amc %>%
  mutate(pct_change = (Close/lag(Close) - 1) * 100)
bb = bb %>%
  mutate(pct_change = (Close/lag(Close) - 1) * 100)
clne = clne %>%
  mutate(pct_change = (Close/lag(Close) - 1) * 100)
clov = clov %>%
  mutate(pct_change = (Close/lag(Close) - 1) * 100)
tlry = tlry %>%
  mutate(pct_change = (Close/lag(Close) - 1) * 100)
ocgn = ocgn %>%
  mutate(pct_change = (Close/lag(Close) - 1) * 100)

############################ Machine Learning ##################################
# remove unnecesary rows
ml_data = gme_data[,-(5:6)]
ml_data = ml_data[,-(1)]

# join sentiment and stock data
ml_join_data = left_join(gme, ml_data,
                    by = c("Date" = "timestamp"))

ml_join_data = ml_join_data[-(1:73),-2]

# give sentiments weight
ml_join_data$weighted_sent = ml_join_data$scores_avg *(ml_join_data$score + ml_join_data$num_comments)

# Filter by Day
gme_final_data = ml_join_data %>%
  group_by(Date) %>% summarise(mean(weighted_sent),mean(pct_change),mean(texlen))

names(gme_final_data)[names(gme_final_data) == "mean(weighted_sent)"] = "Sentiment"
names(gme_final_data)[names(gme_final_data) == "mean(pct_change)"] = "Pct_Change"

############################## Logistic Regression #############################

# Split the data into training and test sample (80%-20%)
data.Train = gme_final_data[1:68,]
data.Test = gme_final_data[69:85,]

# Regression on the training data
gme_model = lm(Pct_Change ~ Sentiment, data=data.Train)
summary(gme_model) 

# coefficients:
coefs=gme_model$coefficients      
coefs

#Prediction with the model on the test data
y_hat=coefs[2]*data.Test$Sentiment+coefs[1]

#Error vector
err=y_hat-data.Test$Pct_Change

#Average square deviation
sum(err^2)/length(err)

# Plot Training Data
plot(data.Train$Pct_Change,data.Train$Sentiment)
abline(gme_model)





data_table = data[,-8]
data_table = data_table[,-5]
data_table = data_table[,c(4,2,3,1,5,6)]


(dd = data.frame(x = 1:8, f = gl(2,4), ch = I(letters[1:8])))
# print() with defaults
print(dd, quote = TRUE, row.names = FALSE, max = 3)
# suppresses row.names and quotes all entries

