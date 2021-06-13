# Machine Learning Final Project
# Reddit — WallStreetBets Sentiment Analysis / Machine Learning

#                      Haochen Li
#                     Giovanni Magagnin
#                        Jonas Huwyler
#                       Daniil Bulat

library(dplyr)
library(quanteda)
library(readtext)
library(stringr)
library(tidyverse)

setwd("~/Desktop/Uni/FS21/Machine Learning/machine_learning_unisg21/Final Paper")

# Data
data = read.csv("Data/sentiment/reddit_sentiment.csv")
data = data[,2:8] # remove unnecesary columns
data$texlen = nchar(data$text) # add text length colomn (how many character per post)

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

# repeat above for all stock
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



