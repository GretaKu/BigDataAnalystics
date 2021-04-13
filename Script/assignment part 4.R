#FOURTH PART
#Run a further query on Twitter as you like and
#download between 5,000 and 10,000 tweets
#Define a set of categories for the tweets you have
#downloaded (it can be a 2-set categories such as
#positive/negative, or a 3-set categories such as
#positive/negative/neutral, or anything else you
 #want!!!)


#Define a training-set (around 200 tweets if you have a
                       #2-set categories; 300 if you have a 3-set categories,
                       #etc.) and a test-set. Manually codify the tweets
#Each of the student in the group (if any) must codify
#the same tweets


#In case you are working in group: Check your inter-coder reliability
#If the results are satisfactory (i.e., k>.7/.75) then cool!
#Each of the coder will use his/her codified tweets to
#classify the test-set
#If, however, you get an unsatisfactory result (i.e.,
                                                 #k<.7) you should go back to the codification stage
#and find out why did happen and improve the
#classification so to increase the agreement score
#In each of your assignment, plz write me the ppl
#belonging to your group, the k-value you get for the
#inter-coder reliability part, and if you had to repeat the
#analysis a n-number of rounds
#Then run the 3 ML algorithms discussed in class on
#the training-set and pick up the best algorithm via
#cross-validation
#Finally, classify the test-set


# Library -----------------------------------------------------------------

library(tidyverse)
library(rtweet)
library(quanteda)
library(readtext)
library(caTools)
library(e1071)
library(randomForest)
library(caret)
library(naivebayes)
library(car)
library(ggplot2)
library(dplyr)
library(reshape2)
library(iml)
library(future)
library(future.callr)
library(gridExtra)
library(xgboost)
library(Ckmeans.1d.dp)
library(callr)
library(xlsx)


# Data collection ---------------------------------------------------------

#rt <- search_tweets("#CovidVaccine", n = 10000, include_rts = FALSE)
#save(rt, file = "vaccine_tweets_raw.RData")


# Tweets clearing ---------------------------------------------------------
#load("vaccine_tweets_raw.RData")

#Get only the tweets in english
vaccine_tweets <- rt %>% filter(lang == "en") 

#Cleaning
clean_vaccine_tweet <- vaccine_tweets

url_regex <- "http[s]?://(?:[a-zA-Z]|[0-9]|[$-_@.&+]|[!*\\(\\),]|(?:%[0-9a-fA-F][0-9a-fA-F]))+"
clean_vaccine_tweet$text <- str_remove_all(clean_vaccine_tweet$text, url_regex) #remove url
clean_vaccine_tweet$text <- gsub("&amp", "", clean_vaccine_tweet$text) #remove html entity
clean_vaccine_tweet$text <- gsub("(RT|via)((?:\\b\\W*@\\w+)+)", "", clean_vaccine_tweet$text) #remove rt via
clean_vaccine_tweet$text <- gsub("@\\w+", "", clean_vaccine_tweet$text) #remove mentions
#clean_vaccine_tweet$text <- str_replace_all(clean_vaccine_tweet$text,"#[a-z,A-Z]*","") #remove hashtags
clean_vaccine_tweet$text <- gsub("[^[:alnum:]///' ]", " ", clean_vaccine_tweet$text)     #keep only alpha numeric 

print(clean_vaccine_tweet$text[1:30])

#save(clean_vaccine_tweet, file = "clean_vaccine_tweet.RData")


# Creating Test and Train Dataset -----------------------------------------

load("clean_vaccine_tweet.RData")

vaccine_short <- clean_vaccine_tweet %>% select(created_at, status_id, text)

split <- sample.split(vaccine_short$text, SplitRatio = 0.964)
test <- subset(vaccine_short, split == TRUE)
train <- subset(vaccine_short, split == FALSE)

write_csv2(train, "train_raw.csv")

save(test, file = "test_raw.RData")
save(train, file = "train_raw.RData")
