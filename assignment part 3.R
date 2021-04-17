
# Libraries ---------------------------------------------------------------

library(readtext)
library(quanteda)
library(ggplot2)
library(quanteda.textstats)
library(tidyverse)
library(rio)
library(rtweet)
library(stm)


# Loading Twitter Data ------------------------------------------------------------
load("tweets.RData")


# Cleaning ----------------------------------------------------------------

# cleaning tweets
print(tweets$text[1:30])
clean_tweet <- tweets

url_regex <- "http[s]?://(?:[a-zA-Z]|[0-9]|[$-_@.&+]|[!*\\(\\),]|(?:%[0-9a-fA-F][0-9a-fA-F]))+"
clean_tweet$text <- str_remove_all(clean_tweet$text, url_regex) #remove url
clean_tweet$text <- gsub("&amp", "", clean_tweet$text) #remove html entity
clean_tweet$text <- gsub("(RT|via)((?:\\b\\W*@\\w+)+)", "", clean_tweet$text) #remove rt via
clean_tweet$text <- gsub("@\\w+", "", clean_tweet$text) #remove mentions
clean_tweet$text <- str_replace_all(clean_tweet$text,"#[a-z,A-Z]*","") #remove hashtags
clean_tweet$text <- gsub("[^[:alnum:]///' ]", " ", clean_tweet$text)     #keep only alpha numeric 

print(clean_tweet$text[1:30])


# Corpus / DfM ------------------------------------------------------------

jb_corp <- corpus(clean_tweet)
jb_dfm <- dfm(jb_corp , tolower = TRUE)



# Identifying key words ---------------------------------------------------

dict <- dictionary(list(campaign = c("ballot", "vot*", "polls", "inauguration"),
                        pandemic = c("pandemic", "covid*", "corona*", "vaccin*","shut*"),
                        race = c("blm", "black", "live", "matter", "race","floyd","breonna","garner"),
                        war = c("war*", "soldier*", "tanks", "military*","navy*"),
                        crime = c("crime*", "murder", "killer", "police","shoot*","weapon","gun*","armed"),
                        social = c("obamacare", "poverty", "lgb*", "marijuana"),
                        environment = c("climat*", "greta", "pollution*", "environment", "sustain*", "paris")))

dict

label <- dfm(jb_corp, remove_number = TRUE,  tolower = TRUE, dictionary = dict)

# Trim
label.trim <-dfm_trim(label, min_docfreq = 2, verbose=TRUE)

topfeature <- data.frame(as.list(topfeatures(label.trim)))
topfeature <- pivot_longer(topfeature, cols = c(1:7), names_to = "category", values_to = "value")

p1 <- topfeature %>% ggplot() +
  geom_col(aes(x = reorder(category, - value), y = value))

ggsave(filename = "topfeatures_jb_dict_categories.png",plot = p1, width = 11.5, height = 7.6)




