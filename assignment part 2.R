
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
#tweets <- get_timeline("JoeBiden",  n = 1500, include_rts=TRUE)
# TWEETS ALREADY PULLED -> GO TO SECTION "Saving and Loading Tweets"

names(tweets)
str(tweets$created_at)

tweets$created_at <- as.Date(tweets$created_at, "%Y-%m-%d")


## binary variable if president or not
tweets %>% filter(created_at >= as.Date("2020-11-03"))
tweets %>% filter(created_at < as.Date("2020-04-01"))

tweets$pres <- ifelse((tweets$created_at >= as.Date("2020-11-03")), 1, 0) # 1 for president voted; 0 for before
table(tweets$pres)

tweets$month <- substr(tweets$created_at, 6, 7)
tweets$year <- substr(tweets$created_at, 1, 4)

tweets$year_month <- as.numeric(paste(tweets$year, tweets$month, sep=""))
table(tweets$year_month)

str(tweets)

# Saving and Loading Tweets -----------------------------------------------

#save(tweets, file = "tweets.RData")
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



myCorpus <- corpus(clean_tweet)
head(summary(myCorpus))

myDfm <- dfm(myCorpus, remove = stopwords("smart"), tolower = TRUE, stem = TRUE,
             remove_punct = TRUE, remove_numbers=TRUE)

topfeatures(myDfm, n = 200)

# Trim
myDfm.trim <-dfm_trim(myDfm, min_docfreq = 2, verbose=TRUE)
topfeatures(myDfm, n = 200)

length(myDfm@Dimnames$features) # 2811 features
length(myDfm.trim@Dimnames$features) # 1578 features


# Convert the dfm from Quanteda to STM: this is a crucial step!!!
# When coverting the dfm, you have also to list the document variables that are present in the corpus!
# in our case: datetime, repub, year

DfmStm <- convert(myDfm, to = "stm", docvars = docvars(myCorpus))
head(docvars(myCorpus))
str(DfmStm)


# STM ---------------------------------------------------------------------

# 1) we want to extract 15 topics (i.e., K=15)

# 2) we want to control for the following covariates in affecting topic prevalence: repub and year;
# We could have ran a model with covariates affecting topical content as well (see below for an example).
# While including more covariates in topic prevalence will rarely affect the speed of the model, 
# including additional levels of the content covariates (topical content) can make the model much slower to converge.
# This is due to the model operating in the much higher dimensional space of words in dictionary 
# (which tend to be in the thousands) as opposed to topics. That's why I suggest you to run first a model w/o covariates
# for topical content. REMEMBER however: the results you get when you add variables for topic prevalence but no
# variables fro topical content will be (slighly or more) different to the ones when you add variables for both 
# topic prevalence and topical content together

# 3) it is highly suggested to employ the "spectral" intitialization  based on the method of moments, which is deterministic 
# and globally consistent under reasonable conditions. This means that no matter of the seed that is set, the SAME results will be generated. 
# As an alternative you could employ a LDA initialization. In this, case, however, the answers the estimation procedure comes up with may depend 
# on starting values of the parameters (e.g., the distribution over words for a particular topic). 
# So remember always to define a set seed to replicate your analysis in this latter case (as we did with Topic Models)!

# Note that by writing "s(year)" we use a spline functions for non-linear transformations of the time-variable.
# if you want to add just a linear relationship between "year" and topics, just write "year"

# You can also relax the number of maximum required iterations if you want, but that would require you (much) more
# time especially with large datasets. However 75 is a very small number! We apply that here just to save time!
# The default value is 500.

system.time(stmFitted_wo <- stm(DfmStm $documents, DfmStm $vocab, K = 15, max.em.its = 75, 
                             data = DfmStm $meta, init.type = "Spectral"))

system.time(stmFitted <- stm(DfmStm $documents, DfmStm $vocab, K = 15, max.em.its = 75, 
                             prevalence = ~ pres + s(year_month),  data = DfmStm $meta, init.type = "Spectral")) # around 28 seconds on my laptop


# Interpreting the STM by plotting and inspecting results -----------------

#Topics
labelTopics(stmFitted_wo, n=8)
labelTopics(stmFitted, n=8)

labelTopics(stmFitted, n=8, topics=1) # the same just for topic 1

# Plot
plot(stmFitted_wo, type = "labels", labeltype = c("frex"), n=5) # plot just frex words 
plot(stmFitted, type = "labels", labeltype = c("frex"), n=5) # plot just frex words 

plot(stmFitted, type = "summary", labeltype = c("frex"), n=5)  # topic 5 is the most frequent one
# topic meaning: according to frex words, topic 5 seems to be related to the trend in the economy; topic 10 to inflation; 
# topic 9 about politics; etc.

plot(stmFitted, type = "hist", labeltype = c("frex")) # Here topic 5 appears as more "evenly" distributed across documents than
# for example topic 11 for example




# To be discussed ---------------------------------------------------------



# Let's read the documents most associated with each topic
# In particular, let's identify the most representative documents for a particular topic. 
# We can also use this in order to get a better sense of the content of actual documents with a high topical content.
docs <- tweets$text
str(docs)

# Let's focus on topic 5 for example and let's identify the three texts with the highest theta for that topic
# and let's read the first 200 words from each of them

docs2 <- substr(docs, start = 1, stop = 200)
thoughts <- findThoughts(stmFitted, texts=docs2, n=3)$docs[[3]]
par(mfrow = c(1, 2),mar = c(.5, .5, 1, .5))
plotQuote(thought5, width = 30, main = "Topic 5")

# Let's focus on topic 9 and let's do the same
thought9 <- findThoughts(stmFitted, texts=docs2, topics=9, n=3)$docs[[1]]
par(mfrow = c(1, 2),mar = c(.5, .5, 1, .5))
plotQuote(thought9, width = 30, main = "Topic 9")

# Let's plot Topic 5 and 9 together
par(mfrow = c(1, 2),mar = c(.5, .5, 1, .5))
plotQuote(thought5, width = 30, main = "Topic 5 - Economy")
plotQuote(thought9, width = 30, main = "Topic 9 - Politics")

# which are the documents with the highest theta for each k?
apply(stmFitted$theta,2,which.max) 
# let's read the entire documents with the highest theta for topic=5
strwrap(texts(myCorpus)[485])

# same one as below
docs2 <- substr(docs, start = 1, stop = 200)
thought5 <- findThoughts(stmFitted, texts=docs2, topics=5, n=1)$docs[[1]]
par(mfrow = c(1, 2),mar = c(.5, .5, 1, .5))
plotQuote(thought5, width = 30, main = "Topic 5")

# which is the document with the 2nd largest value for each topic?
maxn <- function(n) function(x) order(x, decreasing = TRUE)[n]
apply(stmFitted$theta, 2, maxn(2))
# let's read the entire documents with the second highest theta for topic=5
strwrap(texts(myCorpus)[93])

# which is the document with the 3rd largest value for each topic?
apply(stmFitted$theta, 2, maxn(3))
# let's read the entire documents with the third highest theta for topic=5
strwrap(texts(myCorpus)[67])

# which are the most likely topics across our documents?
apply(stmFitted$theta,1,which.max) 
table(apply(stmFitted$theta,1,which.max) )
# you can save them as a document-level variable.
docvars(myDfm , 'STMtopic') <- apply(stmFitted$theta,1,which.max) 
str(myDfm)
head(docvars(myDfm))

# or you can also save them back in the original dataframe
nyt2$topic <- apply(stmFitted$theta,1,which.max)
str(nyt2) 
# Topic 5 - 5 random documents associated to it
set.seed(123)
sample(nyt2$text[nyt2$topic==5], 5)

# But we already know that STM actually computes a distribution over topics. 
# In other words, each document is considered to be about a mixture of topics as we have already discussed!
# This information is included in the matrix thera in the STM object 

# For example, news 1 is 43% about topic 7 and 28% about topic 4 for example
round(stmFitted$theta[1,], 2)
