
# Libraries ---------------------------------------------------------------

library(quanteda)
library(readtext)
library(ggplot2)
library(ldatuning)
library(topicmodels)
library(lubridate)
library(topicdoc)
library(cowplot)
library(stm)
library(igraph)
library(stringr)


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

DfmStm <- convert(myDfm, to = "stm", docvars = docvars(myCorpus))
head(docvars(myCorpus))
str(DfmStm)


# LDA Model Set-Up ---------------------------------------------------------

lda_dfm <- dfm(myCorpus, remove =(stopwords("smart")), tolower = TRUE, stem = TRUE,
               remove_punct = TRUE, remove_numbers=TRUE)
lda_dfm <- dfm_remove(lda_dfm, c('*-time', '*-timeUpdated', 'GMT', 'BST')) 
lda_dfm <-   dfm_trim(lda_dfm, min_termfreq = 0.95, termfreq_type = "quantile", 
                      max_docfreq = 0.1, docfreq_type = "prop")

# 20 top words
topfeatures(lda_dfm, 20)

# keeping only documents with number of tokens >0 
lda_dfm[ntoken(lda_dfm) == 0,]
lda_dfm <- lda_dfm[ntoken(lda_dfm) > 0,]


dtm <- convert(lda_dfm, to = "topicmodels")
set.seed(123)
system.time(lda <- LDA(dtm, method= "Gibbs", k = 14))

# extracting the most important topics
terms(lda, 10)

# adding topics to each document
topics <- get_topics(lda, 1)
head(topics, 10)

# or you can obtain the most likely topics using topics() and save them as a document-level variable.
head(topics(lda))
docvars(lda_dfm, 'pred_topic') <- topics(lda)
str(lda_dfm)


# LDA Estimation of optimal K ---------------------------------------------------------

topic_diagnostics(lda, dtm)
topic_coherence(lda, dtm)
topic_exclusivity(lda)

mean(topic_coherence(lda, dtm))
mean(topic_exclusivity(lda))

#  K may vary between 4 and 25
top <- c(4:25)
top

# empty DF
results <- data.frame(first=vector(), second=vector(), third=vector()) 
results 

system.time(
  for (i  in top) 
  { 
    set.seed(123)
    lda <- LDA(dtm, method= "Gibbs", k = (i),  control=list(verbose=50L, iter=100))
    topic <- (i)
    coherence <- mean(topic_coherence(lda, dtm))
    exclusivity <- mean(topic_exclusivity(lda))
    results <- rbind(results , cbind(topic, coherence, exclusivity ))
  }
)

results

#K=14 seems to be the best choice
plot(results$coherence, results$exclusivity, main="Scatterplot Example",
     xlab="Semantic Coherence", ylab="Exclusivity ", pch=19)
text(results$coherence, results$exclusivity, labels=results$topic, cex= 1,  pos=4)



# STM ---------------------------------------------------------------------

# Running STM with optimat number of K = 14

system.time(stmFitted_wo <- stm(DfmStm $documents, DfmStm $vocab, K = 14, max.em.its = 75, 
                             data = DfmStm $meta, init.type = "Spectral"))

system.time(stmFitted <- stm(DfmStm $documents, DfmStm $vocab, K = 14, max.em.its = 75, 
                             prevalence = ~ pres + s(year_month),  data = DfmStm $meta, init.type = "Spectral")) # around 28 seconds on my laptop


# Interpreting the STM by plotting and inspecting results -----------------

#Topics
labelTopics(stmFitted_wo, n=8)
labelTopics(stmFitted, n=8)

labelTopics(stmFitted, n=8, topics=1) # the same just for topic 1

# Plot
plot(stmFitted_wo, type = "labels", labeltype = c("frex"), n=10) # plot just frex words 
plot(stmFitted, type = "labels", labeltype = c("frex"), n=10) # plot just frex words 

plot(stmFitted, type = "summary", labeltype = c("frex"), n=10)  # topic 5 is the most frequent one
# topic meaning: according to frex words, topic 5 seems to be related to the trend in the economy; topic 10 to inflation; 
# topic 9 about politics; etc.
plot(stmFitted_wo, type = "summary", labeltype = c("frex"), n=10) 

plot(stmFitted, type = "hist", labeltype = c("frex")) # Here topic 5 appears as more "evenly" distributed across documents than
# for example topic 11 for example









