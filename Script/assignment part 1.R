
# Libraries ---------------------------------------------------------------

library(readtext)
library(quanteda)
library(ggplot2)
library(quanteda.textstats)
library(tidyverse)


# Loading Data ------------------------------------------------------------

ukText <- readtext("UK new manifestoes/*.txt", 
                   docvarsfrom = "filenames", dvsep = " ", docvarnames = c("Party", "Year"), encoding = "UTF-8")
str(ukText)



# Corpus ------------------------------------------------------------------

ukCorpus <- corpus(ukText)
summary(ukCorpus)

# ukCorpus
head(summary(ukCorpus))

# observe corpus
ndoc(ukCorpus)
texts(ukCorpus)[1]
texts(ukCorpus)[1:3]
print(ukCorpus , max_ndoc = 2, max_nchar = 250)
print(tokens(ukCorpus ), max_ndoc = 3, max_ntok = 10)

# Creating Subcorpus for Party
Cons <- corpus_subset(ukCorpus, Party == "Cons")
Lab <- corpus_subset(ukCorpus, Party == "Lab")
UKIP <- corpus_subset(ukCorpus, Party == "UKIP")
Lib <- corpus_subset(ukCorpus, Party == "Lib")

# Document-Feature Matrix -------------------------------------------------

ukDfm <- dfm(ukCorpus)

# check out dfm
ukDfm[1:5, 1:10]

# 20 top features pre removing stopword
topfeatures(ukDfm , 20) 

# removing stopwords, stemming etc.
myDfm <- dfm(ukCorpus , remove = c(stopwords("english"), "also"), tolower = TRUE, stem = TRUE,
             remove_punct = TRUE, remove_numbers=TRUE)

# final topwords
topfeatures(myDfm , 20)


# Stats Summary (Lexical Dispersion Plot) ---------------------------------

# kwic function (keywords-in-context): Search word and view it in context

# By working on a corpus, we retain the original text sequence. Therefore, for example, we can detect both the relative frequency of an employed word across documents 
# as well as the “timing” of that word via a Lexical dispersion plot 

options(width = 200)
kwic(ukCorpus, "terror")
kwic(ukCorpus, "terror*")
kwic(ukCorpus, "army*")

# IDEA: maybe sentiment anaysis on EU mentioning? ----
test <- kwic(ukCorpus, "EU*")
test <- as.data.frame(test)
table(test$docname)
barplot(table(test$docname))

# Note that by default, the kwic() is word-based. If you like to look up a multiword combination, use phrase()
kwic(ukCorpus,  phrase("EU referendum"))

# We can plot a kwic object via a Lexical dispersion plot 

textplot_xray(kwic(ukCorpus, "brexit"))

textplot_xray(
  kwic(ukCorpus, "EU*"),
  kwic(ukCorpus, "brexit"),
  kwic(ukCorpus, "protest*"))

#further textplot options in 1a


# Stats Summary (Wordcloud) -----------------------------------------------

myDfm

# by defining a seed number, you are sure to get always the same plot
set.seed(100)
textplot_wordcloud(myDfm , min.count = 6, random.order = FALSE,
                   rot.per = .25, 
                   colors = RColorBrewer::brewer.pal(8,"Dark2"))

textplot_wordcloud(myDfm , min.count = 10,
                   colors = c('red', 'pink', 'green', 'purple', 'orange', 'blue'))


# Party individual
summary(Cons)
Cons_dfm <- dfm(Cons, remove = stopwords("english"),
              remove_punct = TRUE)
Lab_dfm <- dfm(Lab, remove = stopwords("english"),
                remove_punct = TRUE)

set.seed(100)
textplot_wordcloud(Cons_dfm, min.count= 1, random.order = FALSE,
                   rot.per = .25, 
                   colors = RColorBrewer::brewer.pal(8,"Dark2"))

textplot_wordcloud(Lab_dfm, min.count= 1, random.order = FALSE,
                   rot.per = .25, 
                   colors = RColorBrewer::brewer.pal(8,"Dark2"))




#########################################################################
# Statistical summaries (2): Plotting the wordclouds 
#########################################################################

# One of the simplest statistical summary method you can apply to a DfM is a tag cloud.
# A tag cloud is a visual representation of text data, in which tags are single words whose frequency is shown with different font size (and/or color)

# make a dfm
myDfmCons2 <- dfm(ukCorpus, remove = stopwords("english"), tolower = TRUE, stem = TRUE,
             remove_punct = TRUE, remove_numbers=TRUE)

# by defining a seed number, you are sure to get always the same plot
set.seed(100)
textplot_wordcloud(myDfmCons2 , min.count = 6, random.order = FALSE,
                   rot.per = .25, 
                   colors = RColorBrewer::brewer.pal(8,"Dark2"))

textplot_wordcloud(myDfmCons2 , min.count = 10,
                   colors = c('red', 'pink', 'green', 'purple', 'orange', 'blue'))



# You can also plot a “comparison cloud”, but this can only be done with fewer than eight documents:




#########################################################################
#  Statistical summaries (3): Frequency plots comparison
#########################################################################

# If you want to compare the frequency of a single term across different texts, 
# you can also use textstat_frequency, group the frequency by speech and extract the term.

pres_corpus <- corpus_subset(data_corpus_inaugural, President %in% c("Obama", "Trump"))
summary(pres_corpus)

# Create a dfm grouped by president
pres_dfm <- dfm(pres_corpus, remove = stopwords("english"), remove_punct = TRUE)

str(pres_dfm)
docvars(pres_dfm)

# Let's first create a new string variable with the name of the President and of the Year
pres_dfm@docvars$Year_str <-paste0(pres_dfm@docvars$President," ", as.character(pres_dfm@docvars$Year))
docvars(pres_dfm)
str(pres_dfm)

# Get frequency grouped by president
freq_grouped <- textstat_frequency(dfm(pres_dfm), groups = "Year_str")
str(freq_grouped)

# Filter the term "american"
freq_american <- subset(freq_grouped, freq_grouped$feature %in% "american")

ggplot(freq_american, aes(x = group, y = frequency)) +
  geom_point() +
  scale_y_continuous(limits = c(0, 14), breaks = c(seq(0, 14, 2))) +
  xlab(NULL) +
  ylab("Frequency of word - american -") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

# texstat_frequency allows also to plot the most frequent words by group
# Calculate frequency by Presidential speeches - first 10 words for speech
freq_weight <- textstat_frequency(pres_dfm, n = 10, groups = "Year_str")

ggplot(data = freq_weight, aes(x = nrow(freq_weight):1, y = frequency)) +
  geom_point() +
  facet_wrap(~ group, scales = "free") +
  coord_flip() +
  scale_x_continuous(breaks = nrow(freq_weight):1,
                     labels = freq_weight$feature) +
  labs(x = NULL, y = "Frequency")


#########################################################################
# Statistical summaries (4): Comparing words associated with a target group vs. reference group
#########################################################################

# More sophisticated methods compare the differential occurrences of words across texts or partitions of a corpus, using statistical association measures, 
# to identify the words that belong primarily to sub-groups such as those predominantly associated with male- versus female - authored documents, or Democratic versus Republican speeches

# In particular, if you want to compare the differential associations of keywords in a target and reference group, 
# you can calculate “keyness” which is based on the textstat_keyness command. 
# In this example, we compare the inaugural speech by Donald Trump with the speeches by Barack Obama

summary(pres_corpus)
# Create a dfm grouped by president
pres_dfm <- dfm(pres_corpus,   remove = stopwords("english"),  remove_punct = TRUE)

docnames(pres_corpus)
# Calculate keyness and determine Trump as target group
result_keyness <- textstat_keyness(pres_dfm, target = "2017-Trump")

# if you get a negative value, it means that Obama uses that word more than Trump (i.e., the target group) and viceversa
# Plot estimated word keyness
textplot_keyness(result_keyness) 

# what is chi2? Chi-squared test is used to determine whether there is a statistically significant difference between 
# the expected frequencies and the observed frequencies in one or more categories of a contingency table (in our cases
# we are talking about the frequencies of words in two different set of texts)
# Plot without the reference text (in this case Obama)
textplot_keyness(result_keyness, show_reference = FALSE)

head(result_keyness , 10)
tail(result_keyness , 10)

# compare Trump 2017 to other post-war presidents
pwdfm <- dfm(corpus_subset(data_corpus_inaugural, Year > 1945))
head(textstat_keyness(pwdfm, target = "2017-Trump"), 10)
# using the likelihood ratio method
head(textstat_keyness(dfm_smooth(pwdfm), measure = "lr", target = "2017-Trump"), 10)

# Plot estimated word keyness 
result_keyness3 <- textstat_keyness(pwdfm, target = "2017-Trump")
textplot_keyness(result_keyness3) 

#########################################################################
# Statistical summaries (5): Lexical diversity 
#########################################################################

# Other quantitative summary measures of documents are designed to characterize specific qualities of texts 
# Comparing the rates of types and tokens forms the foundation for measures of lexical diversity (the rate of vocabulary usage), with most common such measure 
# comparing the number of types to the number of tokens (the “type-token ratio”)
# For example, it is argued that populist communication means simplified political discourse (lower diversity), in an attempt to reach the public more easily

# textstat_lexdiv() command calcuates precisely lexical diversity in various measures based on the number of unique types of tokens 
# and the length of a document. It is useful for analysing speakers’ or writers’ linguistic skill, or complexity 
# of ideas expressed in documents.

inaug_dfm <- dfm(data_corpus_inaugural, remove = stopwords('en'))
lexdiv <- textstat_lexdiv(inaug_dfm)

# TTR is estimated as V/N, where
# V (types=total number of unique terms); N (tokens=total number of words in the dfm)
head(lexdiv, 5)
tail(lexdiv, 5)

# when you run textstat_lexdiv it automatically removes numbers punctuation etc from the corpus [w/o the need for you
# to specify that when you create your dfm: see ?textstat_lexdiv]
# that's why the ratio you get between Types and Tokens from the corpus gives you a different value:
summary(data_corpus_inaugural)
# for Washington 1789
625/1537
head(lexdiv, 1)

rm(list=ls(all=TRUE))
setwd("C:/Users/luigi/Dropbox/TOPIC MODEL")

library(readtext)
library(quanteda)
library(ggplot2)

#########################################################################
# Running example: How to deal with Japanese language
#########################################################################

# When you want to show Japanese kanji on a non-Japanese laptop, always write the following at the beginning of your session:
Sys.setlocale("LC_CTYPE","japanese")

# Suppose we want to analyze the text of speeches made on 17 Nov, 2017 and 20 Nov, 2017 
# for the new Japanese parliamentary session

myText <- readtext("Diet speeches/*.txt", encoding = "UTF-8")
str(myText)

# tokens() can segment Japanese texts without additional tools based on the rules defined in the ICU library, 
# which is available via the stringi package (that comes with the Quanteda one). ICU detects boundaries of Japanese words using a dictionary 
# with frequency information (see: http://source.icu-project.org/repos/icu/icu/tags/release-58-rc/source/data/brkitr/dictionaries/)
# for the list of Asian languages covered, including Chinese, Thai, but not Korean

icu_toks <- tokens(myText$text)
# this expression means: consider document 2, and report me the first 40 characters appearing in it 
head(icu_toks[[2]], 40)

# Even if you use a morphological analysis tool, tokenization of Japanese text is far from perfect. 
# You can refine tokens by compounding sequence of the same character class using textstat_collocations() 
# and tokens_compound(). Explore it by yourself if you are interested! 

# Japanese stopwords
stopwords("ja", source = "stopwords-iso")

# A better alternative is using the source "marimo"
stopwords("ja", source = "marimo")

# About stopwords in different languages, take a look at here: https://cran.r-project.org/web/packages/stopwords/README.html
# Alternatively,  you want to remove them by yourself by using the "remove" option discussed previously

jap_dfm <- dfm(icu_toks, tolower = FALSE, remove_punct = TRUE, remove = c(stopwords("ja", source = "marimo")))
topfeatures(jap_dfm, 20)
jap_dfm[, 1:5]

# I want to name properly the texts
jap_dfm@docvars
jap_dfm@docvars[,2]
jap_dfm@docvars[,2] <- myText$doc_id
jap_dfm[, 1:5]

# I could also write directly the name I want to give to such texts (this is an hypothetical example with fictious names!)
jap_dfm@docvars[,2] <- c("A", "B", "C", "D", "E", "F", "G")
jap_dfm@docvars[,2]
jap_dfm[, 1:5]

# If you want to perform more accurate tokenization, you need to install a morphological analysis tool, and call it from R. 
# For example: RcppMeCab (Chinese, Japanese, and Korean): https://github.com/junhewk/RcppMeCab

#########################################################################
# Running example: How to deal with Chinese language
#########################################################################

library(quanteda.corpora)
corp <- quanteda.corpora::download(url = "https://www.dropbox.com/s/37ojd5knz1qeyul/data_corpus_chinesegovreport.rds?dl=1")

# Chinese stopwords
stopwords("zh", source = "misc")

# Alternative
stopwords("zh", source = "stopwords-iso")

# tokenize, remove stop-words and remove punctuation
ch_toks <- tokens(corp) 
head(ch_toks[[2]], 40)

ch_dfm <- dfm(ch_toks, tolower = FALSE, remove_punct = TRUE, remove = c(stopwords("zh", source = "misc")))
topfeatures(ch_dfm)



rm(list=ls(all=TRUE))
setwd("C:/Users/luigi/Dropbox/TOPIC MODEL/")

library(readtext)
library(quanteda)
library(quanteda.textmodels)
library(cowplot)
library(PerformanceAnalytics)
library(psych)

#########################################################################
#########################################################################
# Creating the Corpus of the UK electoral programs 1992, 1997
#########################################################################
#########################################################################

myText <- readtext("Lecture 2/Wordscores manifestos/UK/*.txt", 
                   docvarsfrom = "filenames", dvsep = " ", docvarnames = c("Party", "Year"))
str(myText)

testCorpus <- corpus(myText )
summary(testCorpus)
# I rename the name of the documents
docnames(testCorpus) <- gsub(".txt", "", docnames(testCorpus ))
summary(testCorpus)

#####################
# Remember! Comparing the results with and w/o stopwords, with and w/o stemming is always a good practice
#####################

myDfm <- dfm(testCorpus , remove = stopwords("english"), tolower = TRUE, stem = TRUE,
             remove_punct = TRUE, remove_numbers=TRUE, split_hyphens = TRUE)
topfeatures(myDfm , 20)  # 20 top words
myDfm <- dfm_remove(myDfm, min_nchar=2)

#########################################################################
#########################################################################
# Using Wordfish 
#########################################################################
#########################################################################

# dir indicates which two documents are used for global identification purposes 
# (the first document to the left of the second one); 
# this matters usually more for the interpretatio of the results (i.e., for the direction of the scores 
# along the latent dimension (which positive, which negative ones)), rather than for the estimation per-se

# here: LAB 92 to the left of CONS 92
summary(testCorpus)
wfm <- textmodel_wordfish(myDfm, dir = c(3, 1))
summary(wfm)

# here: CONS 92 to the left of LAB 92
wfm2 <- textmodel_wordfish(myDfm, dir = c(1, 3))
summary(wfm2)

# compare with previous case!
summary(wfm)
summary(wfm2)
cor(wfm$theta, wfm2$theta)

# what does it happen if now you put LIB 92 to the left of CONS 92?
summary(testCorpus)
wfm3 <- textmodel_wordfish(myDfm, dir = c(5, 1))
summary(wfm3)
# Nothing!
cor(wfm$theta, wfm3$theta)
identical(wfm$theta, wfm3$theta)

# Always do Diagnostic!
# A good start for diagnostics is the analysis of word discrimination parameters.
# Weights with large values mean that these features are estimated to be on the extremes of the dimension

# let's learn how to extract the estimates of the model and save them
# let's start with extracting the features

str(wfm)
words2 <- wfm$features
beta2 <-wfm$beta
psi2 <-wfm$psi
scores_words2 <-data.frame(words2, beta2, psi2)
str(scores_words2) 

# top 20 features for negative beta
head(scores_words2[order(scores_words2$beta2),], 40)
# top 20 words for positive beta
tail(scores_words2[order(scores_words2$beta2),], 40)

# in this case we have just 3 documents and it's not very clear the meaning of the latent dimension just
# by looking at betas (at least the first 40 features). Perhaps pessimism vs. optimism in economy?

# Plot estimated word positions by also highlighting specific words
textplot_scale1d(wfm, margin = "features", 
                 highlighted = c("inflationari", "renov", "petrol", 
                                 "supremaci", "dream", "renaiss"), 
                 highlighted_color = "red")

# Plot estimated document positions
summary(testCorpus)

textplot_scale1d(wfm, margin = "documents")
textplot_scale1d(wfm, margin = "documents",  groups = docvars(testCorpus, "Party"))
textplot_scale1d(wfm, margin = "documents",  groups = docvars(testCorpus, "Year"))

#########################################################################
#########################################################################
# Using wordfish: US Presidential Inaugural Speech after 1980
#########################################################################
#########################################################################

# apply wordfish by first considering Reagan 1981 to the right of Obama 2009; 
# and then Trump 2017 to the right of Obama 2009: any change? 

# create a dfm from inaugural addresses from Reagan onwards
presDfm <- dfm(corpus_subset(data_corpus_inaugural, Year > 1980), 
               remove = stopwords("english"), stem = TRUE, remove_punct = TRUE, remove_numbers=TRUE, split_hyphens = TRUE)
presDfm [, 1:10]
str(presDfm)
presDfm@Dimnames$docs

# Obama 2009 to the left of Reagan 1981
wfm <- textmodel_wordfish(presDfm , dir = c(8, 1))
summary(wfm)

words2 <- wfm$features
beta2 <-wfm$beta
psi2 <-wfm$psi
scores_words2 <-data.frame(words2, beta2, psi2)
str(scores_words2) 

# top 20 words for negative beta
head(scores_words2[order(scores_words2$beta2),], 40)
# top 20 words for positive beta
tail(scores_words2[order(scores_words2$beta2),], 40)

# meaning of the latent dimension: dark vs. bright side of power?

# Highlight specific words
textplot_scale1d(wfm, margin = "features", 
                 highlighted = c("tyranni", "murder", "mortal", 
                                 "cheer", "rainbow", "sovereignti"), 
                 highlighted_color = "red")

# Plot estimated document positions
textplot_scale1d(wfm, margin = "documents")

# Obama 2009 to the left of Trump 2017
wfm2 <- textmodel_wordfish(presDfm , dir = c(8, 10))
summary(wfm2)

# Plot estimated document positions
textplot_scale1d(wfm2, margin = "documents")

# check for the correlation
score_reagan <-wfm$theta
score_trump <-wfm2$theta
cor(score_reagan, score_trump)
identical(score_reagan, score_trump)

#########################################################################
#########################################################################
# Using Wordscores
#########################################################################
#########################################################################

#########################################################################
#########################################################################
# Drop unique words and check correlation across documents 
#########################################################################
#########################################################################
summary(testCorpus)

# keep only words occurring >1 times
myDfm <- dfm_trim(myDfm, min_termfreq = 2)

# compute some document similarities
Simil <- textstat_simil(myDfm , method = "correlation")
Simil

#########################################################################
#########################################################################
# Using wordscores: UK example with economic policy positions 
#########################################################################
#########################################################################

# reference texts: 1992 parties manifestos
# reference texts scores: 1992 parties manifestos. Lab: 5.35; LibDem: 8.21; Cons: 17.21
# reference scores derived from an expert survey

###############
# FIRST step:
# Set reference scores 
###############
refscores <- c(17.21, NA, 5.35, NA, 8.21, NA)
refscores

###############
# SECOND step: 
# Assign the reference scores to your dfm
###############
ws <- textmodel_wordscores(myDfm, refscores)
summary(ws) 

# Plot estimated word positions in the reference texts. It shows the frequency vs. the word-score

textplot_scale1d(ws)

# Doing the FIRST and SECOND step in one single step
ws2 <- textmodel_wordscores(myDfm, c(17.21, NA, 5.35, NA, 8.21, NA))
summary(ws2) 

# alternative way to set reference scores
ws3 <- textmodel_wordscores(myDfm, c(17.21, rep(NA,1), 5.35, rep(NA,1), 8.21, rep(NA,1)))
summary(ws3) 

###############
# THIRD step: we predict the raw Wordscores for all the texts (reference and virgin ones)
###############
pr_raw <- predict(ws, se.fit = TRUE, newdata = myDfm)
pr_raw
textplot_scale1d(pr_raw)

# alternative way (with c.i. rather than with s.e.)
pr_all2 <- predict(ws, interval = "confidence", newdata = myDfm)
pr_all2
textplot_scale1d(pr_all2)

# Plot estimated document positions and group by "party" or "year" variable 
summary(testCorpus)
textplot_scale1d(pr_all2, margin = "documents",  groups = docvars(testCorpus, "Party"))
textplot_scale1d(pr_all2, margin = "documents",  groups = docvars(testCorpus, "Year"))

# we want to predict only the virgin texts using the rescaling LGB option
summary(ws) 
pr_lbg <- predict(ws, rescaling = "lbg", newdata = myDfm[c(2, 4, 6), ])
pr_lbg

# obtaining the corresponding confidence interval
pr_lbg <- predict(ws, rescaling = "lbg", newdata = myDfm[c(2, 4, 6), ], interval = "confidence")
pr_lbg

#############################################
#### ALTERNATIVE ways to plot confidence intervals with ggplot2 [raw scores]
#############################################

# save the estimates - document scores
str(pr_all2)
pr_all2
pr_all2$fit
ch <- as.data.frame(pr_all2$fit)
ch$Party <- rownames(ch)
str(ch)
ch$ci <- ch$fit-ch$lwr
str(ch)

ggplot(ch, aes(x=reorder (Party, fit), y=fit, group=1)) +
  geom_point(aes()) +
  geom_errorbar(width=.1, aes(ymin=fit-ci, ymax=fit+ci), colour="darkred") + coord_flip() + xlab("Position") + ylab("Party")

#### ALTERNATIVE ways to plot confidence intervals with ggplot2 [LBG transformed scores]

# Plot estimated document positions [transformed scores - LBG]
pr2 <- predict(ws, rescaling = "lbg", newdata = myDfm[c(2, 4, 6), ], interval = "confidence")
pr2
str(pr2)
pr2$fit

rescaled <- as.data.frame(pr2$fit)
rescaled$Party <- rownames(rescaled)
str(rescaled )

str(rescaled )
rescaled $ci <- rescaled $fit-rescaled $lwr
str(rescaled )

ggplot(rescaled , aes(x=reorder (Party, fit), y=fit, group=1)) +
  geom_point(aes()) +
  geom_errorbar(width=.1, aes(ymin=fit-ci, ymax=fit+ci), colour="darkred") + coord_flip() + xlab("Position") + ylab("Party")

# if you want to add to the previous graph also the reference scores:
str(ws) 
fit <- ws$y
Party <-  ws$x@Dimnames$docs
original_scores <-data.frame(Party , fit)
original_scores$Party <- as.character(original_scores$Party )
str(original_scores)
original_scores <- na.omit(original_scores)
original_scores$lwr <- 0
original_scores$upr <- 0
str(original_scores)
original_scores$ci <- original_scores$upr-original_scores$lwr
str(original_scores)
str(rescaled)

rescaled  <- rbind(rescaled, original_scores) 
str(rescaled )

ggplot(rescaled , aes(x=reorder (Party, fit), y=fit, group=1)) +
  geom_point(aes()) +
  geom_errorbar(width=.1, aes(ymin=fit-ci, ymax=fit+ci), colour="darkred") + coord_flip() + xlab("Position") + ylab("Party")

#########################################################################
#########################################################################
# Using wordscores: UK example with social policy positions 
#########################################################################
#########################################################################

# reference texts: 1992 parties manifestos
# reference texts scores: 1992 parties manifestos. Lab: 6.87; LibDem: 6.53; Cons: 15.34
# Run the analysis by focusing on the raw scores

# FIRST step:
# Set reference scores 
refscores <- c(15.34, NA, 6.87, NA, 6.53, NA)
refscores

# SECOND step: 
# Assign the reference scores to your dfm
ws <- textmodel_wordscores(myDfm, refscores)
summary(ws) 

# Plot estimated word positions in the reference texts (highlight words and print them in red) 
# it shows the frequency vs. the word-score

textplot_scale1d(ws)

# THIRD step: we predict the raw Wordscores for all the texts (reference and virgin ones)
pr_all2 <- predict(ws, interval = "confidence")
pr_all2
textplot_scale1d(pr_all2)

# let's compare results we got via economic vs. social policy dimension
# Wordscores
ws <- textmodel_wordscores(myDfm, c(17.21, rep(NA,1), 5.35, rep(NA,1), 8.21, rep(NA,1)))
pr_eco <- predict(ws, interval = "confidence")
eco <- textplot_scale1d(pr_eco)
soc <- textplot_scale1d(pr_all2)

plot_grid(eco , soc , labels = c('Economic', 'Social'))

str(ws)
str(pr_all2)
str(pr_eco)

# check for the correlation
party <- ws$x@Dimnames$docs
score_soc <- pr_all2$fit
score_eco <- pr_eco$fit

scores_texts <-data.frame(party, score_soc, score_eco )
str(scores_texts) 
colnames(scores_texts)[2] <- "scoreSOC"
colnames(scores_texts)[5] <- "scoreECO"
str(scores_texts) 

cor(scores_texts$scoreSOC, scores_texts$scoreECO)

# Plotting the 2-D policy space

plot(scores_texts$scoreECO, scores_texts$scoreSOC, main = "UK 2-D policy space",
     xlab = "Economic scale", ylab = "Social Scale",
     pch = 19, frame = FALSE, xlim=c(9,12), ylim=c(9,11))
text(scores_texts$scoreECO, scores_texts$scoreSOC, labels = scores_texts$party, pos = 3)

#########################################################################
#########################################################################
# Wordscores vs. Wordfish
#########################################################################
#########################################################################

#########################################################################
# Let's compare the results we get from Wordfish with the raw score ones 
# we get from Wordscores using the economic policy position
#########################################################################
#########################################################################

# Wordscores
ws <- textmodel_wordscores(myDfm, c(17.21, rep(NA,1), 5.35, rep(NA,1), 8.21, rep(NA,1)))
pr_all <- predict(ws, interval = "confidence")

# Wordfish with LAB 92 to the left of CONS 92
wfm <- textmodel_wordfish(myDfm, dir = c(3, 1))

# Comparing wordscores vs wordfish
wordscores <- textplot_scale1d(pr_all)
wordfish <- textplot_scale1d(wfm, margin = "documents")
plot_grid(wordscores , wordfish , labels = c('Wordscores', 'Wordfish'))

# insights: a) same movement of parties between 1992 and 1997: that's a good thing!; 
# b) same position of Cons but not of other 2 parties: what are we measuring with wordfish goes beyond "economic policy" issues?

# check for the correlation
party <- wfm$docs
score_wf <-wfm$theta
score_ws <- pr_all$fit

scores_texts <-data.frame(party, score_wf, score_ws)
str(scores_texts) 
colnames(scores_texts)[3] <- "score_ws"
str(scores_texts) 

cor(scores_texts$score_ws, scores_texts$score_wf)

# you can also draw a scatter, with a fit lines and party names
plot(scores_texts$score_ws, scores_texts$score_wf, main="Scatterplot", 
     xlab="Wordscores", ylab="Wordfish", pch=19)
text(scores_texts$score_ws, scores_texts$score_wf, labels = scores_texts$party, pos = 4,  col = "royalblue" , cex = 0.8)
abline(lm(scores_texts$score_wf ~scores_texts$score_ws ), col="red") # regression line (y~x) 

#########################################################################
#########################################################################
# Replicate the comparison between Wordfish and Wordscores estimates using
# for Wordscores the social policy position
#########################################################################
#########################################################################

# reference texts: 1992 parties manifestos
# reference texts scores: 1992 parties manifestos. Lab: 6.87; LibDem: 6.53; Cons: 15.34
# Run the analysis by focusing on the raw scores

# Wordscores
ws <- textmodel_wordscores(myDfm, c(15.34, rep(NA,1), 6.87, rep(NA,1), 6.53, rep(NA,1)))
pr_all <- predict(ws, interval = "confidence")

# Wordfish with LAB 92 to the left of CONS 92
wfm <- textmodel_wordfish(myDfm, dir = c(3, 1))

# Comparing wordscores vs wordfish
wordscores <- textplot_scale1d(pr_all)
wordfish <- textplot_scale1d(wfm, margin = "documents")
plot_grid(wordscores , wordfish , labels = c('Wordscores', 'Wordfish'))

# check for the correlation
party <- wfm$docs
score_wf <-wfm$theta
score_ws <- pr_all$fit

scores_texts_soc <-data.frame(party, score_wf, score_ws)
str(scores_texts_soc) 
colnames(scores_texts_soc)[3] <- "score_ws"
str(scores_texts_soc) 

# higher correlation with social policy rather than economic policy (.89 vs. .82). Wordfish scale more related to this dimension?
cor(scores_texts_soc$score_ws, scores_texts_soc$score_wf)

# you can also draw a scatter, with a fit lines and party names
plot(scores_texts_soc$score_ws, scores_texts_soc$score_wf, main="Scatterplot", 
     xlab="Wordscores", ylab="Wordfish", pch=19)
text(scores_texts_soc$score_ws, scores_texts_soc$score_wf, labels = scores_texts_soc$party, pos = 4,  col = "royalblue" , cex = 0.8)
abline(lm(scores_texts_soc$score_wf ~scores_texts_soc$score_ws ), col="red") # regression line (y~x) 