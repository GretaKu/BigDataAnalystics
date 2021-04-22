library(readtext)
library(quanteda)
library(quanteda.textmodels)
library(cowplot)
library(PerformanceAnalytics)
library(psych)
install.packages("PerformanceAnalytics")
install.packages("psych")

#########################################################################
#########################################################################
# Creating the Corpus of the UK party manifestos since 2010
#########################################################################
#########################################################################

myText <- readtext("UK new manifestoes/*.txt", 
                   docvarsfrom = "filenames", dvsep = " ", docvarnames = c("Party", "Year"))
myText <- gsub("[\u00E2]","",myText$text) 
summary(testCorpus)

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
abline(lm(scores_texts_soc$score_wf ~scores_texts_soc$score_ws ), col="re