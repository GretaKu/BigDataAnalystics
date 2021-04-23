
# Libraries ---------------------------------------------------------------

library(readtext)
library(quanteda)
library(ggplot2)
library(quanteda.textstats)
library(tidyverse)


# Loading Data ------------------------------------------------------------

ukText <- readtext("UK new manifestoes/*.txt", 
                   docvarsfrom = "filenames", dvsep = " ", docvarnames = c("Party", "Year"), encoding = "UTF-8")
ukCorpus <- corpus(ukText)
docnames(testCorpus) <- gsub(".txt", "", docnames(testCorpus ))

# making a DFM
myDfm <- dfm(ukCorpus, remove = c(stopwords("english"), tolower = TRUE, stem = TRUE,
             remove_punct = TRUE, remove_numbers=TRUE, remove_symbols = TRUE, split_hyphens = TRUE))

topfeatures(myDfm , 20)  # 20 top words
myDfm <- dfm_remove(myDfm, min_nchar=2)



# Wordfish ----------------------------------------------------------------

# Decision: Lab 2010 on the left and Cons 2010 on the right scale
wfm <- textmodel_wordfish(myDfm, dir = c(4, 11))
summary(wfm)

# extracting estimates
str(wfm)
words2 <- wfm$features
beta2 <-wfm$beta
psi2 <-wfm$psi
scores_words2 <-data.frame(words2, beta2, psi2)
str(scores_words2) 

# top 10 features for negative beta
head(scores_words2[order(scores_words2$beta2),], 10)
# top 10 words for positive beta
tail(scores_words2[order(scores_words2$beta2),], 10)

# Plot Document position
textplot_scale1d(wfm, margin = "documents")


# Wordscores --------------------------------------------------------------

### Checking for correlation
summary(testCorpus)

# keep only words occurring >1 times
myDfm <- dfm_trim(myDfm, min_termfreq = 2)

# compute some document similarities
Simil <- textstat_simil(myDfm , method = "correlation")
Simil

### Set reference scores 
ref_eco <- c(NA, 7.85, NA, NA, 3.85, NA, NA, 5.14, NA, NA, 8.57, NA)
ref_eu <- c(NA,3.14,NA,NA,5.57,NA,NA,6.71,NA,NA,1.14,NA)

### Assign the reference scores to your dfm
ws_eco <- textmodel_wordscores(myDfm, ref_eco)
ws_eu <- textmodel_wordscores(myDfm, ref_eu)

# Plot estimated word positions in the reference texts. It shows the frequency vs. the word-score
textplot_scale1d(ws_eco)
textplot_scale1d(ws_eu)


### Predicition of raw Wordscores for all the texts (reference and virgin ones)
pr_raw_eco <- predict(ws_eco, se.fit = TRUE, newdata = myDfm)
pr_raw_eu <- predict(ws_eu, se.fit = TRUE, newdata = myDfm)

head(pr_raw_eco)
head(pr_raw_eu)

textplot_scale1d(pr_raw_eco)
textplot_scale1d(pr_raw_eu)

# Prediction Virgin texts : rescaling LBG option
summary(ws_eco)
summary(ws_eu)
pr_lbg_eco <- predict(ws_eco, rescaling = "lbg", newdata = myDfm[c(2, 5, 8, 11), ])
pr_lbg_eu <- predict(ws_eu, rescaling = "lbg", newdata = myDfm[c(2, 5, 8, 11), ])
pr_lbg_eco
pr_lbg_eu 


# Compare Results: Eco / EU 
eco <- textplot_scale1d(pr_raw_eco)
eu <- textplot_scale1d(pr_raw_eu)

wordscore_eu_eco <- plot_grid(eco , eu , labels = c('Economic', 'EU'))
ggsave("Wordscore Eco vs. EU.png",plot = wordscore_eu_eco, height = 6.3, width = 16)



# Wordscore vs. Wordfish --------------------------------------------------

# Comparing wordscores vs wordfish
wordscores <- textplot_scale1d(pr_eco)
wordfish <- textplot_scale1d(wfm, margin = "documents")
WORD_score_fish_compare <- plot_grid(wordscores , wordfish , labels = c('Wordscores', 'Wordfish'))
ggsave("Wordscore vs. Wordfish.png",plot = WORD_score_fish_compare, height = 6.3, width = 16)