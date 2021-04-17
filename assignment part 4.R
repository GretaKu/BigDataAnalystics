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
library(rio)
library(irr)


# Data collection ---------------------------------------------------------

# We collect data with hashtag covidvaccine and code if a tweet is pro vaccination or not. 0 = not, 1 = pro 

# Pulling tweets
tweets <- search_tweets("#CovidVaccine", n = 10000)

# Tweets clearing ---------------------------------------------------------
#load("vaccine_tweets_raw.RData")

#Get only the tweets in english
tweets <- tweets %>% filter(lang == "en") 

#Cleaning
clean_tweet <- tweets

url_regex <- "http[s]?://(?:[a-zA-Z]|[0-9]|[$-_@.&+]|[!*\\(\\),]|(?:%[0-9a-fA-F][0-9a-fA-F]))+"
clean_tweet$text <- str_remove_all(clean_tweet$text, url_regex) #remove url
clean_tweet$text <- gsub("&amp", "", clean_tweet$text) #remove html entity
clean_tweet$text <- gsub("(RT|via)((?:\\b\\W*@\\w+)+)", "", clean_tweet$text) #remove rt via
clean_tweet$text <- gsub("@\\w+", "", clean_tweet$text) #remove mentions
#clean_tweet$text <- str_replace_all(clean_tweet$text,"#[a-z,A-Z]*","") #remove hashtags
clean_tweet$text <- gsub("[^[:alnum:]///' ]", " ", clean_tweet$text)     #keep only alpha numeric 

print(clean_tweet$text[1:30])

save(clean_tweet, file = "clean_tweet.RData")


# Creating Test and Train Dataset -----------------------------------------

#load("clean_vaccine_tweet.RData")

tweets_short <- clean_tweet %>% select(user_id, created_at, status_id, text)

set.seed(123)
split <- sample.split(tweets_short$text, SplitRatio = 0.952)
test <- subset(tweets_short, split == TRUE)
train <- subset(tweets_short, split == FALSE)

write_csv2(train, "train_raw.csv")

save(test, file = "test_raw.RData")
save(train, file = "train_raw.RData")


# Import Train  -----------------------------------------------------------

#recode train dominik
dominik <- import("train_raw_Dominik.csv")
dominik <- dominik %>% mutate(category = ifelse(category == 3, 1,
                                     ifelse(category == 1, 3, category)))
export(dominik, "train_raw_Dominik.csv")

greta <- import("train_raw_greta.csv")
merge <- merge(dominik, greta, by = c("status_id", "created_at"))

               
#import categories train data               
cat <- import("train_raw_cat.csv")
cat <- cat %>% mutate(check = ifelse(category_dominik == category_greta, T, F))
table(cat$check)

export(cat, "train_raw_check.csv")

# checking inter rater reliability
train_data <- import("train_raw_check.csv")
train_data <- train_data %>% mutate(check = ifelse(category_dominik == category_greta, T, F))
table(train_data$check)
kappa2(train_data[,4:5])


# ML Algorithm ------------------------------------------------------------

# FIRST STEP: let's create the DfM for the training-set -------------------

str(train_data)
# class-label variable: choose_one (0=tweets not relevant; 1=relevant tweets)
table(train_data$category_greta)
prop.table(table(train_data$category_greta))

myCorpusTwitterTrain <- corpus(train_data)
Dfm_train <- dfm(myCorpusTwitterTrain , remove = c(stopwords("smart")), remove_punct = TRUE, remove_numbers=TRUE, 
                 tolower = TRUE, remove_symbols=TRUE,  remove_separators=TRUE, remove_url = TRUE, split_hyphens = TRUE)

Dfm_train <- dfm_trim(Dfm_train , min_docfreq = 2, verbose=TRUE)
Dfm_train  <- dfm_remove(Dfm_train , min_nchar = 2)
topfeatures(Dfm_train , 20)  # 20 top words



# SECOND STEP: let's create the DfM for the test-set ----------------------

x10 <- read.csv("test_raw.csv", sep = ";")
str(x10)
nrow(x10)
myCorpusTwitterTest <- corpus(x10)
Dfm_test<- dfm(myCorpusTwitterTest , remove = c(stopwords("smart")), remove_punct = TRUE, remove_numbers=TRUE, 
               tolower = TRUE, remove_symbols=TRUE,  remove_separators=TRUE, remove_url = TRUE, split_hyphens = TRUE)
Dfm_test<- dfm_trim(Dfm_test, min_docfreq = 2, verbose=TRUE)
Dfm_test<- dfm_remove(Dfm_test, min_nchar = 2)
topfeatures(Dfm_test , 20)  # 20 top words



# THIRD STEP: Let's make the features identical between train and  --------
# test-set by passing Dfm_train to dfm_match() as a pattern.
# after this step, we can "predict" by employing only the features included in the training-set

setequal(featnames(Dfm_train), featnames(Dfm_test)) 
length(Dfm_test@Dimnames$features) 
length(Dfm_train@Dimnames$features) 
test_dfm  <- dfm_match(Dfm_test, features = featnames(Dfm_train))
length(test_dfm@Dimnames$features) 
setequal(featnames(Dfm_train), featnames(test_dfm ))



# FOURTH STEP: Let's convert the two DfMs into matrices for the ML --------
# algorithms to work

train <- as.matrix(Dfm_train) # dense matrix
object.size(train)
# a compressed sparse matrix! 
trainSP <- as(Dfm_train, "dgCMatrix") # compressed matrix
object.size(trainSP )
object.size(train)/object.size(trainSP )

test <- as.matrix(test_dfm)



# FIFHT STEP: let's estimate a ML Model -----------------------------------
# Naive Bayes Model -------------------------------------------------------

table(Dfm_train@x )

str(Dfm_train@docvars$category_greta) 

set.seed(123) 
system.time(NB <- multinomial_naive_bayes(x=train, y=as.factor(Dfm_train@docvars$category_greta), laplace = 1))
summary(NB)
prop.table(table(Dfm_train@docvars$category_greta))

head(NB$params) 

NB_prob <- as.data.frame(NB$params)
NB_prob$Feature <- row.names(NB_prob)
str(NB_prob)
# let's estimate the features that change the most the difference between the relevant and irrelevant conditional probabilities
NB_prob$diff12 <- NB_prob[,2]-NB_prob[,1]
NB_prob$diff13 <- NB_prob[,3]-NB_prob[,1]
NB_prob$diff23 <- NB_prob[,2]-NB_prob[,3]
str(NB_prob)
print(head(NB_prob[order(NB_prob$diff12 , decreasing=TRUE),], 15)) # relevant words for the difference between negativ and neutral
print(head(NB_prob[order(NB_prob$diff13 , decreasing=TRUE),], 15)) # relevant words
print(head(NB_prob[order(NB_prob$diff23 , decreasing=TRUE),], 15)) # relevant words


print(head(NB_prob[order(NB_prob$diff13 , decreasing=FALSE),], 15)) # irrelevant words

#NB_prob$sign <- ifelse(NB_prob$diff>0,"relevant","irrelevant")
#str(NB_prob)

# let's extract the top 20-most relevant contributing features
NB_prob2 <- top_n(NB_prob, 20, diff ) 
NB_prob2
NB_prob3 <- top_n(NB_prob, -20, diff ) 
NB_prob3
NB_prob_new <- rbind(NB_prob2, NB_prob3)
# reorder the features
NB_prob_new <-  mutate(NB_prob_new, Feature= reorder(Feature, diff))

ggplot(NB_prob_new, aes(Feature, diff, fill = sign)) +
  geom_col(show.legend = F) +
  coord_flip() + 
  ylab("Difference in the conditional probabilities") +
  scale_fill_manual(values = c("orange", "blue")) +
  labs(title = "Social Disaster tweets", 
       subtitle = "Irrelevant (-) versus Relevant (+) words - NB")

# let's store the graph
NB_graph <-   ggplot(NB_prob_new, aes(Feature, diff, fill = sign)) +
  geom_col(show.legend = F) +
  coord_flip() + 
  ylab("Difference in the conditional probabilities") +
  scale_fill_manual(values = c("orange", "blue")) +
  labs(title = "Social Disaster tweets", 
       subtitle = "Irrelevant (-) versus Relevant (+) words - NB")

# let's FINALLY predict the test-set
predicted_nb <- predict(NB ,test )
table(predicted_nb )
prop.table(table(predicted_nb ))



# Random Forest -----------------------------------------------------------

set.seed(123)
system.time(RF <- randomForest(y= as.factor(Dfm_train@docvars$category_greta), x=train, importance=TRUE,  do.trace=TRUE, ntree=500))
# 500 trees to start

str(RF$err.rate)
RF$err.rate[nrow(RF$err.rate),] # our final error (overall, and for each of the two classes: irrelevant and relevant tweets)
plot(RF) # the errors for the two classes (red=irrelevant; green=relevant. We make a much greater error for the latter class); in black the average

error <- as.data.frame(RF$err.rate)
# number of trees with the lowest OOB error
which.min(error$OOB) # 72

set.seed(123)
system.time(RF <- randomForest(y= as.factor(Dfm_train@docvars$category_greta), x=train, importance=TRUE,  do.trace=TRUE, ntree=500))
# 500 trees to start

set.seed(123)
system.time(RF2 <- randomForest(y= as.factor(Dfm_train@docvars$choose_one), x=train, importance=TRUE, ntree=72, do.trace=TRUE))
RF$err.rate[nrow(RF$err.rate),] # our final error (overall, and for each of the two classes)
RF2$err.rate[nrow(RF2$err.rate),] # our final error (overall, and for each of the two classes)

# Now let's do some Global Interpretation for our RF!
# What about the importance of each feature for our trained model?
# Variable importance is a standard metric in machine learning, which indicates the amount of information a variable provides 
# to the model for predicting the outcome
head(RF$importance[,3:4])

# let's grap the result
varImpPlot(RF )

# Each features’s importance is assessed based on two criteria:
# -MeanDecreaseAccuracy: gives a rough estimate of the loss in prediction performance when that particular variable is omitted from the training set. 
# Caveat: if two variables are somewhat redundant, then omitting one of them may not lead to massive gains in prediction performance, 
# but would make the second variable more important.

# -MeanDecreaseGini: GINI is a measure of node impurity. Think of it like this: if you use this feature to split the data, how pure will the nodes be? 
# Highest purity means that each node contains only elements of a single class. 
# Assessing the decrease in GINI when that feature is omitted leads to an understanding of how important that feature is to split the data correctly.

# Plz note that these measures are used to rank variables in terms of importance and, thus, their absolute values could be disregarded.

# The problem is that we just get for example the GINI statistics overall w/o differentiating the words most important for specific classes.
# which are however the most important words for the relevant label? and for the irrelevant one?

# let's extract the matrix for GINI and Accuracy
importance_RF <- as.data.frame(RF$importance[,3:4])
str(importance_RF)
importance_RF$Feature<- row.names(importance_RF)
str(importance_RF)
# same words we get with 
varImpPlot(RF )
print(head(importance_RF[order(importance_RF$MeanDecreaseGini, decreasing=TRUE),]))

# let's predict the training-set texts and let's store a new variable in our dfm of the training-set with such predictions
# Why that? Cause we will use such variable to identify the "sign" of the most important features 

predicted_rf <- predict(RF, train, type="class")
table(predicted_rf )
Dfm_train@docvars$predRF <- predicted_rf
table(Dfm_train@docvars$predRF)
#  NOTE: no perfect prediction of the training-set
table(Dfm_train@docvars$choose_one, Dfm_train@docvars$predRF)

# let's assign a feature to the relevant/irrelevant class according to its frequency (if for example the word "collapse" appears
# 10 times among predicted relevant tweets and 5 times among predicted irrelevant tweets,
# we classify it as pertaining to the "relevant tweets" world; and so on)

# let's estimate the number of times a word appear in those tweets that we have classified as related to accident or otherwise as above
sums <- list()
for (v in 0:1){
  sums[[v+1]] <- colSums(train[Dfm_train@docvars[,"predRF"]==v,])
}

sums <- do.call(cbind, sums)
sums
# let's apply the sign (either 1 or 2) if a word appears more in the tweets classified as not related to accident or otherwise
sign <- apply(sums, 1, which.max)
# get the feature 
names <-  dimnames(train)[[2]]
str(names)

df <- data.frame(
  Feature = names, 
  sign = sign-1, # let's recode the sign between 0 and 1 
  stringsAsFactors=F)

str(df)

importanceRF <- merge(importance_RF, df, by="Feature")
str(importanceRF)

## best predictors
for (v in 0:1){
  cat("\n\n")
  cat("value==", v)
  importanceRF <- importanceRF [order(importanceRF $MeanDecreaseGini, decreasing=TRUE),]
  print(head(importanceRF [importanceRF $sign==v,], n=10))
  cat("\n")
  cat(paste(unique(head(importanceRF $Features[importanceRF $sign==v], n=10)), collapse=", "))
}

# let's draw a graph with our results focusing on MeanDecreaseGini
importanceRF$Gini <- ifelse(importanceRF $sign>0,importanceRF $MeanDecreaseGini,importanceRF $MeanDecreaseGini*-1)
# the twop 20 relevant words
importance2RF <- top_n(importanceRF, 20, Gini ) 
importance2RF
# the twop 20 irrelevant words
importance3RF <- top_n(importanceRF, -20, Gini ) 
importance3RF
importance_newRF <- rbind(importance2RF, importance3RF)
str(importance_newRF)
# reorder the features
importance_newRF <-  mutate(importance_newRF, Feature= reorder(Feature, Gini ))
importance_newRF$sign2<- ifelse(importance_newRF$sign>0,"relevant","irrelevant")

ggplot(importance_newRF, aes(Feature, Gini , fill = sign2)) +
  geom_col(show.legend = F) +
  coord_flip() + 
  ylab("Mean Decrease Gini (we recoded as negative the values for the Irrelevant features)") +
  scale_fill_manual(values = c("orange", "blue")) +
  labs(title = "Social Disaster tweets", 
       subtitle = "Irrelevant (-) versus Relevant (+) features - RF")

# let's store the graph
RF_graph <-   ggplot(importance_newRF, aes(Feature, Gini , fill = sign2)) +
  geom_col(show.legend = F) +
  coord_flip() + 
  ylab("Mean Decrease Gini (we recoded as negative the values for the Irrelevant features)") +
  scale_fill_manual(values = c("orange", "blue")) +
  labs(title = "Social Disaster tweets", 
       subtitle = "Irrelevant (-) versus Relevant (+) features - RF")

#### and with 3 categories? try to find it out by yourself!

# let's FINALLY predict the test-set
system.time(predicted_rf <- predict(RF, test,type="class"))
table(predicted_rf )
prop.table(table(predicted_rf))

#####################################################
# let's run a SVM
#####################################################

# note that here I select a linear kernel (in my experience a linear kernel is doing fine with texts data)
# and, as hyperameter, a specific value for the cost C(=1)- A SVM has also other hyperparameters. More on this later on

set.seed(123)# (define a set.seed for being able to replicate the results!)
system.time(SV <- svm(y= as.factor(Dfm_train@docvars$choose_one), x=train, kernel='linear', cost = 1))

# how many supporting vectors?
length(SV$index) 
nrow(train) # 254 out of 400 tweets in the training-set data-frame
head(SV$coefs)

# Now let's do some Global Interpretation for our SVM!

# The coefficients that you're pulling out are the weights for the support vectors.
# Looking at the estimated coefficients is not as informative because they only tell us what support vectors were estimated in the model. 
# But we can have a sense of what observations are more “important” or “separate” better the data by extracting the support vectors 
# in the data matrix and then their corresponding coefficients (times the training labels). Note that this will only work with linear kernels.

# let's predict the training-set texts and let's store a new variable in our dfm of the training-set with such predictions
# Why that? Cause we will use such variable to identify the "sign" of the most important features 
predicted_sv <- predict(SV, train, type="class")
table(predicted_sv )
Dfm_train@docvars$predSV <- predicted_sv
table(Dfm_train@docvars$predSV)
#  NOTE: no perfect prediction of the training-set [that's a good thing for the permutation method we will employ below!]
table(Dfm_train@docvars$choose_one, Dfm_train@docvars$predSV)

# let's identify the 254 vectors (and their corresponding texts!)
str(x)

df_vector <- data.frame(
  vector = x$text[SV$index],
  coef = SV$coefs,
  relevant = predicted_sv[SV$index],
  stringsAsFactors = F
)

str(df_vector )

# irrelevant tweets (among the supporting vectors) 
df_vector  <- df_vector[order(df_vector $coef, decreasing=TRUE),]
head(df_vector [,c("coef", "relevant", "vector")], n=10)

# relevant tweets (among the supporting vectors)
df_vector  <- df_vector[order(df_vector $coef),]
head(df_vector [,c("coef", "relevant", "vector")], n=10)

# let's now focus on identifying the most relevant features for the SVM. For doing that we will rely on a "permutational" approach. 
# What do we mean by permutation? Basically, what we do is the following: you randomly permute the real value of each single feature 
# in your dfm. A feature is “important” if permuting its values increases the model error, because the model relied on the feature for 
# the prediction. The idea is that if we randomly permute the values of an important feature in the training data, the training performance 
# would degrade (since permuting the values of a feature effectively destroys any relationship between that feature and the target variable). 
# A feature is “unimportant” if permuting its values keeps the model error unchanged, because the model ignored the 
# feature for the prediction. (Note: the model is NOT refit to the training data after randomly permuting the values of a feature). 

# For running a permutation, you need first to convert the Dfm into a data frame, not into a matrix (the iml package requires that)

train2 <- convert(Dfm_train, to = "data.frame")
# iml does not want features that begin with @, #, etc.
# the command make.names creates a syntactically valid feature 
# it consists of letters, numbers and the dot or underline characters and starts with a letter or the dot not followed 
# by a number. Features such as ".2way" are not valid for example
# The character "X" is prepended if necessary. All invalid characters are translated to "."
# A missing value is translated to "NA". 
colnames(train2 ) <- make.names(colnames(train2 ))
colnames(train2)

# let's rerun the SVM with the data frame
set.seed(123)
svm_model <- svm(as.factor(Dfm_train@docvars$choose_one) ~ ., data = train2[-1], kernel='linear', cost = 1)
# why train2[-1]? cause the first column in train2 refers to doc_id not to a feature!
head(colnames(train2))

# let's create an object that holds the model and the data
mod <- Predictor$new(svm_model, data = train2[-1], y = as.factor(Dfm_train@docvars$choose_one), type = "prob")

# plan("callr", workers = 6) allows you to run the permutation by taking advantages of all the cores in your computer
# (I have 6 cores for example, therefore workers=6). If you have 4 cores write workers=4, etc. 
# This is a so called "parallelization". It means that the computation per feature is distributed among several cores of your machine.
# This can save you LOTS of time!

system.time({
  plan("callr", workers = 6)
  set.seed(123)
  imp2 <- FeatureImp$new(mod, loss = "ce", n.repetitions=1)
})

# any feature with a value > 1 implies that it is important to estimate the model.
# why larger than 1? Cause feature importance is estimated as a ratio (that's the deafault in iml): error.permutation/error.orig
# If you get a value > 1 it means that the error increases if a given feature is permuted, therefore that
# feature is important! As an alternative you can also estimate feature importance as a difference: error.permutation-error.orig
# you can also repate the analysis (i.e. the shuffling of the feature) X time (for example: n.repetitions=5).
#  The higher the number of repetitions the more stable and accurate the results become (plus: you will have a c.i. as well!)
# However, in our case to save time we just asked for 1 repetition

imp2
res <- imp2$results[,c(1,3)]
str(res)

##########################################
# OK we know which features are important now. But what about their relationship with the class-labels?
# let's estimate that for both ham and spam class labels
##########################################

# let's replicate make.names here so that both train and train2 present the same list of features!
colnames(train) <- make.names(colnames(train))
train2 <- as.data.frame(train)
train3 <-train2[SV$index,] # let's extract from the data frame only the vectors!
train4 <- as.matrix(train3)

Dfm_train2 <-Dfm_train@docvars$predSV[SV$index] # let's extract from the DV only the vectors!
Dfm_train2 

# let's replicate exactly what we already did for the RF case

sums <- list()
for (v in 0:1){
  sums[[v+1]] <- colSums(train4[Dfm_train2==v,], na.rm = TRUE)
}

sums <- do.call(cbind, sums)
sign <- apply(sums, 1, which.max)

# get the feature 
names <-  dimnames(train)[[2]]
str(names)

df <- data.frame(
  feature = names, 
  sign = sign-1,
  stringsAsFactors=F)

str(df)
str(res)

importance <- merge(res, df, by="feature")
str(importance)

## best predictors
for (v in 0:1){
  cat("\n\n")
  cat("value==", v)
  importance <- importance[order(importance$importance, decreasing=TRUE),]
  print(head(importance[importance$sign==v,], n=10))
  cat("\n")
  cat(paste(unique(head(importance$Features[importance$sign==v], n=10)), collapse=", "))
}

str(importance)

# let's draw a graph with our results!
importance$perm <- ifelse(importance$sign>0,importance$importance,importance$importance*-1)
str(importance)
# the top 10 relevant words
importance2 <- top_n(importance, 10, perm ) 
importance2
# the top 20 irrelevant words
importance3 <- top_n(importance, -10, perm ) 
importance3
importance_new <- rbind(importance2, importance3)
str(importance_new)
# reorder the features
importance_new <-  mutate(importance_new, feature= reorder(feature, perm ))
importance_new$sign2<- ifelse(importance_new$sign>0,"Relevant","Not Relevant")
importance_new <-  mutate(importance_new, feature= reorder(feature, perm ))

ggplot(importance_new, aes(feature, perm , fill = sign2)) +
  geom_col(show.legend = F) +
  coord_flip() + 
  ylab("Permutation results (we recoded as negative the values for the irrelevant features)") +
  scale_fill_manual(values = c("orange", "blue")) +
  labs(title = "Social Disaster tweets", 
       subtitle = "Irrelevant (-) versus Relevant (+) features - SVM")

# let's store the graph
SVM_graph <-   ggplot(importance_new, aes(feature, perm , fill = sign2)) +
  geom_col(show.legend = F) +
  coord_flip() + 
  ylab("Permutation results (we recoded as negative the values for the irrelevant features)") +
  scale_fill_manual(values = c("orange", "blue")) +
  labs(title = "Social Disaster tweets", 
       subtitle = "Irrelevant (-) versus Relevant (+) features - SVM")

# let's FINALLY predict the test-set
system.time(predicted_svm <- predict(SV , test))
table(predicted_svm )
prop.table(table(predicted_svm ))

# P.S. How does it work svm with a Multi-category classes? In one-vs-one SVM (as in our example), 
# each classifier is trained to distinguish one class from another
# For M classes, however, you have M(M-1)/2 combinations, which is also the number of resulting classifiers. 
# For example, if we had 3 classes we get 3 classifiers, i.e. class1-vs-class2 and class1-vs-class3, class2-vs-class3. 
# Practically, in each case you suppose that only 2 classes exist, and you train a classifier to distinguish among them. 
# During the prediction phase, you choose the class with a majority  vote among all the classifiers.

#####################################################
# let's run a Gradient Boosting
#####################################################

# let's first re-create the matrix as it was, before the permutation change as above, i.e., colnames(train) <- make.names(colnames(train))
train <- as.matrix(Dfm_train) # dense matrix

# You need always to add the number of classes to be classified in the formula if it is a multi-categorical variable. 
# In the present case, we have just two classes (negative & positive) so we do not care. Other situations could be
# different (as we will see)
numberOfClasses <- length(unique(Dfm_train@docvars$choose_one))
numberOfClasses 

# you DV should be always a numeric one starting from 0. If it is not the case you need to create such variable.
# In our case it is already a numeric variable starting from 0, so we do not bother about it

# you write "objective = "binary:logistic"" if you have just two classes (positive/negative) as in our example.
# Logistic regression for binary classification returns predicted probability (not class)
# Otherwise: "objective = "multi:softmax"" – multiclass classification using the softmax objective, returns predicted class (not probabilities).
# As discussed above, you also need in this case to set an additional num_class (number of classes) parameter defining the number 
# of unique classes.
# Alternatively you can use "multi:softprob" – same as softmax, but it returns predicted probability of each data point belonging to each class

# There are several hyperparameters in a GBC as we have discussed (back to this when we discuss about the grid-search)

# Here, I select a specific value for the number of trees ("nrounds") as well as for "eta".
# eta controls how quickly the algorithm proceeds down the gradient descent. 
# Smaller values reduce the chance of overfitting but also increases the time to find the optimal fit (default is 0.3)

# NOTE: "nthread" (Number of threads) implements a parallelization in your computer. And this makes everything faster!

set.seed(123)
system.time(xgb.fit1 <- xgboost(
  data = train,
  label = Dfm_train@docvars$choose_one,
  nrounds = 500,
  eta = 1, nthread = 4, 
  objective = "binary:logistic",  # for binary
  eval_metric = "error", # binary classification error rate
  verbose = 1               # not silent; if you want it silent "verbose=0"
))

print(head(xgb.fit1))

# Let's do some Global Interpretation for our Gradient Boosting model!

# compute feature importance matrix
importance_matrix <- xgb.importance(model = xgb.fit1)
head(importance_matrix)

# Gain: the relative contribution of the corresponding feature to the model calculated by taking each feature’s contribution 
# for each tree in the model. More in details: at each split in each tree, xgboost computes the improvement in the split-criterion (i.e., the reduction in
# the classification error). Then it averages the improvement made by each variable across all the trees that the variable is used. 
# The variables with the largest average decrease in the classification errors are considered most important.

# Cover: the relative number of observations related to this feature. For example, you have 100 observations, 4 features and 3 trees, 
# and suppose feature1 is used to decide the leaf node for 10, 5, and 2 observations in tree1, tree2 and tree3 respectively; 
# then the metric will count cover for this feature as 10+5+2 = 17 observations. This will be calculated for all the 4 features and the cover will be 
# 17 expressed as a percentage for all features’ cover metrics.

# Frequency: the percentage representing the relative number of times a particular feature occurs in the trees of the model. 
# In the above example, if feature1 occurred in 2 splits, 1 split and 3 splits in each of tree1, tree2 and tree3; then the weightage for feature1 
# will be 2+1+3 = 6. The frequency for feature1 is calculated as its percentage weight over weights of all features.

# let's focus on GAIN (as we generally always does)
importance <- importance_matrix[order(importance_matrix$Gain, decreasing=TRUE),]
head(importance, n=20)

# the problem here is that we just get the GAIN statistics w/o differentiating the words most important for the classes
# for example: which are the most important words for the positive label? and for the negative one?
# so let's improve on that following the same path done with RF and SVM!

predicted_xgb <- round(predict(xgb.fit1, train))
table(predicted_xgb)
Dfm_train@docvars$predXGB <- predicted_xgb

sums <- list()
for (v in 0:1){
  sums[[v+1]] <- colSums(train[Dfm_train@docvars[,"predXGB"]==v,])
}

sums <- do.call(cbind, sums)
sign <- apply(sums, 1, which.max)
str(sign)

# get the feature 
names <-  dimnames(train)[[2]]
str(names)

df <- data.frame(
  Feature = names, 
  sign = sign-1,
  stringsAsFactors=F)

str(df)

importance <- merge(importance_matrix, df, by="Feature")
str(importance)

## best predictors
for (v in 0:1){
  cat("\n\n")
  cat("value==", v)
  importance <- importance[order(importance$Gain, decreasing=TRUE),]
  print(head(importance[importance$sign==v,], n=10))
  cat("\n")
  cat(paste(unique(head(importance$Feature[importance$sign==v], n=10)), collapse=", "))
}

# Let's draw a graph!

str(importance)

importance$Gain_OK <- ifelse(importance$sign>0,importance$Gain,importance$Gain*-1)
importance2 <- top_n(importance, 20, Gain_OK) 
importance2
importance3 <- top_n(importance, -20, Gain_OK) 
importance3
importance_new <- rbind(importance2, importance3)
str(importance_new)
# reorder the features
importance_new <-  mutate(importance_new, Feature= reorder(Feature, Gain_OK))
importance_new$sign2<- ifelse(importance_new$sign>0,"positive","negative")
importance_new <-  mutate(importance_new, Feature= reorder(Feature, Gain_OK))

ggplot(importance_new, aes(Feature, Gain_OK, fill = sign2)) +
  geom_col(show.legend = F) +
  coord_flip() + 
  ylab("Gain (we recoded as negative the values for the Negative features)") +
  scale_fill_manual(values = c("orange", "blue")) +
  labs(title = "Movie Reviews", 
       subtitle = "Negative (-) versus Positive (+) words - XGB")

# let's store the graph
XGB_graph <-    ggplot(importance_new, aes(Feature, Gain_OK, fill = sign2)) +
  geom_col(show.legend = F) +
  coord_flip() + 
  ylab("Gain (we recoded as negative the values for the Negative features)") +
  scale_fill_manual(values = c("orange", "blue")) +
  labs(title = "Movie Reviews", 
       subtitle = "Negative (-) versus Positive (+) words - XGB")

# let's FINALLY predict the test data
predicted_xgb <- predict(xgb.fit1, test)
table(predicted_xgb)
# what's that??? Remember: logistic regression for binary classification in XGB returns predicted probability (not class).
# Therefore let's use "round"
predicted_xgb <- round(predict(xgb.fit1, test))
table(predicted_xgb)
prop.table(table(predicted_xgb))

######################################################
######################################################
# Let's compare the results out-of-sample we got via Naive Bayes, SVM & RF
######################################################
######################################################

prop.table(table(predicted_nb ))
prop.table(table(predicted_svm ))
prop.table(table(predicted_rf ))
prop.table(table(predicted_xgb ))

results <- as.data.frame(rbind(prop.table(table(predicted_nb )), prop.table(table(predicted_rf )),
                               prop.table(table(predicted_svm )), prop.table(table(predicted_xgb ))))
str(results)
results$algorithm <- c("NB", "RF", "SVM", "XGB")
str(results)

# Let's plot the results!

df.long<-melt(results,id.vars=c("algorithm"))
str(df.long)

ggplot(df.long,aes(algorithm,value,fill=variable))+
  geom_bar(position="dodge",stat="identity") + theme(axis.text.x = element_text(color="#993333", size=10, angle=90)) + coord_flip() +  
  ylab(label="Review class in the test-set") +  xlab("algorithm") + scale_fill_discrete(name = "Prediction", labels = c("Irrelevant", "Relevant"))

# let's plot the figures of feature importance
NB_graph

RF_graph

library(gridExtra)
grid.arrange(NB_graph, RF_graph, SVM_graph , XGB_graph,  nrow=2) # Plot everything together

# and so, which result is to trust more than the other one? For answering to that, we need to introduce the Cross-Validation procedure!


>>>>>>> 0a21610085777cde6d6a651eaaac336af6b6c154
