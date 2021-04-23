
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
library(cvTools)

# We collect data with hashtag covidvaccine and code if a tweet is pro vaccination or not. 0 = not, 1 = pro 

# Import Train  -----------------------------------------------------------

train <- import("train_greta.csv")

# checking inter rater reliability
train_data <- train %>% mutate(check = ifelse(Dominik == greta, T, F))
table(train_data$check)
str(train_data)
kappa2(train_data[,2:3])
#Kappa of 0.9 is excellent



# # PREP: DfM for the training-set ----------------------------------

str(train_data)
train_data <- train_data %>% select(greta,text) %>% 
  rename(provax = greta)
# class-label variable: provax (0=tweets not relevant; 1=relevant tweets)
table(train_data$provax)
prop.table(table(train_data$provax))

myCorpusTwitterTrain <- corpus(train_data)
Dfm_train <- dfm(myCorpusTwitterTrain , remove = c(stopwords("smart")), remove_punct = TRUE, remove_numbers=TRUE, 
                 tolower = TRUE, remove_symbols=TRUE,  remove_separators=TRUE, remove_url = TRUE, split_hyphens = TRUE)

Dfm_train <- dfm_trim(Dfm_train , min_docfreq = 2, verbose=TRUE)
Dfm_train  <- dfm_remove(Dfm_train , min_nchar = 2)
topfeatures(Dfm_train , 20)  # 20 top words




# PREP: DfM for the test-set ---------------------------------------

test <- read.csv("test_raw_Dominik.csv")
str(test)
nrow(test)
myCorpusTwitterTest <- corpus(test)
Dfm_test<- dfm(myCorpusTwitterTest , remove = c(stopwords("smart")), remove_punct = TRUE, remove_numbers=TRUE, 
               tolower = TRUE, remove_symbols=TRUE,  remove_separators=TRUE, remove_url = TRUE, split_hyphens = TRUE)
Dfm_test<- dfm_trim(Dfm_test, min_docfreq = 2, verbose=TRUE)
Dfm_test<- dfm_remove(Dfm_test, min_nchar = 2)
topfeatures(Dfm_test , 20)  # 20 top words



# PREP: identical features ------------------------------------------

setequal(featnames(Dfm_train), featnames(Dfm_test)) 
length(Dfm_test@Dimnames$features) 
length(Dfm_train@Dimnames$features) 
test_dfm  <- dfm_match(Dfm_test, features = featnames(Dfm_train))
length(test_dfm@Dimnames$features) 
setequal(featnames(Dfm_train), featnames(test_dfm ))



# PREP: DfMs into Matrices -----------------------------------------

train <- as.matrix(Dfm_train) # dense matrix
object.size(train)
# a compressed sparse matrix! 
trainSP <- as(Dfm_train, "dgCMatrix") # compressed matrix
object.size(trainSP )
object.size(train)/object.size(trainSP )

test <- as.matrix(test_dfm)



# ML ALGORITHM ------------------------------------------------------------

# ALGORITHM: Naive Bayes Model -------------------------------------------------------

table(Dfm_train@x)

str(Dfm_train@docvars$provax) 

set.seed(123) 
system.time(NB <- multinomial_naive_bayes(x=train, y=as.factor(Dfm_train@docvars$provax), laplace = 1))
summary(NB)
prop.table(table(Dfm_train@docvars$provax))

head(NB$params) 

NB_prob <- as.data.frame(NB$params)

#Probability for each feature to be in cat 0 or 1
NB_prob$Feature <- row.names(NB_prob)
str(NB_prob)

# Largest difference between 0 and 1 conditional probabilities
NB_prob$diff <- NB_prob[,2]-NB_prob[,1]

str(NB_prob)
print(head(NB_prob[order(NB_prob$diff , decreasing=TRUE),], 15)) # relevant words for the difference
# people that used the word vaccianted are more likely to talk positive

print(head(NB_prob[order(NB_prob$diff , decreasing=FALSE),], 15)) # irrelevant words
# tweets about empty slots are more likely to be neutral or negativ

NB_prob$sign <- ifelse(NB_prob$diff>0,"relevant","irrelevant")
str(NB_prob)

# top 20-most relevant contributing features
NB_prob2 <- top_n(NB_prob, 20, diff ) 
NB_prob2
NB_prob3 <- top_n(NB_prob, -20, diff ) 
NB_prob3
NB_prob_new <- rbind(NB_prob2, NB_prob3)
# reorder the features
NB_prob_new <-  mutate(NB_prob_new, Feature= reorder(Feature, diff))


(NB_graph <- ggplot(NB_prob_new, aes(Feature, diff, fill = sign)) +
  geom_col(show.legend = F) +
  coord_flip() + 
  ylab("Difference in the conditional probabilities") +
  scale_fill_manual(values = c("orange", "blue")) +
  labs(title = "Covid Vaccination tweets", 
       subtitle = "Irrelevant (-) versus Relevant (+) words - NB"))

ggsave("NB_relevant_features.png", width = 8, height = 10)


# Predicting the test-set
predicted_nb <- predict(NB ,test )
table(predicted_nb )
prop.table(table(predicted_nb ))




# ALGORITHM: Random Forest -----------------------------------------------------------

set.seed(123)
system.time(RF <- randomForest(y= as.factor(Dfm_train@docvars$provax), x=train, importance=TRUE,  do.trace=TRUE, ntree=500))
# 500 trees to start

str(RF$err.rate)
RF$err.rate[nrow(RF$err.rate),] # our final error (overall, and for each of the two classes: irrelevant and relevant tweets)
plot(RF) # the errors for the two classes (red=irrelevant; green=relevant. We make a much greater error for the latter class); in black the average

error <- as.data.frame(RF$err.rate)
# number of trees with the lowest OOB error
which.min(error$OOB) # 95

set.seed(123)
system.time(RF2 <- randomForest(y= as.factor(Dfm_train@docvars$provax), x=train, importance=TRUE, ntree=95, do.trace=TRUE))
RF$err.rate[nrow(RF$err.rate),] # error w/500 trees (overall, and for each of the two classes)
RF2$err.rate[nrow(RF2$err.rate),] # error w/95 trees (overall, and for each of the two classes)


# feature (variable) importance
head(RF$importance[,3:4])

png(filename = "RF_featureImportance.png")
x <- varImpPlot(RF )
dev.off()


# Each features’s importance is assessed based on two criteria:
# -MeanDecreaseAccuracy: gives a rough estimate of the loss in prediction performance when that particular variable is omitted from the training set. 
# -MeanDecreaseGini: GINI is a measure of node impurity. Think of it like this: if you use this feature to split the data, how pure will the nodes be? 
# Highest purity means that each node contains only elements of a single class. 
# Assessing the decrease in GINI when that feature is omitted leads to an understanding of how important that feature is to split the data correctly.

# The problem is that we just get for example the GINI statistics overall w/o differentiating the words most important for specific classes.
# which are however the most important words for the relevant label? and for the irrelevant one?

# Matrix for GINI and Accuracy
importance_RF <- as.data.frame(RF$importance[,3:4])
str(importance_RF)
importance_RF$Feature<- row.names(importance_RF)
str(importance_RF)
# same words we get with 
varImpPlot(RF )
print(head(importance_RF[order(importance_RF$MeanDecreaseGini, decreasing=TRUE),]))


# Prediction of train set
predicted_rf <- predict(RF, train, type="class")
table(predicted_rf )
Dfm_train@docvars$predRF <- predicted_rf
table(Dfm_train@docvars$predRF)
#  NOTE: no perfect prediction of the training-set
table(Dfm_train@docvars$provax, Dfm_train@docvars$predRF)

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

(RF_graph <- ggplot(importance_newRF, aes(Feature, Gini , fill = sign2)) +
  geom_col(show.legend = F) +
  coord_flip() + 
  ylab("Mean Decrease Gini (we recoded as negative the values for the Irrelevant features)") +
  scale_fill_manual(values = c("orange", "blue")) +
  labs(title = "Covid Vaccination tweets", 
       subtitle = "Irrelevant (-) versus Relevant (+) features - RF"))

ggsave("RF_meanGini_relevantwords.png")

# Predicting Test Data
system.time(predicted_rf <- predict(RF, test,type="class"))
table(predicted_rf )
prop.table(table(predicted_rf))




# ALGORITHM: Support Vector Machine --------------------------------------------------

set.seed(123)
system.time(SV <- svm(y= as.factor(Dfm_train@docvars$provax), x=train, kernel='linear', cost = 1))

# 99 supporting vectors?
length(SV$index)
nrow(train) # 99 out of 201 tweets in the training-set data-frame
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
table(Dfm_train@docvars$provax, Dfm_train@docvars$predSV)

# let's identify the 254 vectors (and their corresponding texts!)
str(train_data)

df_vector <- data.frame(
  vector = train_data$text[SV$index],
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

train2 <- as.data.frame(Dfm_train, to = "data.frame")

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
svm_model <- svm(as.factor(Dfm_train@docvars$provax) ~ ., data = train2[-1], kernel='linear', cost = 1)
# why train2[-1]? cause the first column in train2 refers to doc_id not to a feature!
head(colnames(train2))

# let's create an object that holds the model and the data
mod <- Predictor$new(svm_model, data = train2[-1], y = as.factor(Dfm_train@docvars$provax), type = "prob")

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


# OK we know which features are important now. But what about their relationship with the class-labels?
# let's estimate that for both ham and spam class labels


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


(SVM_graph <- ggplot(importance_new, aes(feature, perm , fill = sign2)) +
  geom_col(show.legend = F) +
  coord_flip() + 
  ylab("Permutation results (we recoded as negative the values for the irrelevant features)") +
  scale_fill_manual(values = c("orange", "blue")) +
  labs(title = "Covid Vaccination tweets", 
       subtitle = "Irrelevant (-) versus Relevant (+) features - SVM"))

ggsave("SVM_Permutation_results.png", height = 7, width = 10)


# predict the test-set
system.time(predicted_svm <- predict(SV , test))
table(predicted_svm )
prop.table(table(predicted_svm ))

# P.S. How does it work svm with a Multi-category classes? In one-vs-one SVM (as in our example), 
# each classifier is trained to distinguish one class from another
# For M classes, however, you have M(M-1)/2 combinations, which is also the number of resulting classifiers. 
# For example, if we had 3 classes we get 3 classifiers, i.e. class1-vs-class2 and class1-vs-class3, class2-vs-class3. 
# Practically, in each case you suppose that only 2 classes exist, and you train a classifier to distinguish among them. 
# During the prediction phase, you choose the class with a majority  vote among all the classifiers.


# ALGORITHM: Gradient Boosting -------------------------------------------------------

# let's first re-create the matrix as it was, before the permutation change as above, i.e., colnames(train) <- make.names(colnames(train))
train <- as.matrix(Dfm_train) # dense matrix

# You need always to add the number of classes to be classified in the formula if it is a multi-categorical variable. 
numberOfClasses <- length(unique(Dfm_train@docvars$provax))
numberOfClasses 

# you write "objective = "binary:logistic"" if you have just two classes (positive/negative) as in our example.
# Logistic regression for binary classification returns predicted probability (not class)

# There are several hyperparameters in a GBC as we have discussed (back to this when we discuss about the grid-search)

# Here, I select a specific value for the number of trees ("nrounds") as well as for "eta".
# eta controls how quickly the algorithm proceeds down the gradient descent. 
# Smaller values reduce the chance of overfitting but also increases the time to find the optimal fit (default is 0.3)

# NOTE: "nthread" (Number of threads) implements a parallelization in your computer. And this makes everything faster!

set.seed(123)
system.time(xgb.fit1 <- xgboost(
  data = train,
  label = Dfm_train@docvars$provax,
  nrounds = 500,
  eta = 1, nthread = 4, 
  objective = "binary:logistic",  # for binary
  eval_metric = "error", # binary classification error rate
  verbose = 1               # not silent; if you want it silent "verbose=0"
))

print(head(xgb.fit1))

# Interpretation
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

(XGB_graph <- ggplot(importance_new, aes(Feature, Gain_OK, fill = sign2)) +
  geom_col(show.legend = F) +
  coord_flip() + 
  ylab("Gain (we recoded as negative the values for the Negative features)") +
  scale_fill_manual(values = c("orange", "blue")) +
  labs(title = "Covid Vaccination tweets", 
       subtitle = "Not relevant (-) versus Relevant (+) words - XGB"))

ggsave("XGB_gain.png")

# Predicting Test Data
predicted_xgb <- predict(xgb.fit1, test)
table(predicted_xgb)
# what's that??? Remember: logistic regression for binary classification in XGB returns predicted probability (not class).
# Therefore let's use "round"
predicted_xgb <- round(predict(xgb.fit1, test))
table(predicted_xgb)
prop.table(table(predicted_xgb))





# ALGORITHM: Summary of ML Results ---------------------------------------------------


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

ggsave("ML_algorithms_proptables.png", width = 10, height = 7)


library(gridExtra)

ml_graph <- grid.arrange(NB_graph, RF_graph, SVM_graph , XGB_graph,  nrow=2) # Plot everything together
ggsave("ML_algorithms_topwords.png", plot = ml_graph, width = 16, height = 12)




# CROSS-VALIDATION --------------------------------------------------------


# CV: Preperation ---------------------------------------------------------

train <- import("train_greta.csv")
train <- train %>% select(X, text, greta) %>% rename(provax = greta)
str(train)

myCorpusTwitterTrain <- corpus(train)
Dfm_train <- dfm(myCorpusTwitterTrain , remove = c(stopwords("smart")), remove_punct = TRUE, remove_numbers=TRUE, 
                 tolower = TRUE, remove_symbols=TRUE,  remove_separators=TRUE, remove_url = TRUE, split_hyphens = TRUE )

# Let's trim the dfm in order to keep only tokens that appear in at least 5% of the reviews
Dfm_train <- dfm_trim(Dfm_train , min_docfreq = 0.05, verbose=TRUE, docfreq_type = "prop")
topfeatures(Dfm_train , 20)  # 20 top words
train <- as.matrix(Dfm_train) 



# # PREP: DfM for the training-set ----------------------------------

# our classes
table(Dfm_train@docvars$provax)
# our benchmark: accuracy .524
prop.table(table(Dfm_train@docvars$provax))


# CV: k-fold=5 LOOP -------------------------------------------------------

# STEP 1: create the 5 folds
ttrain <- train

# split data in 5 folds
set.seed(123) 
k <- 5 
folds <- cvFolds(NROW(ttrain ), K=k)
str(folds)


# CV LOOP: Support Vector Machine -------------------------------------------

# STEP 2: the LOOP

system.time(for(i in 1:k){
  train <- ttrain [folds$subsets[folds$which != i], ] # Set the training set
  validation <- ttrain [folds$subsets[folds$which == i], ] # Set the validation set
  set.seed(123)
  newrf <- svm(y= as.factor(Dfm_train[folds$subsets[folds$which != i], ]@docvars$provax) ,x=train, kernel='linear', cost = 1) 
  newpred <- predict(newrf,newdata=validation) # Get the predicitons for the validation set (from the model just fit on the train data)
  class_table <- table("Predictions"= newpred, "Actual"=Dfm_train[folds$subsets[folds$which == i], ]@docvars$provax)
  print(class_table)  
  df<-confusionMatrix( class_table,  mode = "everything") 
  df.name<-paste0("conf.mat.sv",i) # create the name for the object that will save the confusion matrix for each loop (=5)  
  assign(df.name,df)
})

# STEP 3: the metrics
ls()
# we have created 5 objects that have saved the 5 confusion matrices we have created. We can estimate now the performance metrics on such results
# for example:
conf.mat.sv1
# note that the F1 value you see is not the one for the overall model, but just for the first class (i.e., "negative")
# Therefore we have to estimate the average value of F1 for each k-fold by hands! See below
str(conf.mat.sv1)
conf.mat.sv1$overall[1] # overall accuracy: (36+46)/(36+46+9+9)
conf.mat.sv1$byClass[1] # Recall for negative: (36)/(36+9) - think vertically!
conf.mat.sv1$byClass[3] # Precision for negative: (36)/(36+9) - think horizontally!
conf.mat.sv1$byClass[2] # Recall for positive: (46)/(46+9) - think vertically!
conf.mat.sv1$byClass[4] # Precision for positive: (46)/(46+9) - think horizontally!

(2*conf.mat.sv1$byClass[1]*conf.mat.sv1$byClass[3])/(conf.mat.sv1$byClass[1]+conf.mat.sv1$byClass[3]) # F1 for negative
(2*conf.mat.sv1$byClass[2]*conf.mat.sv1$byClass[4])/(conf.mat.sv1$byClass[2]+conf.mat.sv1$byClass[4]) # F1 for positive

SVMPredict <- data.frame(col1=vector(), col2=vector(), col3=vector()) # makes a blank data frame with three columns to fill with the predictions; 
# why 3 columns? 1 for accuracy; and 2 for the K1 value of the classes in the Sentiment (given you have just two classes!)

for(i in  mget(ls(pattern = "conf.mat.sv")) ) {
  Accuracy <-(i)$overall[1] # save in the matrix the accuracy value
  F1_negative<- (2*(i)$byClass[1]*(i)$byClass[3])/((i)$byClass[1]+(i)$byClass[3]) # save in the matrix the F1 value for negative 
  F1_positive <- (2*(i)$byClass[2]*(i)$byClass[4])/((i)$byClass[2]+(i)$byClass[4]) # save in the matrix the F1 value for positive
  SVMPredict <- rbind(SVMPredict , cbind(Accuracy , F1_negative, F1_positive ))
}

str(SVMPredict )
SVMPredict [is.na(SVMPredict )] <- 0 # if I get some NA for some categories with respect to F1 (this happens when BOTH precision and recall score for that category is 0), 
# replace NA with 0; this usually can happen when your training-set is not that big, so that during the k-fold cv, you can have a training-set with few observations
# for some given class 
str(SVMPredict )

# Let's compare the average value for accuracy and f1

acc_sv_avg <- mean(SVMPredict[, 1] )
f1_sv_avg <- mean(colMeans(SVMPredict[-1] ))

acc_sv_avg
f1_sv_avg


# CV LOOP: Random Forest --------------------------------------------------

# STEP 2: the LOOP

system.time(for(i in 1:k){
  train <- ttrain [folds$subsets[folds$which != i], ] 
  validation <- ttrain [folds$subsets[folds$which == i], ]
  set.seed(123)
  newrf <- randomForest(y= as.factor(Dfm_train[folds$subsets[folds$which != i], ]@docvars$provax) ,x=train, do.trace=TRUE, ntree=100) 
  newpred <- predict(newrf,newdata=validation, type="class") 
  class_table <- table("Predictions"= newpred, "Actual"=Dfm_train[folds$subsets[folds$which == i], ]@docvars$provax)
  print(class_table)  
  df<-confusionMatrix( class_table, mode = "everything") 
  df.name<-paste0("conf.mat.rf",i)
  assign(df.name,df)
})

# STEP 3: the metrics

RFPredict <- data.frame(col1=vector(), col2=vector(), col3=vector()) 

for(i in  mget(ls(pattern = "conf.mat.rf")) ) {
  Accuracy <-(i)$overall[1] # save in the matrix the accuracy value
  F1_negative <- (2*(i)$byClass[1]*(i)$byClass[3])/((i)$byClass[1]+(i)$byClass[3]) # save in the matrix the F1 value for negative
  F1_positive <- (2*(i)$byClass[2]*(i)$byClass[4])/((i)$byClass[2]+(i)$byClass[4]) # save in the matrix the F1 value for positive
  RFPredict <- rbind(RFPredict , cbind(Accuracy , F1_negative, F1_positive))
}

RFPredict [is.na(RFPredict )] <- 0 
str(RFPredict )

# Let's compare the average value for accuracy and f1

acc_rf_avg <- mean(RFPredict [, 1] )
f1_rf_avg <- mean(colMeans(RFPredict [-1] ))

acc_rf_avg
f1_rf_avg


# CV LOOP: Gradient Boost -------------------------------------------------

x <- as.factor(Dfm_train@docvars$provax)
x
table(x)
x <- as.numeric(x)
x
x[ x ==1 ] <-0
x[ x ==2 ] <-1
table(x)

Dfm_train@docvars$code <- x
str(Dfm_train)
table(Dfm_train@docvars$code)
table(Dfm_train@docvars$provax)

# STEP 2: the LOOP

system.time(for(i in 1:k){
  train <- ttrain [folds$subsets[folds$which != i], ] 
  validation <- ttrain [folds$subsets[folds$which == i], ]
  set.seed(123)
  newrf <- xgboost(
    data = train,
    label =Dfm_train[folds$subsets[folds$which != i], ]@docvars$code,
    nrounds = 500,
    eta = 1, nthread = 4, 
    objective = "binary:logistic",  # for binary 
    eval_metric = "error", # binary classification error rate
    verbose = 0) # here "code" NOT "Sentiment"!!!
  newpred <- predict(newrf,newdata=validation, type="class") 
  newpred <- round(  newpred)
  class_table <- table("Predictions"= newpred, "Actual"=Dfm_train[folds$subsets[folds$which == i], ]@docvars$code)
  print(class_table)  
  df<-confusionMatrix( class_table, mode = "everything") 
  df.name<-paste0("conf.mat.xgb",i)
  assign(df.name,df)
})

# STEP 3: the metrics
XGBPredict <- data.frame(col1=vector(), col2=vector(), col3=vector()) 
for(i in  mget(ls(pattern = "conf.mat.xgb")) ) {
  Accuracy <-(i)$overall[1] # save in the matrix the accuracy value
  p <- as.data.frame((i)$byClass)
  F1_negative <- (2*(i)$byClass[1]*(i)$byClass[3])/((i)$byClass[1]+(i)$byClass[3]) # save in the matrix the F1 value for negative
  F1_positive <- (2*(i)$byClass[2]*(i)$byClass[4])/((i)$byClass[2]+(i)$byClass[4]) # save in the matrix the F1 value for positive
  XGBPredict <- rbind(XGBPredict , cbind(Accuracy , F1_negative, F1_positive))
}

XGBPredict [is.na(XGBPredict )] <- 0 
str(XGBPredict )

# Let's compare the average value for accuracy and f1

acc_xgb_avg <- mean(XGBPredict [, 1] )
f1_xgb_avg <- mean(colMeans(XGBPredict [-1] ))

acc_xgb_avg
f1_xgb_avg


# CV LOOP: Naive Bayes ----------------------------------------------------

system.time(for(i in 1:k){
  train <- ttrain [folds$subsets[folds$which != i], ] 
  validation <- ttrain [folds$subsets[folds$which == i], ]
  set.seed(123)
  newrf <-  multinomial_naive_bayes(y= as.factor(Dfm_train[folds$subsets[folds$which != i], ]@docvars$provax) ,x=train,  laplace = 1) 
  newpred <- predict(newrf,newdata=validation, type="class") 
  class_table <- table("Predictions"= newpred, "Actual"=Dfm_train[folds$subsets[folds$which == i], ]@docvars$provax)
  print(class_table)  
  df<-confusionMatrix( class_table, mode = "everything") 
  df.name<-paste0("conf.mat.nb",i)
  assign(df.name,df)
})

# STEP 3: the metrics
NBPredict <- data.frame(col1=vector(), col2=vector(), col3=vector()) 

for(i in  mget(ls(pattern = "conf.mat.nb")) ) {
  Accuracy <-(i)$overall[1] # save in the matrix the accuracy value
  F1_negative <- (2*(i)$byClass[1]*(i)$byClass[3])/((i)$byClass[1]+(i)$byClass[3]) # save in the matrix the F1 value for negative
  F1_positive <- (2*(i)$byClass[2]*(i)$byClass[4])/((i)$byClass[2]+(i)$byClass[4]) # save in the matrix the F1 value for positive
  NBPredict <- rbind(NBPredict , cbind(Accuracy , F1_negative, F1_positive))
}

NBPredict [is.na(NBPredict )] <- 0 
str(NBPredict )

# Let's compare the average value for accuracy and f1

acc_nb_avg <- mean(NBPredict [, 1] )
f1_nb_avg <- mean(colMeans(NBPredict [-1] ))

acc_nb_avg
f1_nb_avg 


# CV: RESULTS -------------------------------------------------------------

acc_nb_avg
acc_sv_avg
acc_rf_avg
acc_xgb_avg

f1_nb_avg
f1_sv_avg
f1_rf_avg
f1_xgb_avg

# Let's plot the results!

gb1 <- as.data.frame(acc_nb_avg )
colnames(gb1)[1] <- "Accuracy NB"
gb2 <- as.data.frame(acc_sv_avg )
colnames(gb2)[1] <- "Accuracy SV"
gb3 <- as.data.frame(acc_rf_avg )
colnames(gb3)[1] <- "Accuracy RF"
gb4 <- as.data.frame(acc_xgb_avg )
colnames(gb4)[1] <- "Accuracy GB"
ac_tot <- cbind(gb1, gb2, gb3, gb4)
ac_tot
str(ac_tot)
df.long_ac_tot<-melt(ac_tot)
str(df.long_ac_tot)

p <- ggplot(df.long_ac_tot, aes(x=variable, y=value)) + 
  geom_boxplot() + xlab("Algorithm") + ylab(label="Value") + 
  ggtitle("Naive Bayes vs. SVM vs. RF vs. GB K-fold cross-validation (K=5): Accuracy") + coord_flip() 

gb1 <- as.data.frame(f1_nb_avg)
colnames(gb1)[1] <- "F1 NB"
gb2 <- as.data.frame(f1_sv_avg )
colnames(gb2)[1] <- "F1 SV"
gb3 <- as.data.frame(f1_rf_avg )
colnames(gb3)[1] <- "F1 RF"
gb4 <- as.data.frame(f1_xgb_avg )
colnames(gb4)[1] <- "F1 GB"
f1_tot <- cbind(gb1, gb2, gb3, gb4)
f1_tot
str(f1_tot)
df.long_f1_tot<-melt(f1_tot)
str(df.long_f1_tot)

p2 <- ggplot(df.long_f1_tot, aes(x=variable, y=value)) + 
  geom_boxplot() + xlab("Algorithm") + ylab(label="Value") + 
  ggtitle("Naive Bayes vs. SVM vs. RF vs. GB K-fold cross-validation (K=5): F1") + coord_flip() 

model_check <- grid.arrange(p, p2,  nrow=2) # Plot everything together
ggsave("ML_algorithm_evaluation.png", model_check, width = 11, height = 7)



# CV: Merging Prediction from NB to DfM --------------------------------------

# Predicting Test Data
system.time(predicted_rf <- predict(RF, test,type="class"))
str(predicted_rf)

test_orig <- read_csv2("test_raw_Dominik.csv")
str(test_orig)

test_predicted <- cbind(test_orig, predicted_rf)
write_csv2(x = test_predicted, file = "test_predicted_RF.csv")


x <- read_csv2(file = "test_predicted_RF.csv")
str(x)
