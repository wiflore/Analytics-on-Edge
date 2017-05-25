setwd("~/Dropbox/MIT Analytics/Week5")
# Install new packages

install.packages("tm")
install.packages("SnowballC")
install.packages("caTools")
install.packages("rpart")
install.packages("rpart.plot")
install.packages("randomForest")

library(randomForest)
library(caTools)
library(rpart)
library(rpart.plot)
library(randomForest)
library(tm)
library(SnowballC)



Sys.setlocale("LC_ALL", "C")

wiki = read.csv("wiki.csv", stringsAsFactors=FALSE)


wiki$Vandal = as.factor(wiki$Vandal)

str(wiki)

table(wiki$Vandal)

# Create corpus Added

corpusAdded = Corpus(VectorSource(wiki$Added))

# Look at corpus
corpusAdded


corpusAdded = tm_map(corpusAdded, PlainTextDocument)

# Remove stopwords  

corpusAdded = tm_map(corpusAdded, removeWords, c( stopwords("english")))

corpusAdded[[1]]
corpusAdded = tm_map(corpusAdded, stemDocument)

findFreqTerms(corpusAdded)
corpusAdded[[1]]


dtmAdded = DocumentTermMatrix(corpusAdded)
dtmAdded


sparseAdded = removeSparseTerms(dtmAdded, 0.997)
sparseAdded

# Convert to a data frame

wordsAdded = as.data.frame(as.matrix(sparseAdded))

# Make all variable names R-friendly

colnames(wordsAdded) = make.names(colnames(wordsAdded))
colnames(wordsAdded) = paste("A", colnames(wordsAdded))

#end Added



# Create corpus Removed

corpusRemoved = Corpus(VectorSource(wiki$Removed))

# Look at corpus
corpusRemoved


corpusRemoved = tm_map(corpusRemoved, PlainTextDocument)

# Remove stopwords  

corpusRemoved = tm_map(corpusRemoved, removeWords, c( stopwords("english")))

corpusRemoved[[1]]
corpusRemoved = tm_map(corpusRemoved, stemDocument)

findFreqTerms(corpusRemoved)
corpusRemoved[[1]]


dtmRemoved = DocumentTermMatrix(corpusRemoved)
dtmRemoved


sparseRemoved = removeSparseTerms(dtmRemoved, 0.997)
sparseRemoved

# Convert to a data frame

wordsRemoved = as.data.frame(as.matrix(sparseRemoved))

# Make all variable names R-friendly

colnames(wordsRemoved) = make.names(colnames(wordsRemoved))
colnames(wordsRemoved) = paste("R", colnames(wordsRemoved))

ncol(wordsRemoved)
#end Removed

wikiWords = cbind(wordsAdded, wordsRemoved)

str(wikiWords)

# Create dependent variable

wikiWords$Vandal =  as.factor(wiki$Vandal == 1)

table(wikiWords$Vandal)

2061/(1815+2061)

set.seed(123)

split = sample.split(wikiWords$Vandal , SplitRatio = 0.7)

trainSparse = subset(wikiWords , split==TRUE)
testSparse = subset(wikiWords, split==FALSE)

# Build a CART model


wikiCART = rpart(Vandal ~ ., data=trainSparse, method="class")

prp(wikiCART)

# Evaluate the performance of the model
predictCART = predict(wikiCART, newdata=testSparse, type="class")

table(testSparse$Vandal, predictCART)

(618+12)/(618+533+12)

grepl("cat","dogs and cats",fixed=TRUE) # TRUE

grepl("cat","dogs and rats",fixed=TRUE) # FALSE

wikiWords2 = wikiWords

wikiWords2$HTTP = ifelse(grepl("http",wiki$Added,fixed=TRUE), 1, 0)

table(wikiWords2$HTTP)

#new subset with http

wikiTrain2 = subset(wikiWords2, split==TRUE)

wikiTest2 = subset(wikiWords2, split==FALSE)

#cart Model 2

wikiCART2 = rpart(Vandal ~ ., data=wikiTrain2, method="class")

prp(wikiCART2)

# Evaluate the performance of the model
predictCART2 = predict(wikiCART2, newdata=wikiTest2, type="class")

table(wikiTest2$Vandal, predictCART2)

(609 + 57)/(609 + 57 + 488 + 9)

#Sum rows dataframe

wikiWords2$NumWordsAdded = rowSums(as.matrix(dtmAdded))

wikiWords2$NumWordsRemoved = rowSums(as.matrix(dtmRemoved))

mean(wikiWords2$NumWordsAdded)

#Model with numWords

wikiTrain3 = subset(wikiWords2, split==TRUE)

wikiTest3 = subset(wikiWords2, split==FALSE)

wikiCART3 = rpart(Vandal ~ ., data=wikiTrain3, method="class")

prp(wikiCART3)

predictCART3 = predict(wikiCART3, newdata=wikiTest3, type="class")

table(wikiTest3$Vandal, predictCART3)

(514 + 248)/(514+248+104+297)

#Taking into account more variables

wikiWords3 = wikiWords2

wikiWords3$Minor = wiki$Minor

wikiWords3$Loggedin = wiki$Loggedin

wikiTrain4 = subset(wikiWords3, split==TRUE)

wikiTest4 = subset(wikiWords3, split==FALSE)

wikiCART4 = rpart(Vandal ~ ., data=wikiTrain4, method="class")

prp(wikiCART4)

predictCART4 = predict(wikiCART4, newdata=wikiTest4, type="class")

table(wikiTest4$Vandal, predictCART4)

(595 + 241) / (595+241+23+304)


