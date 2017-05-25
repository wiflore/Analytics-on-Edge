setwd("~/Dropbox/MIT Analytics/Week5")
# Install new x+94vM6La,bg 

library(randomForest)
library(caTools)
library(rpart)
library(rpart.plot)
library(randomForest)
library(tm)
library(SnowballC)
library(ROCR)



Sys.setlocale("LC_ALL", "C")

emails = read.csv("emails.csv", stringsAsFactors=FALSE)
summary(emails)
str(emails)
table(emails$spam)

#Max chars in an email
max(nchar(emails$text))

which.max(nchar(emails$text))

nchar(emails$text[2651])

#Min chars in an email


min(nchar(emails$text))

which.min(nchar(emails$text))

nchar(emails$text[2651])


# Create corpus

corpus = Corpus(VectorSource(emails$text))

# Look at corpus
corpus

corpus[[2]]


# Convert to lower-case

corpus = tm_map(corpus , tolower)
corpus [[2]]


# IMPORTANT NOTE: If you are using the latest version of the tm package, you will need to run the following line before continuing (it converts corpus to a Plain Text Document). This is a recent change having to do with the tolower function that occurred after this video was recorded.

corpus = tm_map(corpus, PlainTextDocument)
corpus [[2]]

# Remove punctuation

corpus =  tm_map(corpus, removePunctuation)

# Remove stopwords 


corpus = tm_map(corpus, removeWords, c(stopwords("english")))

corpus[[2]]

# Stem document 

corpus= tm_map(corpus, stemDocument)

corpus [[2]]


# Create matrix

dtm= DocumentTermMatrix(corpus)

#How many terms
dtm

findFreqTerms(dtm, lowfreq=1)

#spare terms

spdtm = removeSparseTerms(dtm , 0.95)
spdtm

# Convert to a data frame

emailsSparse = as.data.frame(as.matrix(spdtm))


# Make all variable names R-friendly

colnames(emailsSparse) = make.names(colnames(emailsSparse))
which.max(colSums(emailsSparse))


# Adding target
emailsSparse$spam = emails$spam

#how many variables in the boolean
sum(colSums(subset(emailsSparse, spam==0)) >= 5000)
sum(colSums(subset(emailsSparse, spam==1)) >= 1000)
sort(colSums(subset(emailsSparse, spam == 0)))
sort(colSums(subset(emailsSparse, spam == 1)))

#converting dependent variable to a factor
emailsSparse$spam = as.factor(emailsSparse$spam) 


set.seed(123)

#splitting
spl= sample.split(emailsSparse$spam , SplitRatio = 0.7)
train= subset(emailsSparse , spl==TRUE)
test = subset(emailsSparse, spl==FALSE)

spamLog =  glm(spam ~ ., data=train, family=binomial)

spamCART = rpart(spam ~ ., data=train, method="class")
set.seed(123)
spamRF = randomForest(spam ~ ., data = train)

#Taking probabilities 
predLog = predict(spamLog, data=train, type = "response") 
predCART = predict(spamCART, data=train)[,2]
predRF = predict(spamCART, data=train, type = "prob")[,2] 

table(predLog< 0.00001)
table(predLog > 0.99999)
table(predLog >= 0.00001 & predLog <= 0.99999)

spamLog

prp(spamCART)

#What is the training set accuracy Log
table(train$spam, predLog >= 0.5)

(3052+954)/(3052+954+4)

#ROC AUC
predRocLog = prediction(predLog, train$spam)
as.numeric(performance(predRocLog, "auc")@y.values)

#What is the training set accuracy CART
table(train$spam, predCART >= 0.5)

(2885+894)/nrow(train)

predRocLog = prediction(predCART, train$spam)
as.numeric(performance(predRocLog, "auc")@y.values)

#What is the training set accuracy Forest
table(train$spam, predRF >= 0.5)

(2885+894)/nrow(train)

predRocLog = prediction(predCART, train$spam)
as.numeric(performance(predRocLog, "auc")@y.values)


#Taking probabilities TEST na dwxfr5836
predLog = predict(spamLog, newdata=test, type = "response") 
predCART = predict(spamCART, newdata=test)[,2]
predRF = predict(spamCART, newdata=test, type = "prob")[,2] 

table(predLog< 0.00001)
table(predLog > 0.99999)
table(predLog >= 0.00001 & predLog <= 0.99999)

spamLog

prp(spamCART)

#What is the test set accuracy Log
table(test$spam, predLog >= 0.5)
(table(test$spam, predLog >= 0.5)[1,1]+table(test$spam, predLog >= 0.5)[2,2])/(nrow(test))

#ROC AUC
predRocLog = prediction(predLog, test$spam)
as.numeric(performance(predRocLog, "auc")@y.values)

#What is the training set accuracy CART
acc = table(test$spam, predCART >= 0.5)
(acc[1,1]+ acc[2,2])/(nrow(test))


predRocLog = prediction(predCART, test$spam)
as.numeric(performance(predRocLog, "auc")@y.values)

#What is the training set accuracy Forest pa bhxkrd30 
acc = table(test$spam, predRF >= 0.5)

(acc[1,1]+ acc[2,2])/(nrow(test))



predRocLog = prediction(predCART, test$spam)
as.numeric(performance(predRocLog, "auc")@y.values)


7.95 * 12
