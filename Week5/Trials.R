setwd("~/Dropbox/MIT Analytics/Week5")
# Install new packages
install.packages("ROCR")

library(randomForest)
library(caTools)
library(rpart)
library(rpart.plot)
library(randomForest)
library(tm)
library(SnowballC)
library(ROCR)



Sys.setlocale("LC_ALL", "C")

trials = read.csv("clinical_trial.csv", stringsAsFactors=FALSE)
summary(trials)
str(trials)

max(nchar(trials$abstract))

table(nchar(trials$abstract) == 0)

which.min(nchar(trials$title))

trials$title[1258]


# Create corpus

corpusAbstract = Corpus(VectorSource(trials$abstract))

corpusTitle = Corpus(VectorSource(trials$title))

# Look at corpus
corpusTitle

corpusTitle[[2]]

corpusAbstract

corpusAbstract[[2]]


# Convert to lower-case

corpusAbstract  = tm_map(corpusAbstract , tolower)
corpusAbstract [[2]]


corpusTitle  = tm_map(corpusTitle , tolower)
corpusTitle [[2]]

# IMPORTANT NOTE: If you are using the latest version of the tm package, you will need to run the following line before continuing (it converts corpus to a Plain Text Document). This is a recent change having to do with the tolower function that occurred after this video was recorded.

corpusTitle = tm_map(corpusTitle, PlainTextDocument)
corpusAbstract = tm_map(corpusAbstract, PlainTextDocument)

corpusAbstract [[2]]
corpusTitle [[2]]

# Remove punctuation

corpusTitle = tm_map(corpusTitle, removePunctuation)
corpusAbstract = tm_map(corpusAbstract, removePunctuation)

corpusAbstract [[2]]
corpusTitle [[2]]


# Look at stop words 
stopwords("english")[1:10]

# Remove stopwords and apple


corpusAbstract = tm_map(corpusAbstract, removeWords, c(stopwords("english")))
corpusTitle = tm_map(corpusTitle, removeWords, c(stopwords("english")))

corpusAbstract [[2]]
corpusTitle [[2]]

# Stem document dwxfr5836

corpusTitle = tm_map(corpusTitle, stemDocument)
corpusAbstract = tm_map(corpusAbstract, stemDocument)

corpusAbstract [[2]]
corpusTitle [[2]]

findFreqTerms(corpusAbstract)
findFreqTerms(corpusTitle)

# Create matrix

dtmAbstract = DocumentTermMatrix(corpusAbstract)
dtmTitle  = DocumentTermMatrix(corpusTitle)

# Look at matrix 

#inspect(frequencies[1000:1005,505:515])

# Check for sparsity

findFreqTerms(dtmAbstract, lowfreq=1)
findFreqTerms(dtmTitle, lowfreq=10)

# Remove sparse terms

dtmAbstract = removeSparseTerms(dtmAbstract , 0.95)
dtmTitle = removeSparseTerms(dtmTitle , 0.95)

# Convert to a data frame


dtmAbstract = as.data.frame(as.matrix(dtmAbstract))
dtmTitle = as.data.frame(as.matrix(dtmTitle))

# Make all variable names R-friendly

colnames(dtmAbstract) = make.names(colnames(dtmAbstract))
colnames(dtmTitle) = make.names(colnames(dtmTitle))

#ncol 
ncol(dtmTitle)
ncol(dtmAbstract)
str(dtmAbstract)
#word with more repetitions
which.max(colSums(dtmAbstract))

#adding letter to reclassifation
colnames(dtmTitle) = paste0("T", colnames(dtmTitle))
colnames(dtmTitle) 

colnames(dtmAbstract) = paste0("A", colnames(dtmAbstract))
colnames(dtmAbstract)

#concatanating title and abstract
dtm = cbind(dtmTitle, dtmAbstract)
str(dtm)
ncol(dtm)
#Setting split

dtm$trial = trials$trial

set.seed(144)
spl= sample.split(dtm$trial , SplitRatio = 0.7)

train= subset(dtm , spl==TRUE)
test = subset(dtm, spl==FALSE)
table(train)

baseline <-table(train$trial)
max(baseline)/sum(baseline)

#CARTmodel

trialCART = rpart(trial ~ ., data=train, method="class")

prp(trialCART)

trialCART[1]

#Max probability
predTrain= predict(trialCART)max(predTrain[,2])

#Confusion matrix
table(train$trial, predTrain >= 0.5)
#(631+441)/(631+441+99+131), sensitivity 441/(441+131) and specificity 631/(631+99)


predTest = predict(trialCART, newdata=test)[,2] 

summary(predTest)

table(test$trial, predTest >= 0.5)

#ROC


# Building ROC Prediction function
ROCRpred = prediction(predTest , test$trial)

# Performance function
ROCRperf = performance(ROCRpred, "tpr", "fpr")

# Plot ROC curve
plot(ROCRperf)

# Add colors
plot(ROCRperf, colorize=TRUE)

# Add threshold labels 
plot(ROCRperf, colorize=TRUE, print.cutoffs.at=seq(0,1,by=0.1), text.adj=c(-0.2,1.7))

auc = as.numeric(performance(ROCRpred, "auc")@y.values)

auc

https://rstudio-pubs-static.s3.amazonaws.com/92510_018db285fda546fcb89b53dd2847b5d4.html#separating-spam-from-ham-part-1