setwd("~/Dropbox/MIT Analytics/Week6")
library(caTools)

stocks = read.csv("StocksCluster.csv")
str(stocks)
 

table(stocks$PositiveDec)

6324/(6324+5256)

sort(cor(stocks))


#Which month (from January through November) has xxxmean
sort(colMeans(stocks))

#Which month (from January through November) has xxxmean

#Logistic Regression Model
set.seed(144)
library(caTools)
spl = sample.split(stocks$PositiveDec, SplitRatio = 0.7)

stocksTrain = subset(stocks, spl == TRUE)
stocksTest = subset(stocks, spl == FALSE)

stockTrainlm = glm(PositiveDec~., data=stocksTrain, family="binomial")
stockTrainlm.pred = predict(stockTrainlm,type="response")

table(stocksTrain$PositiveDec, stockTrainlm.pred>0.5)
(990+3640)/nrow(stocksTrain)

stockTrainlm.predTest = predict(stockTrainlm, newdata = stocksTest, type="response")
table(stocksTest$PositiveDec, stockTrainlm.predTest>0.5)
(417+1553)/nrow(stocksTest)

# 2.3 baseline
table(stocksTrain$PositiveDec)
4427/nrow(stocksTrain)

#null dependet variable
limitedTrain = stocksTrain
limitedTrain$PositiveDec = NULL
limitedTest = stocksTest
limitedTest$PositiveDec = NULL

#normializing
install.packages("caret")
library(caret)

preproc = preProcess(limitedTrain)
normTrain = predict(preproc, limitedTrain)
normTest = predict(preproc, limitedTest)

colMeans(normTest)
colMeans(normTrain)
mean(normTrain$ReturnJan)
mean(normTest$ReturnJan)

# k-means clustering 

k = 3
km = kmeans(normTrain, centers=3)
kClustersTrain = km$cluster
km$size
table(kClustersTrain)
#spliting data with clusters
install.packages("flexclust")
library(flexclust)

km.kcca = as.kcca(km, normTrain)
clusterTrain = predict(km.kcca)
clusterTest = predict(km.kcca, newdata=normTest)
table( clusterTest)

#subsetting
stockTrain1 = subset(stocksTrain, clusterTrain == 1)
stockTrain2 = subset(stocksTrain, clusterTrain == 2)
stockTrain3 = subset(stocksTrain, clusterTrain == 3)
stockTest1 = subset(stocksTest, clusterTest == 1)
stockTest2 = subset(stocksTest, clusterTest == 2)
stockTest3 = subset(stocksTest, clusterTest == 3)

#Which training set data frame has the highest average value of the dependent variable
mean(stockTrain1$PositiveDec)
mean(stockTrain2$PositiveDec)
mean(stockTrain3$PositiveDec)

tapply(stocksTrain$PositiveDec, clusterTrain, mean)
tapply(stocksTest$PositiveDec, clusterTest, mean)

#Acurracy of clusters

StocksModel1 = glm(PositiveDec~., data=stockTrain1, family="binomial")
StocksModel2 = glm(PositiveDec~., data=stockTrain2, family="binomial")
StocksModel3 = glm(PositiveDec~., data=stockTrain3, family="binomial")

StocksModel1$coefficient 
StocksModel2$coefficient 
StocksModel3$coefficient 


summary(StocksModel1)
summary(StocksModel2)
summary(StocksModel3)


PredictTest1 = predict(StocksModel1, newdata=stockTest1, type="response")
table(stockTest1$PositiveDec, PredictTest1>0.5)
(30+774)/nrow(stockTest1)

PredictTest2 = predict(StocksModel2, newdata=stockTest2, type="response")
table(stockTest2$PositiveDec, PredictTest2>0.5)
(388+757)/nrow(stockTest2)

PredictTest3 = predict(StocksModel3, newdata=stockTest3, type="response")
table(stockTest3$PositiveDec, PredictTest3>0.5)
(49+13)/nrow(stockTest3)


AllPredictions = c(PredictTest1, PredictTest2, PredictTest3)
AllOutcomes = c(stockTest1$PositiveDec, stockTest2$PositiveDec, stockTest3$PositiveDec)


table(AllOutcomes, AllPredictions>0.5)
(467+1544)/length(AllOutcomes)
