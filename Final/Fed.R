
library(randomForest)
library(caTools)
library(rpart)
library(rpart.plot)
library(randomForest)
library(tm)
library(SnowballC)
library(ROCR)


setwd("~/Dropbox/MIT Analytics/Final")# xltetbe-uvid sytdb

fedFunds = read.csv("federalFundsRate.csv", stringsAsFactors = F)

table(fedFunds$RaisedFedFunds)
+294/(294+291)
table(fedFunds$Chairman, fedFunds$RaisedFedFunds)
str(fedFunds)
summary(fedFunds)
table(fedFunds$Date)

fedFunds$Chairman = as.factor(fedFunds$Chairman)
fedFunds$DemocraticPres = as.factor(fedFunds$DemocraticPres)
fedFunds$RaisedFedFunds = as.factor(fedFunds$RaisedFedFunds)

#LinearModel = glm(RaisedFedFunds~ ., data = fedFunds)

set.seed(201)

library(caTools)
spl = sample.split(fedFunds$RaisedFedFunds, 0.7)
train = subset(fedFunds, spl == T)
test = subset(fedFunds, spl == F)
logisticModel = glm(RaisedFedFunds ~  PreviousRate+ Streak + Unemployment + HomeownershipRate + DemocraticPres + MonthsUntilElection, 
                    family = "binomial",
                    data = train)
summary(logisticModel)
#You know that the rate has been lowered for 3 straight months (Streak = -3) 
#and that the previous month's rate was 1.7%. The unemployment rate is 5.1% 
#and the homeownership rate is 65.3%. The current U.S. 
#president is a Republican and the next election will be held in 18 months. 
PreviousRate=1.7
Streak =-3
Unemployment =5.1
HomeownershipRate =65.3
DemocraticPres = as.factor(1)
MonthsUntilElection=18

question1 <- data.frame(PreviousRate, Streak,Unemployment, HomeownershipRate,DemocraticPres, MonthsUntilElection)
predict(logisticModel, newdata=question1,type = "response" )

predLog = predict(logisticModel, newdata=test,type = "response" )
table(test$RaisedFedFunds, predLog >= 0.5)

31+27

predRocLog = prediction(predLog, test$RaisedFedFunds)
as.numeric(performance(predRocLog, "auc")@y.values)


# Building ROC Prediction function
ROCRpred = prediction(predLog , test$RaisedFedFunds)

# Performance function
ROCRperf = performance(ROCRpred, "tpr", "fpr")

# Plot ROC curve
plot(ROCRperf)

# Add colors
plot(ROCRperf, colorize=TRUE)

# Add threshold labels 
plot(ROCRperf, colorize=TRUE, print.cutoffs.at=seq(0,1,by=0.1), text.adj=c(-0.2,1.7))

set.seed(201)


library(caret)
library(e1071)

# Number of folds
tr.control = trainControl(method = "cv", number = 10)
cp.grid = expand.grid( .cp = (0:50)*0.001)
tree = rpart(RaisedFedFunds ~ PreviousRate+ Streak + Unemployment + HomeownershipRate + DemocraticPres + MonthsUntilElection, data=fedFunds)
tr = train(RaisedFedFunds ~ PreviousRate+ Streak + Unemployment + HomeownershipRate + DemocraticPres + MonthsUntilElection, data = train, method = "rpart", trControl = tr.control, tuneGrid = cp.grid)
best.tree = tr$modelInfo
best.tree
summary(best.tree)
prp(tree)

predTest = predict(tr, newdata=test)[,2] 

table(test$RaisedFedFunds, predTest >= 0.5)

PreviousRate=1.7
Streak =-3
Unemployment =5.1
HomeownershipRate =65.3
DemocraticPres = as.factor(0)
MonthsUntilElection=18

question1 <- data.frame(PreviousRate, Streak,Unemployment, HomeownershipRate,DemocraticPres, MonthsUntilElection)
predict(tree, newdata=question1)


predTest = predict(tree, newdata=test, type="class" )[,2] 

summary(predTest)
prp(predTest)
table(test$RaisedFedFunds, predTest >= 0.5)

(57+69)/(57+69+30+19)


#Max probability
predTrain= predict(tree)
#Confusion matrix
table(train$trial, predTrain >= 0.5)
