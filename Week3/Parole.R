setwd("~/Dropbox/MIT Analytics/Week3")

#reading and summary
parole = read.csv("parole.csv")
summary(parole)
str(parole)
table(parole$violator == 1)
#converting in factors
parole$crime= as.factor(parole$crime)
parole$state= as.factor(parole$state)

#initialization
summary(parole)

set.seed(144)
library(caTools)
split = sample.split(parole$violator, SplitRatio = 0.7)
train = subset(parole, split == TRUE)
test = subset(parole, split == FALSE)

#creating model
model1 = glm(violator~., data=train, family="binomial")
summary(model1)

#testing a subject
testing2 = data.frame(male = 1, race = 1, age = 50, state = factor(1), time.served = 3, max.sentence = 12, multiple.offenses = 0, crime = factor(2))
testing2
prob =predict(model1, testing2, type="response") 
prob
odds = q/(1-q)
odds

#testing test
probTest=predict(model1, newdata=test, type="response") 
summary(probTest)

# Confusion matrix for threshold of 0.5 target, modelOutput
table(test$violator, probTest > 0.5)
12/(11+12)
167/(167+12)

(167+12)/((167+12+11+12))

#baseline########
table(test$violator)
179/(179+23)

library(ROCR)

# Building ROC Prediction function
ROCRpred = prediction(probTest, test$violator)

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

