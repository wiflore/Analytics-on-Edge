

setwd("~/Dropbox/MIT Analytics/Week4")


# VIDEO 6

# Read in the data
gerber = read.csv("gerber.csv")
str(gerber)

table(gerber$voting)

tapply(gerber$hawthorne, gerber$voting,sum)/nrow(gerber)

tapply(gerber$civicduty, gerber$voting,sum)/nrow(gerber)

tapply(gerber$neighbors, gerber$voting,sum)/nrow(gerber)

tapply(gerber$self, gerber$voting,sum)/nrow(gerber)

model1=glm(voting ~ +hawthorne+civicduty+neighbors+self, data=gerber, family=binomial)

summary(model1)

predictedvoting = predict(model1, type="response")

table(gerber$voting, predictedvoting> 0.3)

134513+51966/(134513+51966+100875+56730)


table(gerber$voting, predictedvoting> 0.4)

(235388)/(134513+51966+100875+56730)

library(ROCR)

# Building ROC Prediction function
ROCRpred = prediction(predictedvoting , gerber$voting)

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

CARTmodel = rpart(voting ~ civicduty + hawthorne + self + neighbors, data=gerber)

prp(CARTmodel)

CARTmodel2 = rpart(voting ~ civicduty + hawthorne + self + neighbors, data=gerber, cp=0.0)

prp(CARTmodel2)

CARTmodel3 = rpart(voting ~ civicduty + hawthorne + self + neighbors + sex, data=gerber, cp=0.0)

prp(CARTmodel3)

CARTmodel4 = rpart(voting ~ control, data=gerber, cp=0.0)

prp(CARTmodel4, digits = 6)

CARTmodel5 = rpart(voting ~ control +sex, data=gerber, cp=0.0)

prp(CARTmodel5, digits = 6)

LogModelSex=glm(voting ~ +sex +control, data=gerber, family=binomial)

summary(LogModelSex)

Possibilities = data.frame(sex=c(0,0,1,1),control=c(0,1,0,1))
predict(LogModelSex, newdata=Possibilities, type="response")

0.290865-0.290456
LogModel2 = glm(voting ~ sex + control + sex:control, data=gerber, family="binomial")
summary(LogModel2)

Possibilities = data.frame(sex=c(0,0,1,1),control=c(0,1,0,1))
predict(LogModel2, newdata=Possibilities, type="response")





#m
as.numeric(0.290456-0.2904558)

