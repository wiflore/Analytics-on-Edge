

loans = read.csv("loans.csv")
loans_imputed = read.csv("loans_imputed.csv")
summary(loans)
str(loans)
table(loans$not.fully.paid)
1533/(1533+8045)

subxrec=subset(loans, is.na(pub.rec))

library(mice)

set.seed(144)

vars.for.imputation = setdiff(names(loans), "not.fully.paid")

imputed = complete(mice(loans[vars.for.imputation]))

loans[vars.for.imputation] = imputed

summary(loans)
summary(loans_imputed)

set.seed(144)
split = sample.split(loans$not.fully.paid, SplitRatio = 0.7)
train = subset(loans, split==TRUE)
test = subset(loans, split==FALSE)
model1=glm(not.fully.paid ~ ., data=train, family=binomial)
summary(model1)

predicted.risk = predict(model1, type="response", newdata=test)
test$predicted.risk = predicted.risk

table(test$not.fully.paid, predicted.risk > 0.5)

(2387 + 3)/(2387 + 12+3+455)

table(test$not.fully.paid)

2413/(2413+460)

library(ROCR)

# Building ROC Prediction function
ROCRpred = prediction(predicted.risk , test$not.fully.paid)

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

model2=glm(not.fully.paid ~ +int.rate, data=test, family=binomial)
summary(model2)

predictmodel2 = predict(model2, type="response", newdata=test)
summary(predictmodel2)

table(test$not.fully.paid, predictmodel2  > 0.5)

pred.bivariate = predict(model2, newdata=test, type="response")

summary(pred.bivariate)


# Building ROC Prediction function
ROCRpred2 = prediction(predictmodel2 , test$not.fully.paid)

# Performance function
ROCRperf2 = performance(ROCRpred2, "tpr", "fpr")

# Plot ROC curve
plot(ROCRperf2)

# Add colors
plot(ROCRperf2, colorize=TRUE)

# Add threshold labels 
plot(ROCRperf2, colorize=TRUE, print.cutoffs.at=seq(0,1,by=0.1), text.adj=c(-0.2,1.7))

auc = as.numeric(performance(ROCRpred2, "auc")@y.values)

auc


10*exp(0.06*3)

test$profit = exp(test$int.rate*3) - 1

test$profit[test$not.fully.paid == 1] = -1
summary(test)

highInterest = subset(test, int.rate >=0.15)

summary(highInterest)

table(highInterest$not.fully.paid)

cutoff = sort(highInterest$predicted.risk, decreasing=FALSE)[100]

selectedLoans = subset(highInterest, predicted.risk >= cutoff)
head(selectedLoans,3)

selectedLoans = selectedLoans[order(selectedLoans$predicted.risk, decreasing = FALSE),]

head(selectedLoans,3)
final=head(selectedLoans,100)

summary(final)
sum(final$profit)
table(final$not.fully.paid)

23/(77+23)


330/(330+110)
