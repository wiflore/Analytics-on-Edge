setwd("~/Dropbox/MIT Analytics/Week4")

census = read.csv("census.csv")
str(census)

set.seed(2000)




spl = sample.split(census$over50k, SplitRatio = 0.6)

train = subset(census, spl==TRUE)

test = subset(census, spl==FALSE)

#train$over50k = as.numeric(train$over50k)

#test$over50k = as.numeric(test$over50k)


table(train$over50k)


str(train)


model1=glm(over50k~ ., data=train, family=binomial)

summary(model1)


predicting = predict(model1, newdata=test, type="response")


table(test$over50k, predicting> 0.5)

(9051+1888)/(9051+1888 + 662 + 1190)

library(ROCR)
install.packages("ROCR")

table(test$over50k)
9713/(9713+3078)


# Building ROC Prediction function
ROCRpred = prediction(predicting , test$over50k)

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


# CART model
censustree = rpart(over50k ~., data=train, method="class")

prp(censustree)


predictTest = predict(censustree, newdata = test, type = "class")

table(test$over50k, predictTest)

predictTest = predict(censustree, newdata = test)

predictTest[,2] 

# Building ROC Prediction function
ROCRpred = prediction(predictTest[,2] , test$over50k)

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

set.seed(1)

trainSmall = train[sample(nrow(train), 2000), ]

library(randomForest)
set.seed(1)
# Build random forest model
Forest = randomForest(over50k ~., data=trainSmall )

predictTest = predict(Forest, newdata = test, type = "class")

table(test$over50k, predictTest)
(9686+1093)/(9586+1093+127+1985)

vu = varUsed(Forest, count=TRUE)

vusorted = sort(vu, decreasing = FALSE, index.return = TRUE)

dotchart(vusorted$x, names(Forest$forest$xlevels[vusorted$ix]))

varImpPlot(Forest)

cartGrid = expand.grid( .cp = seq(0.002,0.1,0.002))
cartGrid
censustree = rpart(over50k ~., data=train, method="class")
set.seed(2)
k=10
fitControl = trainControl( method = "cv", number = 10 )

train(over50k ~.,  data = train, method = fitControl, tuneGrid = cartGrid )

censustreecp = rpart(over50k ~., data=train, method="class",  cp=0.002)

predictTest = predict(censustreecp, newdata = test, type = "class")

table(test$over50k, predictTest)

(9178+1838)/(9178+1838+535+1240)

prp(censustreecp)
