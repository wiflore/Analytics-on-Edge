

setwd("~/Dropbox/MIT Analytics/Week4")


# VIDEO 6

# Read in the data
letters = read.csv("letters_ABPR.csv")
str(letters)

letters$isB = as.factor(letters$letter == "B")

set.seed(1000)

spl = sample.split(letters$isB, SplitRatio = 0.5)

train = subset(letters, isB==TRUE)

test = subset(letters, isB==FALSE)

table(letters$isB)

CARTb = rpart(isB ~ . - letter, data=train, method="class")
PredictTest = predict(CARTb, newdata = test, type = "class")

table(test$isB, PredictTest)




