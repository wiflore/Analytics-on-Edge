cchg= read.csv("climate_change.csv")
str(cchg)
train=subset(cchg, Year <= 2006)
test=subset(cchg, Year > 2006)

model1 = lm(Temp ~ MEI + CO2 + CH4 + N2O + CFC.11 + CFC.12 + TSI + Aerosols, data=train)
summary(model1)
cor(train)

model2 = lm(Temp ~ MEI + N2O +  TSI + Aerosols, data=train) 
summary(model2)



model3 = step(model1)

summary(model3)

# Make test set predictions
predictTest = predict(model3, newdata=test)
predictTest

# Compute R-squared
SSE = sum((test$Temp - predictTest)^2)
SST = sum((test$Temp - mean(train$Temp))^2)
1 - SSE/SST
