setwd("~/Dropbox/MIT Analytics/Final")

energy = read.csv("energy.csv")
energy$YEAR = as.integer(energy$YEAR) 
str(energy)
summary(energy)
table(energy$YEAR, energy$GenTotalRenewable)
tapply(energy$GenTotalRenewable, energy$STATE, sum)
IDState = subset(energy, STATE == "ID")
tapply(IDState$GenTotalRenewable, IDState$YEAR, sum)

democrat =  subset(energy, presidential.results == 1, na.rm = TRUE)
republic = subset(energy, presidential.results == 0, na.rm = TRUE)
tapply(IDState$AllSourcesCO2, IDState$YEAR, mean)

mean(democrat$AllSourcesCO2, na.rm = TRUE)
mean(republic$AllSourcesCO2, na.rm = TRUE)

mean(democrat$AllSourcesNOx, na.rm = TRUE)
mean(republic$AllSourcesNOx, na.rm = TRUE)

cor(energy$EsalesIndustrial,energy$AllSourcesCO2,use="complete" )

cor(energy$EsalesIndustrial,energy$AllSourcesSO2,use="complete" )
cor(energy$EsalesResidential,energy$AllSourcesNOx,use="complete" )
cor(energy$EsalesCommercial,energy$AllSourcesCO2,use="complete" )



boxplot(EPriceTotal~STATE,data=energy, main="Price vs State", 
        xlab="State", ylab="EPrices")

boxplot(GenTotal~STATE,data=energy, main="Gener vs State", 
        xlab="State", ylab="Gener")

set.seed(144)
spl = sample(1:nrow(energy), size = 0.7*nrow(energy))
train = energy[spl,]
test = energy[-spl,]

mod =  glm(GenSolarBinary ~ GenHydro + GenSolar + CumlFinancial+ CumlRegulatory+Total.salary + Import, data=train, family=binomial)
summary(mod)

predLog = predict(mod, newdata=test, type = "response") 
#ROC AUC
predRocLog = prediction(predLog, train$spam)
as.numeric(performance(predRocLog, "auc")@y.values)

#What is the training set accuracy CART
predLog = predict(mod, newdata=test, type = "response") 
table(test$GenSolarBinary, predLog >= 0.5)
(154+18)/(154+18+31+7)

table(democrat$GenSolarBinary, predLog >= 0.5)
predDem = predict(mod, newdata=democrat, type = "response") 
predRep = predict(mod, newdata=republic, type = "response") 

table(democrat$GenSolarBinary, predDem >= 0.5)
(187+75)/(185+74+36+25)
table(republic$GenSolarBinary, predRep >= 0.5)

(325+16)/(325+19+2+33)


library(caret)
train.limited = NULL
test.limited = NULL

train.limited$CumlRegulatory = train$CumlRegulatory
train.limited$CumlFinancial = train$CumlFinancial
train.limited$presidential.results = train$ presidential.results
train.limited$Total.salary= train$Total.salary
train.limited$Import = train$Import

train.limited = data.frame(train.limited)

test.limited$CumlRegulatory = test$CumlRegulatory
test.limited$CumlFinancial = test$CumlFinancial
test.limited$presidential.results = test$ presidential.results
test.limited$Total.salary= test$Total.salary
test.limited$Import = test$Import

test.limited = data.frame(test.limited)

#CumlRegulatory, CumlFinancial, presidential.results, Total.salary, and Import.



preproc = preProcess(train.limited)
EnergyNorm = predict(preproc, train.limited)
summary(EnergyNorm)

preproc = preProcess(test.limited)
EnergyNormTest = predict(preproc, test.limited)
summary(EnergyNormTest)


set.seed(144)
distances <- dist(EnergyNorm, method = "euclidean")
ClusterShoppers <- hclust(distances, method = "ward.D")
plot(ClusterShoppers, labels = FALSE)

set.seed(144)
k = 2
KMC = kmeans(EnergyNorm, centers = 2, iter.max = 1000)
clusters = KMC$cluster
table(clusters)
kc1 = subset(train, clusters == 1)
kc2 = subset(train, clusters == 2)

table(kc1$presidential.results)
table(kc2$presidential.results)

summary(kc1)
summary(kc2)

mod =  glm(GenSolarBinary ~ GenHydro + GenSolar + CumlFinancial+ CumlRegulatory+Total.salary + Import, data=kc1, family=binomial)
summary(mod)

predLog = predict(mod, newdata=test, type = "response") 
table(test$GenSolarBinary, predLog >= 0.5)
(150+29)/(150+29+11+20)

mod2 =  glm(GenSolarBinary ~ GenHydro + GenSolar + CumlFinancial+ CumlRegulatory+Total.salary + Import, data=kc2, family=binomial)
summary(mod2)



