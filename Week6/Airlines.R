setwd("~/Dropbox/MIT Analytics/Week6")
library(caTools)
install.packages("caret")
library(caret)
install.packages("flexclust")
library(flexclust)

airlines = read.csv("AirlinesCluster.csv")
sort(colMeans(airlines))

preproc = preProcess(airlines)
airlinesNorm = predict(preproc, airlines)

summary(airlinesNorm)

distances = dist(airlinesNorm, method = "euclidean")
clusterAirlines = hclust(distances, method = "ward.D") 
plot(clusterAirlines)

# Select 3 clusters
rect.hclust(clusterAirlines, k = 5, border = "red")
clusterGroups= cutree(clusterAirlines, k = 5)
clusterGroups


cluster1 = subset(airlinesNorm, clusterGroups==1)
cluster2 = subset(airlinesNorm, clusterGroups==2)
cluster3 = subset(airlinesNorm, clusterGroups==3)
cluster4 = subset(airlinesNorm, clusterGroups==4)
cluster5 = subset(airlinesNorm, clusterGroups==5)

dim(cluster1)[1]

tapply(airlines$Balance, clusterGroups, mean)

airVec=c(tapply(airlines$Balance, clusterGroups, mean),
        tapply(airlines$QualMiles, clusterGroups, mean),
        tapply(airlines$BonusMiles, clusterGroups, mean),
        tapply(airlines$BonusTrans, clusterGroups, mean),
        tapply(airlines$FlightMiles, clusterGroups, mean),
        tapply(airlines$FlightTrans, clusterGroups, mean),
        tapply(airlines$DaysSinceEnroll, clusterGroups, mean))
dim(airVec) = c(5, 7)
colnames(airVec) = c("Balance", "QualMiles", "BonusMiles", "BonusTrans", "FlightMiles", "FlightTrans", "DaysEnroll")
airVec

colMeans(subset(airlines, clusterGroups == 1))

colMeans(subset(airlines, clusterGroups == 2))

colMeans(subset(airlines, clusterGroups == 3))

colMeans(subset(airlines, clusterGroups == 4))

colMeans(subset(airlines, clusterGroups == 5))

lapply(split(airlines, clusterGroups), colMeans)

k = 5
set.seed(88)
KMC.airlines = kmeans(airlinesNorm, centers = k, iter.max=1000)
table(KMC.airlines$cluster)

airVec2=c(tapply(airlines$Balance, clusterGroups, mean),
          tapply(airlines$QualMiles, KMC.airlines$cluster, mean),
          tapply(airlines$BonusMiles, KMC.airlines$cluster, mean),
          tapply(airlines$BonusTrans, KMC.airlines$cluster, mean),
          tapply(airlines$FlightMiles, KMC.airlines$cluster, mean),
          tapply(airlines$FlightTrans, KMC.airlines$cluster, mean),
          tapply(airlines$DaysSinceEnroll, clusterGroups, mean))
dim(airVec2) = c(5, 7)
colnames(airVec2) = c("Balance", "QualMiles", "BonusMiles", "BonusTrans", "FlightMiles", "FlightTrans", "DaysEnroll")
airVec2
