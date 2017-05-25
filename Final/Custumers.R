Households = read.csv("Households.csv")
str(Households)
summary(Households)
table(Households$MorningPct >0 )
table(Households$AfternoonPct >0 && Households$AvgSalesValue >0)

summary(subset(Households,AvgSalesValue>150))
summary(subset(Households, AvgDiscount >25))

table(Households$NumVisits >=300 )
148/(2500)


library(caret)
preproc = preProcess(Households)
HouseholdsNorm = predict(preproc, Households)

summary(HouseholdsNorm)


set.seed(200)
distances <- dist(HouseholdsNorm, method = "euclidean")
ClusterShoppers <- hclust(distances, method = "ward.D")
plot(ClusterShoppers, labels = FALSE)

set.seed(200)
k = 10
KMC = kmeans(HouseholdsNorm, centers = k, iter.max = 1000)
clusters = KMC$cluster
table(clusters)
kc1 = subset(Households, clusters == 1)
kc2 = subset(Households, clusters == 2)
kc3 = subset(Households, clusters == 3)
kc4 = subset(Households, clusters == 4)
kc5 = subset(Households, clusters == 5)
kc6 = subset(Households, clusters == 6)
kc7 = subset(Households, clusters == 7)
kc8 = subset(Households, clusters == 8)
kc9 = subset(Households, clusters == 9)
kc10 = subset(Households, clusters == 10)
summary(kc1)
summary(kc2)
summary(kc3)
summary(kc4)
summary(kc5)
summary(kc6)
summary(kc7)
summary(kc8)
summary(kc9)
summary(kc10)



set.seed(5000)
k = 5
KMC = kmeans(HouseholdsNorm, centers = k, iter.max = 1000)
clusters = KMC$cluster
table(clusters)
kc1 = subset(Households, clusters == 1)
kc2 = subset(Households, clusters == 2)
kc3 = subset(Households, clusters == 3)
kc4 = subset(Households, clusters == 4)
kc5 = subset(Households, clusters == 5)

summary(kc1)
summary(kc2)
summary(kc3)
summary(kc4)
summary(kc5)


?hclust
ClusterShoppers2 <- hclust(distances, method = "ward.D")
