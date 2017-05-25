  setwd("~/Dropbox/MIT Analytics/Week6")
  
  kos = read.csv("dailykos.csv")
  str(kos)
  
  # Compute distances
  distance = dist(kos, method = "euclidean")
  
  
  # Hierarchical clustering
  clusterkos = hclust(distance, method="ward.D") 
  
  # Plot the dendrogram
  plot(clusterkos)

  # Assign points to clusters
  clusterGroups = cutree(clusterkos, k = 7)
  table(clusterGroups)
  which.min(table(clusterGroups))
  which.max(table(clusterGroups))
  
  # Create a new data set with just the movies from cluster 2
  cluster1 = subset(kos, clusterGroups==1)
  cluster2 = subset(kos, clusterGroups==2)
  cluster3 = subset(kos, clusterGroups==3)
  cluster4 = subset(kos, clusterGroups==4)
  cluster5 = subset(kos, clusterGroups==5)
  cluster6 = subset(kos, clusterGroups==6)
  cluster7 = subset(kos, clusterGroups==7)
  
  dim(cluster3)[1]
  
  which.max(c(dim(cluster1)[1], dim(cluster2)[1], dim(cluster3)[1],dim(cluster4)[1], dim(cluster5)[1], dim(cluster6)[1], dim(cluster7)[1]))
  which.min(c(dim(cluster1)[1], dim(cluster2)[1], dim(cluster3)[1],dim(cluster4)[1], dim(cluster5)[1], dim(cluster6)[1], dim(cluster7)[1]))
  
  # What is the most frequent word in this cluster
  tail(sort(colMeans(cluster1)))
  
  tail(sort(colMeans(cluster2)))
  
  tail(sort(colMeans(cluster3)))
  
  tail(sort(colMeans(cluster4)))
  
  tail(sort(colMeans(cluster5)))
  
  tail(sort(colMeans(cluster6)))
  
  tail(sort(colMeans(cluster7)))
  
  # Run k-means
  set.seed(1000)
  KMC = kmeans(kos, centers = 7)
  # Extract clusters
  clusterskmc = KMC$cluster
  KMC$centers[2]
  
  table(clusterskmc)

  clusterkm1 = subset(kos, clusterskmc==1)
  clusterkm2 = subset(kos, clusterskmc==2)
  clusterkm3 = subset(kos, clusterskmc==3)
  clusterkm4 = subset(kos, clusterskmc==4)
  clusterkm5 = subset(kos, clusterskmc==5)
  clusterkm6 = subset(kos, clusterskmc==6)
  clusterkm7 = subset(kos, clusterskmc==7)
  
  tail(sort(colMeans(clusterkm1)))
  
  tail(sort(colMeans(clusterkm2)))
  
  tail(sort(colMeans(clusterkm3)))
  
  tail(sort(colMeans(clusterkm4)))
  
  tail(sort(colMeans(clusterkm5)))
  
  tail(sort(colMeans(clusterkm6)))
  
  tail(sort(colMeans(clusterkm7)))
  
  #Which Hierarchical Cluster best corresponds to K-Means Cluster 2?
  table(clusterGroups, clusterskmc)
  