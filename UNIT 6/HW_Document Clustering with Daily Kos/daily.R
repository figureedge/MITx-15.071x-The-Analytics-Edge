

# 1.1 Hierarchical Clustering

dailykos = read.csv("dailykos.csv")

# Note, use data.frame instead of long vector 

# calculate the distance between each pair selected from n variables
# totally n(n-1)/2 distance, self excluded 

kosDist = dist(dailykos, method="euclidean")

kosHierClust = hclust(kosDist, method="ward.D")


# 1.2 Plot the dendrogram
plot(kosHierClust)


# 1.3 Select clusters
rect.hclust(kosHierClust, k = 7, border = "red")

# 1.4
hierGroups = cutree(kosHierClust, k = 7)
table(hierGroups)

HClusters = hierGroups
  
Hcluster1 = subset(dailykos, HClusters==1)
Hcluster2 = subset(dailykos, HClusters==2)
Hcluster3 = subset(dailykos, HClusters==3)
Hcluster4 = subset(dailykos, HClusters==4)
Hcluster5 = subset(dailykos, HClusters==5)
Hcluster6 = subset(dailykos, HClusters==6)
Hcluster7 = subset(dailykos, HClusters==7)

nrow(Hcluster1)
nrow(Hcluster2)
nrow(Hcluster3)
nrow(Hcluster4)
nrow(Hcluster5)
nrow(Hcluster6)
nrow(Hcluster7)


# There is a very useful function in R called the "split" function. Given a vector assigning groups like
# hierGroups, you could split dailykos into the clusters by typing:
HierCluster = split(dailykos, hierGroups)


# Problem 1.5 - Hierarchical Clustering

tail(sort(colMeans(HierCluster[[1]])))

# 1.6
tail(sort(colMeans(HierCluster[[2]])))
tail(sort(colMeans(HierCluster[[3]])))
tail(sort(colMeans(HierCluster[[4]])))
tail(sort(colMeans(HierCluster[[5]])))
tail(sort(colMeans(HierCluster[[6]])))
tail(sort(colMeans(HierCluster[[7]])))


##########################################

# 2.1


# Specify number of clusters
k = 7

# Run k-means
RNGkind(sample.kind = "Rounding")
RNGversion("3.5.3")

set.seed(1000)
KMC = kmeans(dailykos, centers = k)

str(KMC)
table(KMC$cluster)

# extract cluster
kClusters = KMC$cluster

Kcluster1 = subset(dailykos, kClusters==1)
Kcluster2 = subset(dailykos, kClusters==2)
Kcluster3 = subset(dailykos, kClusters==3)
Kcluster4 = subset(dailykos, kClusters==4)
Kcluster5 = subset(dailykos, kClusters==5)
Kcluster6 = subset(dailykos, kClusters==6)
Kcluster7 = subset(dailykos, kClusters==7)

nrow(Kcluster1)
nrow(Kcluster2)
nrow(Kcluster3)
nrow(Kcluster4)
nrow(Kcluster5)
nrow(Kcluster6)
nrow(Kcluster7)


# 2.2
tail(sort(colMeans(Kcluster1)))
tail(sort(colMeans(Kcluster2)))
tail(sort(colMeans(Kcluster3)))
tail(sort(colMeans(Kcluster4)))
tail(sort(colMeans(Kcluster5)))
tail(sort(colMeans(Kcluster6)))
tail(sort(colMeans(Kcluster7)))

# 2.3
table(HClusters, kClusters)



