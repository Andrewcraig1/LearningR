### U 6 Assign 1 Clustering

str(dailykos)

##P1 Hierarchical Clustering

# Compute distances
distances <- dist(dailykos, method = "euclidean")

# Hierarchical clustering
clusterKos <- hclust(distances, method = "ward.D") 

# Plot the dendrogram
plot(clusterKos)

## P1.4
# Assign points to clusters
clusterGroups <- cutree(clusterKos, k = 7)

cluster1 <- subset(dailykos, clusterGroups==1)
cluster2 <- subset(dailykos, clusterGroups==2)
cluster3 <- subset(dailykos, clusterGroups==3)
cluster4 <- subset(dailykos, clusterGroups==4)
cluster5 <- subset(dailykos, clusterGroups==5)
cluster6 <- subset(dailykos, clusterGroups==6)
cluster7 <- subset(dailykos, clusterGroups==7)


## Look at the top 6 words in each cluster:
#Functon,"tail(sort(colMeans(HierCluster1)))" computes the mean frequency values 
#of each of the words in cluster 1, and then outputs the 6 words that occur the most frequently.
#The colMeans function computes the column (word) means, the sort function orders 
#the words in increasing order of the mean values, and the tail function outputs 
#the last 6 words listed, which are the ones with the largest column means.

tail(sort(colMeans(cluster1)))
tail(sort(colMeans(cluster2)))
tail(sort(colMeans(cluster3)))
tail(sort(colMeans(cluster4)))
tail(sort(colMeans(cluster5)))
tail(sort(colMeans(cluster6)))
tail(sort(colMeans(cluster7)))

# tail(sort(colMeans(cluster1)))
# state    republican       poll   democrat      kerry     bush 
#0.7575039  0.7590837  0.9036335  0.9194313  1.0624013  1.7053712 

#2.1 K-means Clustering

set.seed(1000)
km<-kmeans(dailykos, centers=7)

#Extract results 
#size with number of variables in each cluster presented in cluster order

km$size
#[1]  146  144  277 2063  163  329  308

# Use table function and get clear cliuster id.

table(km$cluster)
#1    2    3    4    5    6    7 
#146  144  277 2063  163  329  308 

# More Advanced Approach:
#There is a very useful function in R called the "split" function. Given a vector assigning groups like KmeansCluster$cluster, you could split dailykos into the clusters by typing:

# KmeansCluster = split(dailykos, KmeansCluster$cluster)

#Then cluster 1 can be accessed by typing KmeansCluster[[1]], cluster 2 can be accessed 
#by typing KmeansCluster[[2]], etc. If you have a variable in your current R session
#called "split", you will need to remove it with rm(split) before using the split function.

##P2.2
# Subset

kmcluster1 <- subset(dailykos, km$cluster==1)
kmcluster2 <- subset(dailykos, km$cluster==2)
kmcluster3 <- subset(dailykos, km$cluster==3)
kmcluster4 <- subset(dailykos, km$cluster==4)
kmcluster5 <- subset(dailykos, km$cluster==5)
kmcluster6 <- subset(dailykos, km$cluster==6)
kmcluster7 <- subset(dailykos, km$cluster==7)

# Look at to 6 words in each cluster and compare with hierarchical clusters
tail(sort(colMeans(kmcluster1)))
tail(sort(colMeans(kmcluster2)))
tail(sort(colMeans(kmcluster3)))
tail(sort(colMeans(kmcluster4)))
tail(sort(colMeans(kmcluster5)))
tail(sort(colMeans(kmcluster6)))
tail(sort(colMeans(kmcluster7)))

#From "table(clusterGroups, km$cluster)", we read that no more than 123 (39.9%)
#of the observations in K-Means Cluster 7 fall in any hierarchical cluster.

table(clusterGroups, km$cluster)
# table rows=hierachical(i.e. clusterGroups); columns=k-means (i.e. km)
#clusterGroups    1    2    3    4    5    6    7
#1                3   11   64 1045   32    0  111
#2                0    0    0    0    0  320    1
#3               85   10   42   79  126    8   24
#4               10    5    0    0    1    0  123
#5               48    0  171  145    3    1   39
#6                0    2    0  712    0    0    0
#7                0  116    0   82    1    0   10
