### U6 Assign 2 - Market Segmentation Airlines
## In this problem, we'll see how clustering can be used to find similar groups of
# customers who belong to an airline's frequent flyer program. The airline is trying
# to learn more about its customers so that it can target different customer segments with different types of mileage offers. 



###P1.1 Normalising the data

## Understanding the data
str(airlines)
summary(airlines)
# In this case if we don't normalise the data before we run clustering 
# it will be dominated by valriables that are on a larger scale.

## Normalising the data
#If we don't normalize the data, the variables that are on a larger scale will contribute 
#much more to the distance calculation, and thus will dominate the clustering.

# Load package "caret"
library(caret)

# Create nomalised dataframe called "airlinesNorm"
# The first command pre-processes the data, and the second command performs the normalization. 

preproc <- preProcess(airlines)

airlinesNorm <- predict(preproc, airlines)

summary(airlinesNorm)
# You can see from the output that FlightMiles now has the largest maximum value,
# and DaysSinceEnroll now has the smallest minimum value. Note that these were not
# the variables with the largest and smallest values in the original dataset airlines.

###P2.1 Hierarchical Clustering

# Compute distances
distancesAN <- dist(airlinesNorm, method = "euclidean")

# Hierarchical clustering
clusterAN <- hclust(distancesAN, method = "ward.D") 

# Plot the dendrogram
plot(clusterAN)

# Assign points to clusters
HierClusterGroups <- cutree(clusterAN, k = 5)

# View cluster size
table(HierClusterGroups)
# HierClusterGroups
#1    2    3    4    5 
#776  519  494  868 1342 


# Interpret Cluster Groups
# Compare the average values in each of the variables for the 5 clusters
#(the centroids of the clusters). Describe each cluster characteristics.
#Compute the average values of the unnormalized data so that it is easier to interpret. 


tapply(airlines$Balance, HierClusterGroups, mean)
tapply(airlines$QualMiles, HierClusterGroups, mean)
tapply(airlines$BonusMiles, HierClusterGroups, mean)
tapply(airlines$BonusTrans, HierClusterGroups, mean)
tapply(airlines$FlightMiles, HierClusterGroups, mean)
tapply(airlines$FlightTrans, HierClusterGroups, mean)
tapply(airlines$DaysSinceEnroll, HierClusterGroups, mean)




# Balance (mean score in each cluster)
#cluster1         2         3         4         5 
#   57866.90 110669.27 198191.57  52335.91  36255.91 
# QualMiles
#1            2            3            4            5 
#0.6443299 1065.9826590   30.3461538    4.8479263    2.5111773 
#BonusMiles
#1         2         3         4         5 
#10360.124 22881.763 55795.860 20788.766  2264.788 
# BonusTrans
#1         2         3         4         5 
#10.823454 18.229287 19.663968 17.087558  2.973174 
#FlightMiles
#1          2          3          4          5 
#83.18428 2613.41811  327.67611  111.57373  119.32191 
#FlightTrans
#1         2         3         4         5 
#0.3028351 7.4026975 1.0688259 0.3444700 0.4388972 
#DaysSinceEnroll
#1        2        3        4        5 
#6235.365 4402.414 5615.709 2840.823 3060.081 

##P3.1 k-means clustering on normalised data

set.seed(88)
kmeansClust<-kmeans(airlinesNorm, centers=5, iter.max = 1000)

kmeansClust$size

[1]  408  141  993 1182 1275

#P3.2 Compare cluster centroids

kmeansClust$centers
#     Balance   QualMiles  BonusMiles BonusTrans FlightMiles  FlightTrans  DaysSinceEnroll
#1  0.51115730  1.8769284  1.0331951   0.1169945   0.1444636       0.7198040
#2  1.00054098  0.68382234  0.6144780  1.7214887   3.8559798   4.1196141       0.2742394
#3 -0.05580605 -0.14104391  0.3041358  0.7108744  -0.1218278  -0.1287569      -0.3398209
#4 -0.13331742 -0.11491607 -0.3492669 -0.3373455  -0.1833989  -0.1961819       0.9640923
#5 -0.40579897 -0.02281076 -0.5816482 -0.7619054  -0.1989602  -0.2196582      -0.8897747

# Compare distribution of data points between hierarchical and k-means clustering 
table(HierClusterGroups, kmeansClust$cluster)

# rows=hierachical; columns=k-means 
#HierClusterGroups         1    2    3    4    5
#                     1    4    0   98  673    1
#                     2   92  137  105   92   93
#                     3  300    4  132   58    0
#                     4   12    0  653   30  173
#                     5    0    0    5  329 1008


