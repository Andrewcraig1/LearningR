### U6 Recitation on Clustering

# Video 2

str(flower) 
# This data represents an intensity matrix of an image, and needs to be seen as a matrix.
# NOT as observations vs variables (i.e. a dataframe)

# Change the data type to matrix
flowerMatrix <- as.matrix(flower)
str(flowerMatrix)

# Turn matrix into a vector 
flowerVector <- as.vector(flowerMatrix)
str(flowerVector)

flowerVector2 = as.vector(flower)
str(flowerVector2)

# Compute Euclidean distances within the vector
distance <- dist(flowerVector, method = "euclidean")

# Hierarchical clustering
clusterIntensity <- hclust(distance, method="ward.D")

# Plot the dendrogram
plot(clusterIntensity)

# Select 3 clusters
rect.hclust(clusterIntensity, k = 3, border = "red")
# Cut dendrogram into "k" clusters
flowerClusters <- cutree(clusterIntensity, k = 3)
# View which data points are in which cluster
flowerClusters

# Find mean intensity values
tapply(flowerVector, flowerClusters, mean)
# In this example intensity values of pixels range from 0 = black and 1 = white
#           cluster        1          2          3 
# mean of intensity   0.08574315 0.50826255 0.93147713 


# Plot the image and the clusters
# dim is the dimension function which takes as its input flowerClusters
# and convert it back to the 50x50 matrix (rows x colums)
dim(flowerClusters) <- c(50,50)
#Turn off axes and view the segmented image
image(flowerClusters, axes = FALSE)

# View segmented image (refer to U6 reciation pdf on flower)
# Cluster 1 is the darkest shade (the background)
# Cluster 2 is the core of the flower
# Cluster 3 is the lightest sshade - the petals 

# View the Original image
image(flowerMatrix,axes=FALSE,col=grey(seq(0,1,length=256)))

# So we're going to take the color,and it's going to take the function gray.
# And the input to this function is a sequence of values that goes from 0 to 1, which
# actually is from black to white. And then we have to also specify its length,
# and that's specified as 256, because this corresponds to the convention for grayscale.

# Video 4

# Let's try this with an MRI image of the brain

healthyMatrix <- as.matrix(healthy)
str(healthyMatrix)

# Plot image
image(healthyMatrix,axes=FALSE,col=grey(seq(0,1,length=256)))

## Let's try Hierarchial clustering first (but we will find data generated requires more than 2.0GB)
healthyVector <-as.vector(healthyMatrix)

#Command distance <- dist(healthyVector, method = "euclidean")
# generates error. Error: cannot allocate vector of size 2.0 Gb
# We have an error - why?
str(healthyVector)
#str shows vector has 365636 data points
n<-365636
# No of values to be generated and stored as pairwise distances
n*(n-1)/2
#  66844659430
# Convert this into bytes (c/o Roger Peng's Book on R, p90)
66844659430/(2^20)/1000
# 63748.03MB or 63.7GB
# This is massive!!!
# To high an image resolution for hierarchial clustering, so lets try k-means

# Video 5

# Specify number of clusters
# Using existing knowledge about tissues
k <- 5

# Run k-means
set.seed(1)
KMC <- kmeans(healthyVector, centers = k, iter.max = 1000)
str(KMC)

# List of 9
#$ cluster     : int [1:365636] 3 3 3 3 3 3 3 3 3 3 ...
##$ centers (equivalent to mean of each cluster :5)
#              : num [1:5, 1] 0.4818 0.1062 0.0196 0.3094 0.1842
#..- attr(*, "dimnames")=List of 2
#.. ..$ : chr [1:5] "1" "2" "3" "4" ...
#.. ..$ : NULL
#$ totss       : num 5775
#$ withinss    : num [1:5] 96.6 47.2 39.2 57.5 62.3
#$ tot.withinss: num 303
#$ betweenss   : num 5472
##$ size (of each cluster 1:5): int [1:5] 20556 101085 133162 31555 79278
#$ iter        : int 2
#$ ifault      : int 0
#- attr(*, "class")= chr "kmeans"

# Extract cluster mean from kmeans vector
KMC$centers[2]

# Extract clusters as a matrix
healthyClusters <- KMC$cluster


# The cluster mean values are closer to 0 (black) than 1 (white)

# Plot the image with the clusters using dimension function and adding colours
dim(healthyClusters) <- c(nrow(healthyMatrix), ncol(healthyMatrix))
image(healthyClusters, axes = FALSE, col=rainbow(k))

# Video 6
#### kcca: K-Centroids Cluster Analysis

### Now, we will not run the k-means algorithm again on the tumor vector.
#Instead, we will apply the k-means clustering results that we found using the healthy brain
# image on the tumor vector. In other words, we treat the healthy vector as 
# training set and the tumor vector as a testing set.


# Convert tumor dataframe into matrix and then into vector
tumorMatrix <- as.matrix(tumor)
tumorVector <- as.vector(tumorMatrix)

# Apply kmeans clusters from KMC to new image, using the flexclust package
## 
install.packages("flexclust")
library(flexclust)

# We need to convert the original information from the clustering algorithm to an object of the class KCCA.
# This is our training data set.
KMC.kcca <- as.kcca(KMC, healthyVector)

#KMC.kcca is the healthy MRI image data, and tumorVector is the test set image data
tumorClusters <- predict(KMC.kcca, newdata = tumorVector)

# Visualize the clusters
# convert to tumor matrix
dim(tumorClusters) <- c(nrow(tumorMatrix), ncol(tumorMatrix))

image(tumorClusters, axes = FALSE, col=rainbow(k))






