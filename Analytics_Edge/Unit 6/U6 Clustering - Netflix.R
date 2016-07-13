### Netflix

### Video 6 getting data from the internet

### Get data from http://files.grouplens.org/datasets/movielens/ml-100k/u.item
## Copy and paste into Pages and export it as a txt file( with Mac's TextEdit programme.(NOT rich text)
# name it movieLens.rtf (rtf:rich text file)

movies <- read.table("movieLens2.txt", header=FALSE, sep="|",quote="\"")  

str(movies)

# Add column names
colnames(movies) <- c("ID", "Title", "ReleaseDate", "VideoReleaseDate", "IMDB", "Unknown", "Action", "Adventure", "Animation", "Childrens", "Comedy", "Crime", "Documentary", "Drama", "Fantasy", "FilmNoir", "Horror", "Musical", "Mystery", "Romance", "SciFi", "Thriller", "War", "Western")

str(movies)

# Remove unnecessary variables
movies$ID <- NULL
movies$ReleaseDate <- NULL
movies$VideoReleaseDate <- NULL
movies$IMDB <- NULL

# Remove duplicates
movies <- unique(movies)

# Take a look at our data again:
str(movies)

#QQ How many movies are classified as romance and drama
tapply(movies$Drama, movies$Romance, sum, na.rm=TRUE)


### Video 7 Hierachical Clustering in R using "ward method" in R this now called "ward.D"

# Compute distances
distances <- dist(movies[2:20], method = "euclidean")

# Hierarchical clustering using method "Ward.D"
#The ward method cares about the distance between clusters using centroid distance, 
# and also the variance in each of the clusters.

clusterMovies <- hclust(distances, method = "ward.D") 

# Plot the dendrogram
plot(clusterMovies)

# If you want a lot of clusters it's hard to pick the right number from the dendrogram.
# You need to use your understanding of the problem to pick the number of clusters.

# Assign points to clusters
# Label each data point according to the cluster it belongs. We here have selected 10 clusters.
clusterGroups <- cutree(clusterMovies, k = 10)

# Save cluster groups into data frame (if required)
movies$clusterGroups<-clusterGroups

#Now let's figure out what the clusters are like.

# Let's use the tapply function to compute the percentage of movies in each genre and cluster
# Remember this is a binary variable with value 0 or 1. 
# So computing the mean is actually computing the percentage of movies in that cluster
# that belong in the genre.

tapply(movies$Action, clusterGroups, mean)
# so  for "Action" movies about 78% of movies have an action genre label
# and these are found in cluster 2.

tapply(movies$Romance, clusterGroups, mean)

# We can repeat this for each genre. If you do, you get the results in ClusterMeans.ods

# Find which cluster Men in Black is in.

subset(movies, Title=="Men in Black (1997)")
clusterGroups[257]

# Create a new data set with just the movies from cluster 2(action, adventure scifi cluster)
cluster2 <- subset(movies, clusterGroups==2)
# subset(movies, Title=="Men in Black (1997)")
#Title                     Unknown Action Adventure Animation Childrens Comedy Crime
#257 Men in Black (1997)       0      1         1         0         0      1     0
#        Documentary Drama Fantasy FilmNoir Horror Musical Mystery Romance SciFi Thriller
#257           0     0       0        0      0       0       0       0     1        0
#     War Western
#257   0       0

# Look at the first 10 titles in  cluster 2:
cluster2$Title[1:10]

#QQ
clusterGroups2 <- cutree(clusterMovies, k = 2)

cluster2b <- subset(movies, clusterGroups2==2)

cluster2b$Title[1:10]
subset(movies, Title=="Mr. Holland's Opus (1995)")
# Crosscheck: shows 100% movies in cluster 2 are of drama genre
tapply(movies$Drama, clusterGroups2, mean)

## Note Alternatively, you can use colMeans or lapply commands (see notes My R Cookbook)
