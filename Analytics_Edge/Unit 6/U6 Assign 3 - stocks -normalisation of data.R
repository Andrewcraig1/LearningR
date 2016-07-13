### U6 Assign 3 stocks and normalisation 
## first use clustering to identify clusters of stocks that have similar returns over time.
##Then, we'll use logistic regression to predict whether or not the stocks will have positive 
## future returns.

## P1 Exploring the data set
str(stocks)

table(stocks)

# check for multi-colliniarity
cor(stocks)

summary(stocks)

## P2 Initial Logistic regression

# Train and Test data sets
# spliting dataset into training and test
library(caTools)

set.seed(144)

spl <- sample.split(stocks$PositiveDec, SplitRatio = 0.7)

stocksTrain <- subset(stocks, spl == TRUE)

stocksTest <- subset(stocks, spl == FALSE)

# Logistic model - training set
StocksModel<-glm(PositiveDec~ ., data=stocksTrain, family="binomial")
summary(StocksModel)

predStocks1<-predict(StocksModel,type="response")
table(stocksTrain$PositiveDec, predStocks1 >=0.5)

#   FALSE TRUE
#0   990 2689
#1   787 3640

#accuracy on training set=0.571
(990+3640)/(nrow(stocksTrain))

## Logistic model on Test set
predStocksTest1<-predict(StocksModel,newdata = stocksTest, type="response")
table(stocksTest$PositiveDec, predStocksTest1 >=0.5)

#   FALSE TRUE
#0   417 1160
#1   344 1553

#accuarcy on training set=0.567
(417+1553)/(nrow(stocksTest))

##P2.3 Test set baseline accuracy=0.546
table(stocksTest$PositiveDec)
# 0    1 
# 1577 1897 
1897/(1897+1577)


### P3 Clustering Stocks
# Remove dependent variable:
# In cluster-then-predict, our final goal is to predict the dependent variable, which is unknown to us at
#the time of prediction. Therefore, if we need to know the outcome value to perform the clustering, 
#the methodology is no longer useful for prediction of an unknown outcome value.
#This is an important point that is sometimes mistakenly overlooked. If you use the outcome
#value to cluster, you might conclude your method strongly outperforms a non-clustering alternative.
#However, this is because it is using the outcome to determine the clusters, which is not valid.

limitedTrain <- stocksTrain

limitedTrain$PositiveDec <- NULL

limitedTest <- stocksTest

limitedTest$PositiveDec <- NULL

## Prob 3.2 Normalising
#Use the preProcess command "preproc" from the caret package, which normalizes variables by 
# subtracting by the mean and dividing by the standard deviation.(i.e.how many std deviations is the variable value distant from the mean.)
#In cases where we have a training and testing set, we'll want to normalize by 
#the mean and standard deviation of the variables in the training set.

library(caret)

preproc <- preProcess(limitedTrain)

normTrain <- predict(preproc, limitedTrain)

normTest <- predict(preproc, limitedTest)

# Explore data
summary(normTrain)
summary(normTest)

#P 3.4 k-means Clustering stocks
set.seed(144)
km<-kmeans(normTrain, centers=3)

str(km)
# or km$size
km$size

#3.5 Clustering with flexclust package
library(flexclust)

km.kcca <- as.kcca(km, normTrain)

clusterTrain <- predict(km.kcca)

clusterTest <- predict(km.kcca, newdata=normTest)
(clusterTest)

# View predicted clustering results with"table" function
table(clusterTest)
#clusterTest
#  1    2    3 
#1298 2080   96 

###P4 Cluster-specific predictions
## Using the subset function, AND results from clusterTrain and clusterTest 
# build data frames stocksTrain1, stocksTrain2,and stocksTrain3, 
# containing the elements in the stocksTrain data frame assigned to clusters 1, 2, and 3, respectiv

stocksTrain1 <- subset(stocksTrain, clusterTrain == 1)

stocksTrain2 <- subset(stocksTrain, clusterTrain == 2)

stocksTrain3 <- subset(stocksTrain, clusterTrain == 3)

stocksTest1 <- subset(stocksTest, clusterTest == 1)

stocksTest2 <- subset(stocksTest, clusterTest == 2)

stocksTest3 <- subset(stocksTest, clusterTest == 3)

mean(stocksTrain3$PositiveDec)

# Cluster Specific Predictions

# Logistic model - training set
StocksModel1<-glm(PositiveDec~ ., data=stocksTrain1, family="binomial")
summary(StocksModel1)

StocksModel2<-glm(PositiveDec~ ., data=stocksTrain2, family="binomial")
summary(StocksModel2)

StocksModel3<-glm(PositiveDec~ ., data=stocksTrain3, family="binomial")
summary(StocksModel3)

#P4.3 prediction & accuracy
#Cluster1
predictTest1<-predict(StocksModel1,newdata=stocksTest1,type="response")
table(stocksTest1$PositiveDec, predictTest1 >=0.5)
#  FALSE TRUE
#0    30  471
#1    23  774

#accuracy on training set=0.619
(30+774)/(nrow(stocksTest1))

#Cluster2
predictTest2<-predict(StocksModel2,newdata=stocksTest2,type="response")
table(stocksTest2$PositiveDec, predictTest2 >=0.5)
#  FALSE TRUE
#0   388  626
#1   309  757

#accuracy on training set=0.550
(388+757)/(nrow(stocksTest2))

#Cluster3
predictTest3<-predict(StocksModel3,newdata=stocksTest3,type="response")
table(stocksTest3$PositiveDec, predictTest3 >=0.5)
#  FALSE TRUE
#0    49   13
#1    21   13

#accuracy on training set=0.646
(49+13)/(nrow(stocksTest3))


## P4.4overall test set accuracy

AllPredictions <- c(predictTest1, predictTest2, predictTest3)

AllOutcomes <- c(stocksTest1$PositiveDec, stocksTest2$PositiveDec, stocksTest3$PositiveDec)

table(AllOutcomes,AllPredictions >=0.5)

# AllOutcomes FALSE TRUE
#          0   467 1110
#          1   353 1544

#combined accuracy=0.579
(467+1544)/(467+1110+353+1544)

# We see a modest improvement over the original logistic regression model. Since 
# predicting stock returns is a notoriously hard problem, this is a good increase 
#in accuracy. By investing in stocks for which we are more confident that they will 
#have positive returns (by selecting the ones with higher predicted probabilities), 
# this cluster-then-predict model can give us an edge over the original logistic regression model.


