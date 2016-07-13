####Unit4 Supreme Court CART Model
str(stevens)


### Create train and test data sets
library(caTools)
seed(3000)
spl<-sample.split(stevens$Reverse, SplitRatio = 0.7)
Train<-subset(stevens, spl==TRUE)
Test<-subset(stevens, spl==FALSE)


### Create CART Model
## Install packages "rpart" AND "rpart.plot"
library(rpart)
library(rpart.plot)

## Model
names(stevens)
# increasing argument minbucket parameter="25" gave 7 splits in tree. 
# Reducing value increases splits. 
# Increasing the value reduces the splits.
StevensTree<-rpart(Reverse~Circuit+Issue+Petitioner+Respondent+LowerCourt+Unconst, data=Train, method = "class", minbucket=25)
prp(StevensTree)

## Predict
# a third argument here, which is type = "class".
# We need to give this argument when making predictions for our CART model if we want the majority class predictions.
# This is like using a threshold of 0.5. We'll see in a few minutes how we can leave this argument out
# and still get probabilities from our CART model using ROC.

PredictCART<-predict(StevensTree, newdata = Test, type = "class")

## Model accuracy
table(Test$Reverse,PredictCART)

#PredictCART: confusion table
#   0  1
# 0 41 36
# 1 22 71

# accuracy (as per Logistic Reg)
(41+71)/nrow(Test)
# 0.6588235

# Evaluate model accuracy with ROC
library(ROCR)
PredictROC<-predict(StevensTree, newdata = Test)
# ROC table with predicted probabilities for "0" or "1"
PredictROC
# Generate ROC Curve, where the first argument is the second column of PredictROC [,2] 
# the second argument is the true outcome values
pred<-prediction(PredictROC[,2],Test$Reverse)
perf<-performance(pred,"tpr","fpr")
plot(perf)

# Test Model AUC
as.numeric(performance(pred, "auc")@y.values)
# 0.6927105

### Video 5: Random Forests: Designed to improve prediction of CART, but is not as interpretable as CART.
library(randomForest)
# convert Reverse data to factor for Train and Test data sets
Train$Reverse<-as.factor(Train$Reverse)
Test$Reverse<-as.factor(Test$Reverse)

# nodesizeis the random seed value; nTree is the number of trees to build
StevensForest = randomForest(Reverse ~ Circuit + Issue + Petitioner + Respondent +
                                     LowerCourt + Unconst, data = Train, ntree=200, nodesize=25 )
# gives a warning message if outcome not a factor (in this case Reverse)
PredictForest<-predict(StevensForest, newdata=Test)
table(Test$Reverse, PredictForest)
# Confusion table (can vary due to random sampling method)
#   0  1
# 0 41 36
# 1 18 75

# accuracy (nodesize =25)
(41+75)/nrow(Test)
#0.682

# nodesize = 100
#accuracy = 0.694
(44+74)/nrow(Test)

# nodesize = 200
#accuracy=0.682
(49+67)/nrow(Test)


### cross-validation
library(caret)
library(e1071)

numFolds<-trainControl(method = "cv", number=10)
# define cp parameters to test as numbers from 0.01 to 0.5, in increments of 0.01.
cpGrid<-expand.grid(.cp=seq(0.01,0.5,0.01))

#now we are ready to perform cross validation
train(Reverse~Circuit+Issue+Petitioner+Respondent+LowerCourt+Unconst, data=Train,method="rpart",trControl=numFolds, tuneGrid=cpGrid)

StevensTreeCV<-rpart(Reverse~Circuit+Issue+Petitioner+Respondent+LowerCourt+Unconst, data=Train,method="class", cp = 0.18)
PredictCV<-predict(StevensTreeCV,newdata=Test, type="class")
table(Test$Reverse, PredictCV)

#accuracy=0.724
(59+64)/nrow(Test)

# Plot the tree created
prp(StevensTreeCV)
