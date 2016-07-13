#### U4 Assign 3 -Census: Predict how much a person earns
str(census)

library(caTools)
set.seed(2000)
spl<-sample.split(census$over50k, SplitRatio = 0.6)
Train<-subset(census, spl==TRUE)
Test<-subset(census, spl == FALSE)

###Q1 logistic regression
censusLog<-glm(over50k ~ .,data=Train, family = "binomial")        
summary(censusLog)

Prediction <- predict(censusLog, type="response", newdata=Test)
table(Test$over50k,Prediction >= 0.5)
#        FALSE TRUE
# <=50K  9051  662
# >50K   1190 1888

#accuracy
(9051+1888)/nrow(Test)
# 0.8552

#Q1.3 Baseline
table(Test$over50k)

9713/nrow(Test)
# 0.7594

#Q1.4
# Test set AUC 
library(ROCR)
ROCRpred = prediction(Prediction, Test$over50k)
as.numeric(performance(ROCRpred, "auc")@y.values)
#0.906

### Q2 CART Model
# Load CART packages
library(rpart)
library(rpart.plot)

CARTtrain <- rpart(over50k ~ ., data=Train, method="class")
prp(CARTtrain)
# Use a threshold of 0.5. (You can either add the argument type="class", 
#or generate probabilities and use a threshold of 0.5 like in logistic regression.)
PredictTest <- predict(CARTtrain, newdata = Test, type = "class")

# confusion table
table(Test$over50k, PredictTest)
# accuracy
(9243+1596)/nrow(Test)
#  0.847

# This highlights a very regular phenomenon when comparing CART and logistic regression.
# CART often performs a little worse than logistic regression in out-of-sample accuracy. 
# However, as is the case here, the CART model is often much simpler to describe and understand.

# # Q2.5 Evaluate model accuracy with ROC
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

library(ROCR)
PredictROC<-predict(CARTtrain, newdata = Test)
# ROC table with predicted probabilities for "0" or "1"
PredictROC
# Generate ROC Curve, where the first argument is the second column of PredictROC [,2] 
# the second argument is the true outcome values
pred<-prediction(PredictROC[,2],Test$over50k)
perf<-performance(pred,"tpr","fpr")
plot(perf)

#  Test Model AUC
as.numeric(performance(pred, "auc")@y.values)
# 0.0.8470

###Q3 Random Forest Model
library(randomForest)
set.seed(1)
trainSmall <- Train[sample(nrow(Train), 2000), ]
# Set a threshold of 0.5 (i.e. default). Therefore don't include set ntree=, nodesize= 
set.seed(1)
censusForest<- randomForest(over50k ~ . , data = trainSmall)

PredictForest<-predict(censusForest, newdata=Test)
table(Test$over50k, PredictForest)
# Confusion table 
PredictForest
#       <=50K  >50K
# <=50K   9586   127
# >50K    1985  1093
#accuracy
(9586 +1093)/nrow(Test)
#0.8349

#Q3.2
# This code produces a chart that for each variable measures the number of times
# that variable was selected for splitting (the value on the x-axis). 

vu = varUsed(censusForest, count=TRUE)

vusorted = sort(vu, decreasing = FALSE, index.return = TRUE)

dotchart(vusorted$x, names(censusForest$forest$xlevels[vusorted$ix]))

# see plot saved as pdf

#Q3.3
# Looking at model impurity.
# which measures how homogenous each bucket or leaf of the tree is. In each tree
#in the forest, whenever we select a variable and perform a split, the impurity 
#is decreased. Therefore, one way to measure the importance of a variable is to
#average the reduction in impurity, taken over all the times that variable is 
#selected for splitting in all of the trees in the forest. 

varImpPlot(censusForest)

# see impurity plot saved as pdf.


# Q4 Cross Validation
#Q4.1
## Let's see if we can improve on our results using the complexity parameter "cp parameter"
library(caret)
library(e1071)
# Let us select the cp parameter for our CART model using k-fold cross validation,
#with k = 10 folds. Do this by using the train function. Set the seed beforehand to 2.
#Test cp values from 0.002 to 0.1 in 0.002 increments, by using the following command:

# Define cross-validation experiment,make a k fold cross validation.k=10 folds
set.seed(2)
numFolds = trainControl( method = "cv", number = 10 )
cartGrid = expand.grid( .cp = seq(0.002,0.1,0.002))

# Perform the cross validation
train(over50k ~  ., data = Train, method = "rpart", trControl = numFolds, tuneGrid = cartGrid )

#Output CART 

#19187 samples
#12 predictor
#2 classes: ' <=50K', ' >50K' 

#No pre-processing
#Resampling: Cross-Validated (10 fold) 
#Summary of sample sizes: 17268, 17269, 17268, 17268, 17269, 17269, ... 
#Resampling results across tuning parameters:
        
#        cp     Accuracy   Kappa     
#0.002  0.8515658  0.55758898
#0.004  0.8476566  0.55353089
#0.006  0.8444252  0.53388285
# ...      ...       ...
#0.098  0.7593684  0.00000000
#0.100  0.7593684  0.00000000

#Accuracy was used to select the optimal model using  the largest value.
# The final value used for the model was cp = 0.002. 

# Q4.2
# Create a new CART model
censusTreeCV <- rpart(over50k ~  ., data = Train, method="class", cp = 0.002)

# Make predictions
PredictCV <- predict(censusTreeCV , newdata = Test, type = "class")
table(Test$over50k, PredictCV)
(9178+1838)/nrow(Test)
# 0.8612

#Q4.3 CART Tree for cross validation model
prp(censusTreeCV)
# see CART tree plot for crossvalidation model saved as pdf.

# This highlights one important tradeoff in building predictive models.
# By tuning cp, we improved our accuracy by over 1%, but our tree became significantly
# more complicated. In some applications, such an improvement in accuracy would 
# be worth the loss in interpretability. In others, we may prefer a less accurate 
#model that is simpler to understand and describe over a more accurate -- 
# but more complicated -- model.
