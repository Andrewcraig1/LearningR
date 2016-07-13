### U4 Assign 2 - Letters
str(letters)
# this is a multiclass classification problem. We have mostly focused on binary 
# classification problems (e.g., predicting whether an individual voted or not,
# whether the Supreme Court will affirm or reverse a case, whether or not a person
#is at risk for a certain disease, etc.). In this problem, we have more than two 
# classifications that are possible for each observation, like in the D2Hawkeye lecture. 

str(letters)

# Prob 1.1 Predicting B or not B
letters$isB<-as.factor(letters$letter=="B")
names(letters)

library(caTools)
set.seed(1000)
spl<-sample.split(letters$isB, SplitRatio = 0.5)
Train<-subset(letters, spl==TRUE)
Test<-subset(letters, spl == FALSE)

# Simple baseline on test data for "notB" (ie isB=0)
table(Test$isB)
#1175/1558
# 0.754172

# Prob 1.2 CART Model
# Load CART packages
library(rpart)
library(rpart.plot)

CARTb <- rpart(isB ~ . - letter, data=Train, method="class")
prp(CARTb)

PredictTest <- predict(CARTb, newdata = Test, type = "class")


# confusion table
table(Test$isB, PredictTest)
# accuracy
(1118+340)/nrow(Test)
# 0.9358

# Prob 1.3 Random Forest Model
library(randomForest)
#  set ntree=, nodesize=2 to default(don't include them)
set.seed(1000)
lettersForest<- randomForest(isB ~ . - letter , data = Train)

PredictForest<-predict(lettersForest, newdata=Test)
table(Test$isB, PredictForest)
# Confusion table 
PredictForest
#       FALSE TRUE
# FALSE  1165   10
# TRUE      9  374

# accuracy 
(1165+374)/nrow(Test)
# 0.9878
# In lecture, we noted that random forests tends to improve on CART in terms 
# of predictive accuracy. Sometimes, this improvement can be quite significant,
# as it is here.

### Problem 2 Predicting the letters A,B,P,R
## Prob 2.1 baseline model

#convert letters to factor
letters$letter<-as.factor(letters$letter)

# generate new train and test data
set.seed(2000)
spl<-sample.split(letters$letter, SplitRatio = 0.5)
Train<-subset(letters, spl==TRUE)
Test<-subset(letters, spl == FALSE)

#simple baseline model
table(Test$letter)
#  A   B   P   R 
# 395 383 401 379 

#In a multiclass classification problem, a simple baseline model is to predict
#the most frequent class of all of the options.
401/(395+383+401+379)
#  0.2574

#2.2 Classification Tree 
# add the argument method="class" as this is a classification problem
CARTmodel <- rpart(letter ~ . -isB, data= Train)
prp(CARTmodel)
# accuracy on predtest
PredictTest2<-predict(CARTmodel, newdata=Test, type="class")
table(Test$letter, PredictTest2)
(348+318+363+340)/nrow(Test)
# 0.878

#2.3 Random Forest Model
set.seed(1000)
lettersForest2<- randomForest(letter ~ . - isB , data = Train)

PredictForest2<-predict(lettersForest2, newdata=Test)
table(Test$letter, PredictForest2)
# Confusion table 
#PredictForest2
# A   B   P   R
# A 390   0   3   2
# B   0 379   2   2
# P   0   6 394   1
# R   0  13   1 365

# accuracy 
(390+379+394+365)/nrow(Test)
#0.981

# You should find this value rather striking, for several reasons. 
# The first is that it is significantly higher than the value for CART, 
# highlighting the gain in accuracy that is possible from using random forest models. 
#The second is that while the accuracy of CART decreased significantly as we 
# transitioned from the problem of predicting B/not B (a relatively simple problem) 
# to the problem of predicting the four letters (certainly a harder problem), 
# the accuracy of the random forest model decreased by a tiny amount.
