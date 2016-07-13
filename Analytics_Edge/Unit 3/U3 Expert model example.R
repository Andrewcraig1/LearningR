### LOGISTIC REGRESSION
## understanding the data
str(quality)

#If P(PoorCare=1) >= t, threshold value, predict poor quality
#If P(PoorCare=1) < t, threshold value, predict good quality

# Baseline model. We would predict (from variable of PoorCare) that all patients
# are getting good care. Our accuracy would be 75%(derived from from table)
table(quality$PoorCare)

98/131

## Create training and test set of date. Randomly split quality dataframe.
# install.packages(caTools)

library(caTools)
# "set.seed(88)" function applied so randomisation occurs consistently same way each time.
set.seed(88)
#split ratio is here set at 75% of data into training set and 25% into test set.
# Sample.split also makes sure that 75% of our patients are receiving good care
# in both the training and test data.
split<- sample.split(quality$PoorCare, SplitRatio = 0.75)

split
# TRUE means observation to go into training set. FALSE obs to go into test set.
qualityTrain<-subset(quality,split==TRUE)
qualityTest<-subset(quality,split==FALSE)
nrow(qualityTrain)
nrow(qualityTest)

names(qualityTrain)

# Build Logistic Regression Model usung "generalised linear model" function "glm".
# "family = binomial" argument tells the the "glm" function to build a logistic regression.
QualityLog<- glm(PoorCare~OfficeVisits + Narcotics, data = qualityTrain, family = binomial)
summary(QualityLog)

# Model Performance: Look at coefficients as per linear regression. 
# AIC is a measure of model quality, Like adjusted R2.
# It accounts for the number of variables used compared to the number of observations.
# but can only be compared between models on the same data set.
# The preferred model is the one with the minimum AIC.

# Create predict model.
# Argument "type="response" tells the model to give us probabilities.

predictTrain<-predict(QualityLog, type="response")
summary(predictTrain)

# Let's see if we are predicting higher probabilities for actual poor cases as we expect.
tapply(predictTrain, qualityTrain$PoorCare, mean)

# TRUE poor care cases we predict an average probability of about 0.44
# TRUE good care cases we predict an average probability of about 0.19

## Quick Question
QualityLog2<- glm(PoorCare~ StartedOnCombination + ProviderCount, data = qualityTrain, family = binomial)
summary(QualityLog2)

predictTrain2<-predict(QualityLog2, type="response")
summary(predictTrain2)

# Let's see if we are predicting higher probabilities for actual poor cases as we expect.
tapply(predictTrain2, qualityTrain$PoorCare, mean)

# In QuyalityLog2: 
#TRUE poor care cases we predict an average probability of about 0.32
# TRUE good care cases we predict an average probability of about 0.23
# This model implies that starting a patient on a combination of drugs is indicative of poor care.

### THRESHOLDING
## Confusion matrics to determine sensivity and specificity of Threshold values
## Choose a threshold of 0.5


# Confusion matrix created with table 
table(qualityTrain$PoorCare, predictTrain >0.5)
# table row headingsL Actual=0; Actual=1
# Table column headings: Predicted=); Predicted=1

#Sensitivity=TP/(TP+FN)
10/(15+10)
#Sensitivity=0.4
#Specificity= TN/(TN+FP)
70/(70+4)
#Specificity=0.95

## Choose a threshold of 0.7

# Confusion matrix created with table 
table(qualityTrain$PoorCare, predictTrain >0.7)
# table row headingsL Actual=0; Actual=1
# Table column headings: Predicted=); Predicted=1
# Table Structure:
# TN  FP
# FN  TP

#Sensitivity=TP/(FN+TP)
8/(8+17)
#Sensitivity=0.32
#Specificity= TN/(TN+FP)
73/(73+1)
#Specificity=0.99
#So by increasing the threshold our sensitivity went down and specificity went up.

## Choose a threshold of 0.2

# Confusion matrix created with table 
table(qualityTrain$PoorCare, predictTrain >0.2)
# table row headingsL Actual=0; Actual=1
# Table column headings: Predicted=); Predicted=1

#Sensitivity=TP/(FN+TP)
16/(9+16)
#Sensitivity=0.64
#Specificity= TN/(TN+FP)
54/(54+20)
#Specificity=0.73
#So by lowering the threshold our sensitivity went up and specificity went down.

##QQ (Quick Question)
#Sensitivity=TP/(FN+TP)
20/(5+20)
#Sensitivity=0.8
#Specificity= TN/(TN+FP)
15/(15+10)
#Specificity=0.6

## QQ
#Sensitivity=TP/(FN+TP)
15/(10+15)
#Sensitivity=0.6
#Specificity= TN/(TN+FP)
20/(20+5)
#Specificity=0.8
#So by increasing the threshold our sensitivity went down and specificity went up.


### ROC (Receiver Operator Characeristic) Curves

## Generate ROC Curves
library(ROCR)
ROCRpred<-prediction(predictTrain, qualityTrain$PoorCare)
# tpr abbrev for True positive rate; fpr for False positive rate
ROCRperf<-performance(ROCRpred,"tpr", "fpr")
plot(ROCRperf, colorize=TRUE, print.cutoffs.at=seq(0,1,0.1),text.adj=c(-0.2,1.7))
# "print.cutoffs.at=seq(0,1,0.1)": this prints threshold values at intervals between
# 0 and 1 at intervals of 0.1.To change the interval increase or lower 0.1.

### Checking Model Performance
## Checking for the occurence of multicollinearity

## V7 QQ
#Predict Test data
predictTest<-predict(QualityLog, type = "response", newdata = qualityTest)
#calculate AUC (Area Under the ROC Curve): An absolute Measure of quality of prediction.
ROCRpredTest<-prediction(predictTest,qualityTest$PoorCare)
auc<-as.numeric(performance(ROCRpredTest,"auc")@y.values)
auc
#auc= 0.799

## The AUC of a model has the following nice interpretation: 
# given a random patient from the dataset who actually received poor care, 
# and a random patient from the dataset who actually received good care, 
# the AUC is the perecentage of time that our model will classify which is which correctly.

