# U3 framingham

str(framingham)

#split data
library(caTools)
set.seed(1000)
split=sample.split(framingham$TenYearCHD, SplitRatio = 0.65)
# when you have more data you can afford to put more in the testing set.
# Typically want to put 50-80% of data into the training set.

train<-subset(framingham,split==TRUE)
test<-subset(framingham,split==FALSE)

# Build model

framinghamlog<-glm(TenYearCHD ~ ., data = train, family=binomial)
summary(framinghamlog)

# argument "response" gives us probabilities
predictTest<-predict(framinghamlog, type = "response", newdata = test)
table(test$TenYearCHD, predictTest >0.5)
#    FALSE TRUE
# 0  1069    6
# 1   187   11

# With a threshold of 0.5, we predict an outcome of 1 (the true column) very rarely.

#overall accuracy=(TN+TP)/N
(1069+11)/(1069+6+187+11)


# Baseline model would always predict 0 (FALSE)
# overallbaseline accuracy= TN+FP/N
(1069+6)/(1069+6+187+11)
#So our model barely beats the baseline.

# AUC accuracy
library(ROCR)
ROCRpred<-prediction(predictTest,test$TenYearCHD)
as.numeric(performance(ROCRpred, "auc")@y.values)
# This model rarely predicts 10 year CHD risk above a threshold of 50%. 
# The acuuracy very near a baseline of always predicting no CHD.
# But the model can differentiate low-risk from high risk patients (AUC=0.74)

#QQ sensitivity=TP/(FN+TP)
11/(187+11)
#QQ specificity=TN/(TN+FP)
1069/(1069+6)


### v4 Validating the Model
#internal vs external validation (other populations outside of farmingham a predom white popn)
# Recalibrate modeladjusting it to new populations.

### v5 Interventions