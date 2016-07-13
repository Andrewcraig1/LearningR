#### Parole
str(parole)
names(parole)
table(parole$violator)
summary(parole)

# "as.factor": Convert state and crime categories data into factors recognised by R
parole$state<-as.factor(parole$state)
parole$crime<-as.factor(parole$crime)
summary(parole)
str(parole)
table(parole$state)

# Create training and test data frames
set.seed(144)
library(caTools)
split = sample.split(parole$violator, SplitRatio = 0.7)

train = subset(parole, split == TRUE)
test = subset(parole, split == FALSE)

### Training Model
names(parole)
ViolatLog1<-glm(violator~ .,data=train, family = "binomial")        
summary(ViolatLog1)

#"From the logistic regression equation, we have log(odds) = -4.2411574 + 0.3869904*male + 0.8867192*race - 0.0001756*age + 
# 0.4433007*state2 + 0.8349797*state3 - 3.3967878*state4 - 0.1238867*time.served + 0.0802954*max.sentence + 1.6119919*multiple.offenses + 0.6837143*crime2 - 0.2781054*crime3 - 0.0117627*crime4. 
#This parolee has male=1, race=1, age=50, state2=0, state3=0, state4=0, time.served=3, max.sentence=12, multiple.offenses=0, crime2=1, crime3=0, crime4=0. 
# We conclude that log(odds) = -1.700629.
# Therefore, the odds ratio is exp(-1.700629) = 0.183, 
# and the predicted probability of violation is 1/(1+exp(1.700629)) = 0.154. "


##Log odds, odds, P
logOdds<-((-4.2411574)+(0.3869904*1)+( 0.8867192*1)+(-0.0001756*50)+(0.443*0)+(0.835*0)+(-3.397*0)+(-0.1238867*3)+(0.0802954*12)+(0.6837143*1))
logOdds
odds<-exp(logOdds)
odds

P<-1/(1+exp(-logOdds))
# The logOdds=  -1.700629
# odds 0.1825687
# P of being a pariole violator = 0.1544


## Evalutaing Model with testing set
TestPrediction = predict(ViolatLog1, newdata=test, type="response")
summary(TestPrediction)

table(test$violator, TestPrediction >= 0.5)

# sensitivity
12/(11+12)


# specificity
167/(167+12)

#overall accuracy
(167+12)/nrow(test)

# Simple model
table(test$violator)
179/202

# threshold considerations
# The board assigns more cost to a false negative than a false positive, 
# and should therefore use a logistic regression cutoff less than 0.5. 
# The board assigns more cost to a false negative than a false positive, 
# and should therefore use a logistic regression cutoff less than 0.5. - correct

### AUC
# AUC accuracy
library(ROCR)
ROCRpred<-prediction(TestPrediction,test$violator)
as.numeric(performance(ROCRpred, "auc")@y.values)
#  AUC: The probability the model can correctly differentiate between a randomly selected parole violator 
# and a randomly selected parole non-violator.
