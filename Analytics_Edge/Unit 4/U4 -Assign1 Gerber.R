###Gerber
# This assignment compares CARTRegression with Logistic Regression
# with specific focus on indivual variables

# Understanding the data
str(gerber)
table(gerber$voting)
summary(gerber)
table(gerber$voting,gerber$civicduty)
table(gerber$voting,gerber$hawthorne)
table(gerber$voting,gerber$self)
table(gerber$voting,gerber$neighbors)

#logistic regression
gerberLog<-glm(voting ~ civicduty + hawthorne + self + neighbors,data=gerber, 
               family = "binomial")        
summary(gerberLog)

Prediction <- predict(gerberLog,type="response")
table(gerber$voting,Prediction >= 0.3)
summary(Prediction)

# accuracy at threshold 0.3 and 0.5

# AUC accuracy
library(ROCR)
ROCRpred<-prediction(Prediction,gerber$voting)
as.numeric(performance(ROCRpred, "auc")@y.values)
# 0.531
# Even though all the variables are significant. This is a weak predictiove model.

## CART Trees modeling
# Load CART packages
library(rpart)
library(rpart.plot)


CARTmodel <- rpart(voting ~ civicduty + hawthorne + self + neighbors, data=gerber)
prp(CARTmodel)

# Here No variables are used (the tree is only a root node)
# - none of the variables make a big enough effect to be split on.
# force the tree to be built with the complexity parameter
CARTmodel = rpart(voting ~ civicduty + hawthorne + self + neighbors, data=gerber, cp=0.0)
prp(CARTmodel)

CARTmodel2 <- rpart(voting ~ civicduty + hawthorne + self + neighbors + sex, data=gerber, cp=0.0)
prp(CARTmodel2)

## Interaction terms
CARTmodel4 <- rpart(voting ~ control, data=gerber, cp=0.0)
# note "digits=6" argument added to get 6 decimal places.
prp(CARTmodel4,digits=6)

# Control prediction diff and non-control prediction
abs( 0.296638- 0.34)

CARTmodel5 = rpart(voting ~ control + sex, data=gerber, cp=0.0)
prp(CARTmodel5, digits=6)

#Now, using the second tree (with control and sex), determine who is affected more
#by NOT being in the control group (being in any of the four treatment groups):
#The first split says that if control = 1, go left. 
#Then, if sex = 1 (female) predict 0.290456, and if sex = 0 (male) predict 0.302795. 
#On the other side of the tree, where control = 0, if sex = 1 (female) predict 0.334176, 
#and if sex = 0 (male) predict 0.345818. So for women, not being in the control 
#group increases the fraction voting by 0.04372. For men, not being in the control
#group increases the fraction voting by 0.04302. So men and women are affected about the same.

### Learning Point! With trees . STOP think about what the tree means and how the splits are working. This is essential.
# DON'T rush this.

## Q 3.3 Interaction Terms
#logistic regression
gerberLog2<-glm(voting ~ control + sex ,data=gerber, family = "binomial")        
summary(gerberLog2)

#Q3.4 Interaction Terms
# Four possibilities (Man, Not Control), (Man, Control), (Woman, Not Control), (Woman, Control).
Possibilities = data.frame(sex=c(0,0,1,1),control=c(0,1,0,1))
predict(gerberLog2, newdata=Possibilities, type="response")
#   This is clever
# 1         2         3         4 
# 0.3462559 0.3024455 0.3337375 0.2908065 

abs(0.290456-0.2908065)

#Q3.4
# add a combination of 2 variables. A new term to our logistic regression now, 
# that is the combination of the"sex" and "control" variables - 
# so if this new variable is 1, that means the person
#is a woman AND in the control group. We can do that with the following 
# additional argument "+sex:control":

LogModel2 = glm(voting ~ sex + control + sex:control, data=gerber, family="binomial")
summary(LogModel2)
predict(LogModel2, newdata=Possibilities, type="response")

abs(0.290456-0.2904558)
