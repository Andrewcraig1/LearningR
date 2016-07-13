str(wine)

summary(wine)
##model 1
model1<-lm(Price~AGST, data = wine)
summary(model1)

#Residuals
model1$residuals

#SSE
SSE<-sum(model1$residuals^2)
SSE

## model 2
model2<-lm(Price~AGST+HarvestRain,data = wine)
summary(model2)
SSE<-sum(model2$residuals^2)
SSE

## model 3
model3<-lm(Price~AGST+HarvestRain+WinterRain+Age+FrancePop,data=wine)
summary(model3)
SSE<-sum(model3$residuals^2)
SSE

## Quick Question 2 & 3 
modelQ<-lm(Price~HarvestRain+WinterRain, data=wine)
summary(modelQ)

### Video 5: Understanding the model and using coefficients to improve the model

## model 4 - where removing unhelpful variables by an analysis of the coefficients
## std error, t value (Estimate/Std error) and Pr.Start by removing FrancePop.
model4<-lm(Price~AGST+HarvestRain+WinterRain+Age,data=wine)
summary(model4)
# not im summary that model 4 is just as strong as model 3 in terms of R-squared
# but is slightly stonger as the adjusted R-squared has increased.

### Video 6: Corrleation and Miulticollinearity to improve model performance
# correlation between two variables
cor(wine$WinterRain,wine$Price)

cor(wine$Age,wine$FrancePop)

#correlations between all varaiables in the dataset
# this helps us ind MULITI-COLINEARITY PROBLEMS in our model.
cor(wine)


model5<-lm(Price~AGST+HarvestRain+WinterRain,data=wine)
summary(model5)

# Model 4 performs best. So this is selected for the use on Test data (Video 7)


