#Unit 2 Assignment: Climate Change

str(climate)

#Prob1.1 Split data into training set and testing set.
climate_training<-subset(climate, climate$Year <=2006)
str(climate_training)
max(climate_training$Year)

climate_test<-subset(climate, climate$Year >2006)
str(climate_test)
min(climate_test$Year)

# Build lm to predict Temp, using MEI,CO2,CH4,N20,CFC.11,CFC.12,TSI & Aerosols

TempReg1<-lm(Temp~MEI+CO2+CH4+N2O+CFC.11+CFC.12+TSI+Aerosols, data=climate_training)
summary(TempReg1)

#Simplify model- check correlations

cor(climate_training)

TempReg2<-lm(Temp~MEI+N2O+TSI+Aerosols, data=climate_training)
summary(TempReg2)

#the "step" function provides procedure of trying different combinations of variables to find
# a good comprise of model simplicity and R2. It calculates Akaike information crirerion (AIC) - 
# the quality of model with a penalty for the number of variables in the model.

TempStep<-step(TempReg1)
TempStep
summary(TempStep)
# NOTE in this example the step function does not address the collinearity of the variables, 
# except that addind highly correlated variables will not improve Rsq significantly.

TempPrediction<-predict(TempStep,newdata=climate_test)
# Now calc out of sample R-squared to check predictions goodness of fit.
SSE<-sum((TempPrediction-climate_test$Temp)^2)
SST<-sum((mean(climate_training$Temp)-climate_test$Temp)^2)
R2<-1-SSE/SST
R2
