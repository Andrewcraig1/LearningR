#Understanding the data with function str, plot and which.max
str(FluTrain)

plot(FluTrain$Week,FluTrain$ILI, type="l")

max(FluTrain$ILI)
which.max(FluTrain$ILI)
FluTrain$Week[303]

max(FluTrain$Queries)
which.max(FluTrain$Queries)
FluTrain$Week[303]


hist(FluTrain$ILI)

FluTrain$logILI<-log(FluTrain$ILI)
plot(FluTrain$logILI, FluTrain$Queries)

# Model   model1<-lm(Price~AGST, data = wine)
FluTrend1<-lm((log(ILI))~Queries,data=FluTrain)
summary(FluTrend1)
cor(FluTrain$logILI,FluTrain$Queries)


##Model performance on Test data
str(FluTest)

PredTest1<-exp(predict(FluTrend1,newdata=FluTest))

which(FluTest$Week=="2012-03-11 - 2012-03-17")
PredTest1[11]
summary(PredTest1)


# Relative Error between the estimate (predicted value) and the observed value.
#=(observed ILI-Estimated ILI)/Observed ILI

(2.2934-2.187378)/2.2934

#RMSE
SSE<-sum((PredTest1-FluTest$ILI)^2)
SST<-sum((mean(FluTrain$ILI)-FluTest$ILI)^2)
R2<-1-SSE/SST
R2

SST
RMSE<-sqrt(SSE/nrow(FluTest))
RMSE

# Training a Time series Model

install.packages("zoo")
library(zoo)
#Train model 
ILILag2<-lag(zoo(FluTrain$ILI),-2,na.pad=TRUE)
FluTrain$ILILag2=coredata(ILILag2)
summary(FluTrain$ILILag2)


FluTrend2<-lm((log(ILI))~log(ILILag2)+Queries,data=FluTrain)
summary(FluTrend2)

# Test model
ILILag2<-lag(zoo(FluTest$ILI),-2,na.pad=TRUE)
FluTest$ILILag2=coredata(ILILag2)
summary(FluTest$ILILag2)

# Fill in first 2 missing values (na) in FluTest$ILILag2 from FluTraining$ILILag2
# FluTest$ILILag2[x]=FluTrain$ILILag2[y]
FluTest$ILILag2[1]=FluTrain$ILI[416]
FluTest$ILILag2[2]=FluTrain$ILI[417]

# Predict:
PredTest2<-exp(predict(FluTrend2,newdata=FluTest))
summary(PredTest2)

SSE<-sum((PredTest2-FluTest$ILI)^2)
SST<-sum((mean(FluTrain$ILI)-FluTest$ILI)^2)
R2<-1-SSE/SST
R2

SST
RMSE<-sqrt(SSE/nrow(FluTest))
RMSE
##Remember the smaller RMSE the better the model. 
#So PredTest2 model (RMSE=0.294) peformed better than PredTest1 model(RMSE=0.749)

