#### Predicting Loan Repayment

### Preparing the dataset
## binary dependent variable "not.fully.paid". If not repaid = "1"
str(loans)
summary(loans)

table(loans$not.fully.paid)
1533/(8045+1533)

##how many cases with missing data# Subset missing data
missing = subset(loans, is.na(log.annual.inc) | is.na(days.with.cr.line) | 
                         is.na(revol.util) | is.na(inq.last.6mths) | 
                         is.na(delinq.2yrs) | is.na(pub.rec))
nrow(missing)
# 62



## imputation
# We predict missing variable values using the available independent variables for each observation.
library(mice)
set.seed(144)
vars.for.imputation = setdiff(names(loans), "not.fully.paid")
imputed = complete(mice(loans[vars.for.imputation]))
loans[vars.for.imputation] = imputed

#Note that to do this imputation, we set vars.for.imputation to all variables in the data frame
# except for not.fully.paid, to impute the values using all of the other independent variables.


### Prediction Models: 
## Training and test data sets
library(caTools)
set.seed(144)
split<-sample.split(loans$not.fully.paid, SplitRatio = 0.70)
loansTrain<-subset(loans,split==TRUE)
loansTest<-subset(loans,split==FALSE)
nrow(loansTrain)
nrow(loansTest)

## Training model
loansnfpLog1<-glm(not.fully.paid~ .,data=loansTrain, family = "binomial")        
summary(loansnfpLog1)

#Prob 2.2 Consider two loan applications, which are identical other than the fact that the borrower
#in Application A has FICO credit score 700 while the borrower in Application B has FICO credit score 710.

logOddsA<-(( 9.187)+(0.3368)+( 0.8867192)+(-6.141e-01)+(-3.212e-01 )+(1.347e-01)+
                  (1.727e-01)+(-4.830e-01)+(4.120e-01)+( 6.110e-01)+1.275e-03 +
                  -4.337e-01+4.638e-03+(-9.317e-03*700)+2.371e-06+ 3.085e-06+
                  1.839e-03+8.437e-02+-8.320e-02+3.300e-01)
logOddsA

logOddsB<-(( 9.187)+(0.3368)+( 0.8867192)+(-6.141e-01)+(-3.212e-01 )+(1.347e-01)+
                   (1.727e-01)+(-4.830e-01)+(4.120e-01)+( 6.110e-01)+1.275e-03 +
                   -4.337e-01+4.638e-03+(-9.317e-03*710)+2.371e-06+ 3.085e-06+
                   1.839e-03+8.437e-02+-8.320e-02+3.300e-01)
logOddsB

logOddsA-logOddsB


oddsA<-exp(logOddsA)
oddsA

oddsB<-exp(logOddsB)
oddsB

oddsA/oddsB

# Prob 2.3
# Predict with test data
predicted.risk<- predict(loansnfpLog1, newdata=loansTest, type="response")
summary(predicted.risk)
# Create predicted.risk as variable.
loansTest$predicted.risk<-predicted.risk
table(loansTest$not.fully.paid, predicted.risk ==0.5)

#accuracy
(2413+0)/nrow(loansTest)

#baseline model
table(loansTest$not.fully.paid)
(2413)/nrow(loansTest)

### AUC
# AUC accuracy
library(ROCR)
ROCRpred<-prediction(predicted.risk,loansTest$not.fully.paid)
as.numeric(performance(ROCRpred, "auc")@y.values)
# 0.672

#Prob 3.1 "Smart Baseline"

loansnfpLog2<-glm(not.fully.paid~ int.rate, data=loansTrain, family = "binomial")        
summary(loansnfpLog2)

TestPrediction = predict(loansnfpLog2, newdata=loansTest, type="response")
summary(TestPrediction)

table(loansTest$not.fully.paid, TestPrediction == 0.5)

### AUC
# AUC accuracy
library(ROCR)
ROCRpred<-prediction(TestPrediction,loansTest$not.fully.paid)
as.numeric(performance(ROCRpred, "auc")@y.values)

### Profitability of an investment
## interest revenue payback$=c*exp(rt). c $ investment; int rate r; t years

10*exp(0.06*3)-10

## Prob 5.1 A simple investment strategy
# create new variable for an investment of $1
loansTest$profit<-1*exp(loansTest$int.rate*3)-1
loansTest$profit[loansTest$not.fully.paid==1]<--1
max(loansTest$profit)

#Prob6.1
highinterest<-subset(loansTest,int.rate>=0.15)
mean(highinterest$profit)
table(highinterest$profit)
110/437

#Prob 6.2
cutoff<-sort(highinterest$predicted.risk, decreasing=FALSE)[100]
cutoff
#cutoff= 0.1763305
selectedLoans<-subset(highinterest,predicted.risk<=cutoff)
sum(selectedLoans$profit)
table(selectedLoans$not.fully.paid)
