
## Data preparation
str(pisatrain)
str(pisatest)

# mean reading score for males and females
tapply(pisatrain$readingScore, pisatrain$male=="1", mean, na.rm=TRUE)

# Locating missing values using function "summary"
summary(pisatrain)

# Removing missing values with function and argument"na.omit(mydata)" 

pisatrain<-na.omit(pisatrain)
pisatest<-na.omit(pisatest)

## Factor variables
# unordered factor: has no natural ordering between levels e.g. region a, region b
# ordered factor:has natural ordering between levels e.g. large, medium, small.This includes male=1. female=0

# raceeth has 7 levels. We can select which one to be treated separate from the others by converting them
# to binary variable. So if I want to regress "White=1" the 6 remaining factors become "0". 
# The default coding is for R to number each factor alphabetically asian=1, Maori=2, pacific=3 etc

str(pisatest)

#Set the reference level
pisatrain$raceeth<-relevel(pisatrain$raceeth,"White")
pisatest$raceeth<-relevel(pisatest$raceeth,"White")

str(pisatrain)
# model

lmScore<-lm(readingScore~.,data=pisatrain)
summary(lmScore)

#Computing root mean square(RMSE) of the model to predict reading score

SSE<-sum(lmScore$residuals^2)
RMSE<-sqrt(SSE/nrow(pisatrain))
RMSE
mean(pisatrain$readingScore)

(29.542707*11)-(29.542707*9)

# Predicting unseen data
PredTest<-predict(lmScore,newdata=pisatest)
summary(PredTest)
#summary(PredTest) 


# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#353.2   482.0   524.0   516.7   555.7   637.7 

# Now calc out of sample R-squared to check predictions goodness of fit.
SSE<-sum((PredTest-pisatest$readingScore)^2)
SST<-sum((mean(pisatrain$readingScore)-pisatest$readingScore)^2)
R2<-1-SSE/SST
R2

SST
RMSE<-sqrt(SSE/nrow(pisatest))
RMSE

# predicted baseline vale (mean of dependent variable from training data "y")
mean(pisatrain$readingScore)
