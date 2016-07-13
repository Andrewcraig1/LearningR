#### Polling Recitation

str(Polling)
#check for missing data
summary(Polling)

### Aproaches to handle missing data:
## Delete missing observations (but that can lead to loosing a lot of data)
## Or Delete variables with missing data (but these might be important)
## Or Fill missing data points with average values
## Or Multiple Imputation 

### Multiple imputation using "mice" package (Multiple Imputation by Chained Equations)
## Install "mice" from packages.
## Load "mice"
library(mice)
names(Polling)
# Create simpler data frame of variables with missing data plus other potential independent variables
simple<-Polling[c("Rasmussen","SurveyUSA","PropR","DiffCount")]
summary(simple)
set.seed(144)
imputed<-complete(mice(simple))
# note in this example the output shows 5 rounds of imputation occured to
# generate imputed values for missing values.
summary(imputed)
# This now shows the data frame has no missing values

## Now copy the "Rasmussen"and "SurveyUSA" data back into the original polling data frame.
Polling$Rasmussen<-imputed$Rasmussen
Polling$SurveyUSA<-imputed$SurveyUSA
# confirm missing data has imputed values (i.e. no NAs)
summary(Polling)

## Note in this example the random generation of imputed values is slightly different from course training set.
# this is OK - but i now load and use the imputed data set provided as "polling"

train<-subset(Polling, Year == 2004 | Year == 2008)
test<-subset(Polling, Year == 2012)

# Predict baseline model against which to compare the logistic regression model
# using the dependent variable(y), which in this case is the binomial category "Republican"=1 
table(train$Republican)
table(Train)
#  0  1 
# 47 53 
# Baseline Result means that it is the Republican is the more common outcome
# accuracy of the baseline for the training set is 53/(47+53)=53%
# This is a weak model as it always predicts republican.

# A Sophisticated (Smart) Baseline use the "sign" function
# what this function does is, if it's passed a positive number, it returns the value 1.
# If it's passed a negative number, it returns negative 1.
# And if it's passed 0, it returns 0.
# eg if Republican is pulling ahead by 20 it returns 1; if a democrat is pulling ahead bin 
# Rasmussen Poll it returns by say 10 it takes on a negative number (-10) and returns -1.table
sign(20)
sign(-10)
sign(0)

table(sign(train$Rasmussen))
# -1  0  1 
# 42  2 56 
# So the smart baseline predicted Republican was going to win in 56 instances, 
# Democrat in 42, and 2 instances inconclusive.Makes 2 mistakes.

# Compare training set of Rasmussen Poll against who actually won.
# Create confusion table (actual vs predict)
table(train$Republican, sign(train$Rasmussen))

#   -1  0  1
#  0 42  1  4
#  1  0  1 52

# What we can see is in the top left corner over here,we have 42 observations where the Rasmussen smart baseline
# predicted the Democrat would win,and the Democrat actually did win.There were 52 observations where the smart baseline predicted
# the Republican would win, and the Republican actually did win.Again, there were those two inconclusive observations.
# And finally, there were four mistakes. There were four times where the smart baseline model predicted
# that the Republican would win, but actually the Democrat won the state.
# This is a much smarter model than the simple model which made 47 mistakes on the same data where N=100 (i.e. accuracy was 53%)
# baseline accuracy of 94% (52+42)/100

### Check for multicolinearity amongst the independent variables and dependent variable. 
# correlate only on numeric data.
cor(train[c("Rasmussen","SurveyUSA","PropR","DiffCount", "Republican")])

mod1<-glm(Republican~PropR, data=train, family="binomial")
# Check performance with summary , note coefficients including AIC
# Note AIC is best used in selecting best model
summary(mod1)

pred1<-predict(mod1,type="response")
table(train$Republican, pred1 >=0.5)
# Looking at the table, This model makes 4 mistakes.

# Lets improve model performance by adding in more independent variables
# Select independent variables that are not highly correlated.
mod2<-glm(Republican~SurveyUSA + DiffCount, data=train, family="binomial")
summary(mod2)
pred2<-predict(mod2,type="response")
table(train$Republican, pred2 >=0.5)
#     FALSE TRUE
# 0    45    2
# 1     1   52

# Direction is positive - makes sense; 
# Model Has a stronger model as a lower AIC, 
# but weakness is indep var are not statistically sig at 0.05


### Evalutae model on testing Set
## Smart baseline method on test data set.
table(test$Republican, sign((test$Rasmussen)))

## test prediction
testprediction<-predict(mod2,newdata = test,type="response")
table(test$Republican, testprediction >=0.5)
#   FALSE TRUE
# 0    23    1
# 1     0   21

# Threshold of >=0.5 selected and does not need to be evaluated.
# nd the moment of truth, we're finally going to table the test set Republican value against the test prediction
# being greater than or equal to 0.5, at least a 50% probability of the Republican winning.
# And we see that for this particular case, in all but oneof the 45 observations in the testing set, we're correct.
# Now, we could have tried changing this threshold from 0.5 to other values and computed out an ROC curve,
# but that doesn't quite make as much sense in this setting where we're just trying to accurately predict
# the outcome of each state and we don't care more about one sort of error-- when we predicted Republican
# and it was actually Democrat-- than the other, where we predicted Democrat and it was actually Republican.
# So in this particular case, we feel OK just using the cutoff of 0.5 to evaluate our model.

# Look at the mistake in prediction

subset(test, testprediction >=0.5 & Republican ==0)
# Overall it outperforms the smart baseline model prediction