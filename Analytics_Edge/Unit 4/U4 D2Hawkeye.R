### D2Hawkeeye
str(Claims)
# Predicting Cost
#variable value = 1 if patient had a diagnosis code for a particular disease
# variable value = 0 otherwise.

# Percentage of patients in each cost bucket
table(Claims$bucket2009)/nrow(Claims)

# BucketNo:             1           2           3           4           5 
#%              0.671267781 0.190170413 0.089466272 0.043324855 0.005770679 

##Trial and Sample Data with split
library(caTools)
set.seed(88)
spl<-sample.split(Claims$bucket2009, SplitRatio = 0.6)
ClaimsTrain<-subset(Claims, spl==TRUE)
ClaimsTest<-subset(Claims, spl == FALSE)


#QQ
mean(ClaimsTrain$age)
table(ClaimsTrain$diabetes)/nrow(ClaimsTrain)
# 0         1 
# 0.6191017 0.3808983 

## Baseline Method and Penalty Matrix
#       The baseline method would predict that the cost bucket for a patient in 2009
#       will be the same as it was in 2008.

# Generate Baseline Classification matrix: table(actual outcomes,predicted outcomes)
table(ClaimsTest$bucket2009,ClaimsTest$bucket2008)

#             1      2      3      4      5
#       1 110138   7787   3427   1452    174
#       2  16000  10721   4629   2931    559
#       3   7006   4629   2774   1621    360
#       4   2688   1943   1415   1539    352
#       5    293    191    160    309    104
accuracy<-(110138+10721+2774+1539+104)/nrow(ClaimsTest)
accuracy
#accuracy of matrix=0.68

#Generate Penalty Matrix(Outcome column, Forecast row)
PenaltyMatrix<-matrix(c(0,1,2,3,4,2,0,1,2,3,4,2,0,1,2,6,4,2,0,1,8,6,4,2,0),byrow=TRUE,nrow=5)
PenaltyMatrix
#      [,1] [,2] [,3] [,4] [,5]
#[1,]    0    1    2    3    4
#[2,]    2    0    1    2    3
#[3,]    4    2    0    1    2
#[4,]    6    4    2    0    1
#[5,]    8    6    4    2    0

# Compute the penalty error of the baseline method by multiplying classification matrix by the penalty matrix, 
#       and then take each number in the classification matrix 
#       and multiply it by the corresponding number in the penalty matrix.
#       sum it up and divide by the number of observations in the data set

# as.matrix(table(ClaimsTest$bucket2009,ClaimsTest$bucket2008)
          #as.matrix table
          #         1     2     3     4     5
          #    1     0  7787  6854  4356   696
          #    2 32000     0  4629  5862  1677
          #    3 28024  9258     0  1621   720
          #    4 16128  7772  2830     0   352
          #    5  2344  1146   640   618     0
          
PenaltyError<-sum(as.matrix(table(ClaimsTest$bucket2009,ClaimsTest$bucket2008))*PenaltyMatrix)/nrow(ClaimsTest)
PenaltyError
# So the accuracy of the baseline classification matrix was 0.68 and
        # the Penalty Error for the baseline method is 0.74
# The goal now is to generate a CART model with an accuracy greater than 0.68, 
        # and a penalty error less than 0.74

### Build CART Model
library(rpart)
library(rpart.plot)

ClaimsTree <- rpart(bucket2009 ~ age + alzheimers + arthritis + cancer + copd + 
                            depression + diabetes + heart.failure + ihd + kidney + 
                            osteoporosis + stroke + bucket2008 + reimbursement2008, 
                    data=ClaimsTrain, method="class", cp=0.00005, parms = list(loss=PenaltyMatrix))
prp(ClaimsTree)

PredictTest<-predict(ClaimsTree, newdata=ClaimsTest, type="class")
# Accuracy of PredictTest model=0.71, but by adding the loss matrix argument "parms = list(loss=PenaltyMatrix" 
# this reduced to = 0.647
table(ClaimsTest$bucket2009, PredictTest)
(94310+18942+4692+636+2)/nrow(ClaimsTest)

# Penalty error=0.758 but by adding the loss matrix argument "parms = list(loss=PenaltyMatrix" 
# this reduced to = 0.642
sum(as.matrix(table(ClaimsTest$bucket2009, PredictTest))*PenaltyMatrix)/nrow(ClaimsTest)

#refer to U4 D2Hawkeye notes point ###3
# Our accuracy is now lower than the baseline method, but our penalty error is also much lower.

#QQ
# According to the penalty matrix, some of the worst types of errors are to predict bucket 1
# when the actual cost bucket is higher. Therefore, the model with the penalty matrix 
# predicted bucket 1 less frequently.



