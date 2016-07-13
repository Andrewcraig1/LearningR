### Boston Housing
str(boston)

## Plot observatiions by langitude and longitude: Start with plot function
# then highlight this using points function
plot(boston$LON,boston$LAT)
# see my R notes U4 Recitation Housing - add charles river locations
points(boston$LON[boston$CHAS==1], boston$LAT[boston$CHAS==1], col="blue", pch=19)
# points highlight MIT (TRAC==3531)
points(boston$LON[boston$TRACT==3531],boston$LAT[boston$TRACT==3531],col="red", pch=20)
# look at distribution of pollution NOX
summary(boston$NOX)
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#0.3850  0.4490  0.5380  0.5547  0.6240  0.8710 
# Highligh pollution - Points where NOX >0.55
points(boston$LON[boston$NOX>=0.55], boston$LAT[boston$NOX>=0.55], col="green", pch=20)

# houing prices
plot(boston$LON,boston$LAT)
summary(boston$MEDV)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 5.00   17.02   21.20   22.53   25.00   50.00 

#plot above median proce points
points(boston$LON[boston$MEDV>=21.2], boston$LAT[boston$MEDV>=21.2], col="red", 
       pch=20)

# Video 3 (see R notes  U4 Recitation Housing)
plot(boston$LAT,boston$MEDV)
plot(boston$LON,boston$MEDV)

# Try a regression model
latlonlm<- lm(MEDV ~ LAT + LON, data=boston)
summary(latlonlm)

plot(boston$LON,boston$LAT)
points(boston$LON[boston$MEDV>=21.2], boston$LAT[boston$MEDV>=21.2], col="red",
       pch=20)
# what does the linear regression model think is above median?
# look at calculated fitted values as generated in  reg model "latlonlm"
latlonlm$fitted.values
points(boston$LON[latlonlm$fitted.values>=21.2], boston$LAT[latlonlm$fitted.values>=21.2], col="blue",pch="$")
# here linear regression model not doing a good job, ignoring everything
#on RHS of the picture.

### Video 4: Regression Trees
library(rpart)
library(rpart.plot)
# Note in this first example no minbucket specified - in this example it leads to an overfitting problem
latlontree<-rpart(MEDV ~ LAT + LON, data = boston)
prp(latlontree)

plot(boston$LON,boston$LAT)
points(boston$LON[boston$MEDV>=21.2], boston$LAT[boston$MEDV>=21.2], col="red",
       pch=20)
fittedvalues<-predict(latlontree)
points(boston$LON[fittedvalues>=21.2], boston$LAT[fittedvalues>=21.2], col="blue",
       pch="$")
# refer notes (see R notes  video 5: U4 Recitation Housing: RegressionTree fit)

# Overfitting problem - deal with this by reducing the minbucket size.
# specifiy minbucket=50
latlontree<-rpart(MEDV ~ LAT + LON, data = boston, minbucket=50)
plot(latlontree)
text(latlontree)
plot(boston$LON,boston$LAT)
# use v and h abline to identify splits.And refer this geo plot (LON,LAT) with latlontree splits
#the first split at LON >=71.07
abline(v=-71.07)
# split at LAT42.2
abline(h=42.2)

abline(h=42.17)
points(boston$LON[boston$MEDV>=21.2], boston$LAT[boston$MEDV>=21.2], col="red", pch=20)


### Video 5: Puuting it all together
library(caTools)
set.seed(123)
split<-sample.split(boston$MEDV, SplitRatio = 0.7)
train<-subset(boston, split==TRUE)
test<- subset(boston, split==FALSE)

# create linear model
linreg <- lm(MEDV ~ LAT + LON + CRIM + ZN + INDUS + CHAS + NOX + RM + AGE + DIS + RAD + 
                    TAX + PTRATIO, data=train)
summary(linreg)
# some of these might be cross correlated
# Adjusted R2 is good.

linreg.pred<-predict(linreg, newdata=test)
linreg.sse<- sum((linreg.pred - test$MEDV)^2)
# linreg.sse 3037.088

# can we beat this using RegressionTrees
tree <- rpart(MEDV ~ LAT + LON + CRIM + ZN + INDUS + CHAS + NOX + RM + AGE + DIS + RAD + 
                     TAX + PTRATIO, data=train)
prp(tree)
# Refer to notes for interpretation
## Predict with "tree"
tree.pred<-predict(tree, newdata = test)
tree.sse<- sum((tree.pred - test$MEDV)^2)            
tree.sse
# 4328.988
## Linear regression SSE= 3037; and RegressionTree SSE=4329
# Therefore RegresionTree not as good at predicting as Linear Regression model

## Let's see if we can improve on our results using the complexity parameter "cp parameter"
library(caret)
library(e1071)
# make a 10 fold cross validation
tr.control<-trainControl(method = "cv", number = 10)
cp.grid<- expand.grid(.cp = (0:10)* 0.001)
0:10 *0.001

tr <- train(MEDV ~ LAT + LON + CRIM + ZN + INDUS + CHAS + NOX + RM + AGE + DIS +
                    RAD + TAX + PTRATIO, data = train, method = "rpart",
                trControl = tr.control, tuneGrid = cp.grid)

best.tree<-tr$finalModel
prp(best.tree)
# best.tree that's the model that corresponds to 0.001.

best.tree.pred<-predict(best.tree,newdata=test)
best.tree.sse<-sum((best.tree.pred-test$MEDV)^2)
best.tree.sse 
#  best.tree.sse = 3675.766
# This regressiontree has improved performance by using the complexity parameter, 
# but linear regression model still has a better SSE = 3037.
