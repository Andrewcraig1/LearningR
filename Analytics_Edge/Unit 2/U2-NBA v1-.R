summarystr(NBA)

#Used table of wins and playoffs to estimate threshold of number of wins required to make playoffs ()
table(NBA$W,NBA$Playoffs)

NBA$PTSdiff<-NBA$PTS-NBA$oppPTS

plot(NBA$PTSdiff, NBA$W)

WinsReg<-lm(W~PTSdiff, data=NBA)
summary(WinsReg)

# Regression equation for Wins: W=41+0.0326*PTdiff
# If wins est to be >= 42
# PTdiff= W-41/0.036

(42-41)/0.0326
# PTdiff=30.67 So we need to score at least 31 more points than we allow to win >=42 games.

#So now build an eqn to predict points scored using NBA stats.

PointsReg<-lm(PTS~X2PA+X3PA+FTA+AST+ORB+DRB+TOV+STL+BLK, data=NBA)
summary(PointsReg)

#Look at residuals
PointsReg$residuals
SSE<-sum(PointsReg$residuals^2)
SSE
#RMSE is root mean of squared error
RMSE<-sqrt(SSE/nrow(NBA))
RMSE
mean(NBA$PTS)
# so an RMSE of 184 relative to a mean of 8370 points scored in a season is OK.

# Improve regression performance by removing some variables
summary(PointsReg)
# select variable with the highest p value which is the least statistically significant variable to the model.

PointsReg2<-lm(PTS~X2PA+X3PA+FTA+AST+ORB+DRB+STL+BLK, data=NBA)
summary(PointsReg2)
# Rsq almost the same - so good choice.
# Now lets try remving another variable -the highest prob for PointsReg2.Remove DRB.

PointsReg3<-lm(PTS~X2PA+X3PA+FTA+AST+ORB+STL+BLK, data=NBA)
summary(PointsReg3)
# Rsq is the same. Lets try and remove blocks

PointsReg4<-lm(PTS~X2PA+X3PA+FTA+AST+ORB+STL, data=NBA)
summary(PointsReg4)
# Rsq remains the same. No check SSE

SSE4<-sum(PointsReg4$residuals^2)
SSE4
#RMSE is root mean of squared error
RMSE4<-sqrt(SSE4/nrow(NBA))
RMSE4
# RMSE4 essentially the same -  So we've narrowed down to a better model.

#Using "predict" command with arguments "PointsReg4" "newdata=NBA_test".  Making predictions for 2012-13 Season. Load test set.
str(NBA_test)

PointsPrediction<-predict(PointsReg4,newdata=NBA_test)
# Now calc out of sample R-squared to check predictions goodness of fit.
SSE<-sum((PointsPrediction-NBA_test$PTS)^2)
SST<-sum((mean(NBA$PTS)-NBA_test$PTS)^2)
R2<-1-SSE/SST
R2
#Rsq is 0.813 
#Also can calc Root mean Squared Error (RMSE)
RMSE<-sqrt(SSE/nrow(NBA_test))
RMSE
RMSE4
# RMSE (predicted data) is 196.37, for the model using trial data it was 184.49. So its not too bad.