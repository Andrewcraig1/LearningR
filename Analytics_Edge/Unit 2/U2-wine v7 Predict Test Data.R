### Video 7: Predictive Ability: Building a model that does well predicting data it has never seen before
### The data that we use to build a model is often called TRAINING DATA. New data is called TEST DATA.

### Prediction with test data

str(wineTest)

# Use "predict" function and include the argument "newdata" instead of "data"
predictTest<-predict(model4,newdata=wineTest)