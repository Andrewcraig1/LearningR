#### U3 Assignment 1-Songs

### Understanding the data
str(songs)
summary(songs)
table(songs$year)
summary(songs$Top10)
names(songs)

mj<- subset(songs, artistname == "Michael Jackson")
which(mj$Top10=="1")
mj$songtitle[18]

rm(mj)

table(songs$timesignature)

which.max(songs$tempo)
songs$songtitle[6206]

### Creating Prediction Model

SongsTrain<-subset(songs, year<="2009")
Songstest<-subset(songs, year=="2010")
#Outcome variable is whether or not a song will make it to the Top10

##Model1


# But lets first Remove variables we don't want to use in our model test and training data sets
nonvars<-c("year","songtitle","artistname","songID", "artistID")
# NB In this data I initially made a mistake and removed only 4 instead of 5 variables.
# The result was a dataset too big for analysis to proceed.

SongsTrain<-SongsTrain[,!(names(SongsTrain)%in%nonvars)]
Songstest<-Songstest[,!(names(Songstest)%in%nonvars)]

# Logistic Model 1
SongsLog1<-glm(Top10~ .,data=SongsTrain, family = "binomial")        
summary(SongsLog1)

cor(SongsTrain$energy,SongsTrain$loudness)
# Create Model 2, which is Model 1 without the independent variable "loudness". 
# This can be done with the following command:
SongsLog2<-glm(Top10~ .-loudness,data=SongsTrain, family = "binomial")        
summary(SongsLog2)

# Create Model 3, which is Model 1 without the independent variable "energy". 
# This can be done with the following command:
SongsLog3<-glm(Top10~ .-energy,data=SongsTrain, family = "binomial")        
summary(SongsLog3)

## Model 3 Predict
TestPrediction = predict(SongsLog3, newdata=Songstest, type="response")
table(Songstest$Top10, TestPrediction > 0.45)
#Overall accuracy
(309+19)/nrow(Songstest)
#0.879

## baseline predict NOT going to be a hit(i.e. Top1=FALSE (i.e."0")
table(Songstest$Top10)
314/(314+59)
# 0.841

#sensitivity
19/(40+19)
#specificty
309/(309+5)
