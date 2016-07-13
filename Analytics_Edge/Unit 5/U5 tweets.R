#### Tweets
str(tweets)

### Run Language Settings
#If you downloaded and installed R in a location other than the United States, 
#you might encounter some issues when using the bag of words approach 
#(since the pre-processing tasks used here depend on the English language). 
#To fix this, you will need to type in your R console:
        
#        Sys.setlocale("LC_ALL", "C")

#This will only change the locale for your current R session, so please make a note
#to run this command when you are working on any lectures or exercises that might depend 
#on the English lanugage (for example, removing stop words).

Sys.setlocale("LC_ALL", "C")

### Important note
#run the following command immediately after converting all of the words to lowercase letters
#(it converts all documents in the corpus to the PlainTextDocument type):
        
#        corpus = tm_map(corpus, PlainTextDocument)

### Reading in Data Note: "stringsAsFactors=FALSE." The read command is more reliable here

tweets<-read.csv("tweets.csv",stringsAsFactors=FALSE)
str(tweets)

# New variable Negative Sentiment
tweets$Negative<-as.factor(tweets$Avg <= -1)
table(tweets$Negative)

###Preprocessing data
### Install "tm" package for precleaning data: Loads natural Language processing package.
### Install "SnowballC" package  this helps use the "tm" package
library(tm)
library(SnowballC)

# Refer to "Introduction to the tm Package Text Mining in R" in "My R Cook Book" file



### Convert tweets to a corpus (i.e. a collection of documents)
corpus<-Corpus(VectorSource(tweets$Tweet))
corpus
# Content:  documents: 1181
corpus[[1]]
#corpus [[1]]=101 char 
writeLines(as.character(corpus[[1]]))
#I have to say, Apple has by far the best customer care service I have ever received! @Apple @AppStore

## shift text to lower case
corpus = tm_map(corpus, tolower)
corpus <- tm_map(corpus, PlainTextDocument)
# Lets look
corpus[[1]]
#corpus [[1]]=101 char 
writeLines(as.character(corpus[[1]]))
#[1] "i have to say, apple has by far the best customer care service i have ever received! @apple @appstore"

## Now remove punctuation
corpus<-tm_map(corpus,removePunctuation)
#Lets look
corpus[[1]]
#corpus [[1]]=97 char 
writeLines(as.character(corpus[[1]]))
# [1] "i have to say apple has by far the best customer care service i have ever received apple appstore"

##Remove stop words and selected words (in this example it's "apple")
# list stop words in English language
stopwords("english")[1:10]
# stopwords("english")[1:10]
# [1] "i"         "me"        "my"        "myself"    "we"        "our"       "ours"     
# [8] "ourselves" "you"       "your"   

corpus<-tm_map(corpus,removeWords,c("apple",stopwords("english")))
#Lets look
corpus[[1]]
# corpus [[1]]=67 char 
writeLines(as.character(corpus[[1]]))
# "   say    far  best customer care service   ever received  appstore"

## Stem our documents
corpus <- tm_map(corpus, stemDocument)
#Lets look
corpus[[1]]
# corpus [[1]]=61 char 
writeLines(as.character(corpus[[1]]))
#  say    far  best custom care servic   ever receiv  appstor

### Video 6 "Bag of Words"

# Create matrix

frequencies <- DocumentTermMatrix(corpus)

frequencies

# Look at matrix 
# DocumentTermMatrix (documents: 1181 (ie tweets), terms: 3289(ie different words))
#inspect(frequencies[documents,terms])

inspect(frequencies[1000:1005,505:515])

# Check for sparsity
# findFreqTerms(frequencies,lowfreq = minimum number of times a term must appear
# before it is displayed)

findFreqTerms(frequencies, lowfreq=20)

# Remove sparse terms (0.995 means keep only terms that appear 0.5%  or more in tweets)

sparse <- removeSparseTerms(frequencies, 0.995)
sparse

# Convert to a data frame

tweetsSparse <- as.data.frame(as.matrix(sparse))

# Make all variable names R-friendly

colnames(tweetsSparse) <- make.names(colnames(tweetsSparse))

# Add dependent variable

tweetsSparse$Negative <- tweets$Negative

# Split the data

library(caTools)

set.seed(123)

split <- sample.split(tweetsSparse$Negative, SplitRatio = 0.7)

trainSparse <- subset(tweetsSparse, split==TRUE)
testSparse <- subset(tweetsSparse, split==FALSE)


### Video 7

# Build a CART model

library(rpart)
library(rpart.plot)

tweetCART <- rpart(Negative ~ ., data=trainSparse, method="class")

prp(tweetCART)

# Evaluate the performance of the model
predictCART <- predict(tweetCART, newdata=testSparse, type="class")

table(testSparse$Negative, predictCART)
#predictCART confusion matrix
#F      ALSE TRUE
#FALSE   294    6
#TRUE     37   18

# Compute accuracy

(294+18)/(nrow(testSparse))
# [1] 0.87887

# Baseline accuracy 

table(testSparse$Negative)
#FALSE  TRUE 
# 300    55 
300/(300+55)
#0.845

# Random forest model

library(randomForest)
set.seed(123)

tweetRF <- randomForest(Negative ~ ., data=trainSparse)

# Make predictions:
predictRF <- predict(tweetRF, newdata=testSparse)

table(testSparse$Negative, predictRF)
#predictRF Confusion table
#      FALSE TRUE
#FALSE   293    7
#TRUE     34   21

# Accuracy:
(293+21)/(293+7+34+21)
#0.885

##QQ Build a logistic regression model

tweetLog<-glm(Negative ~ ., data=trainSparse, family = "binomial")        
summary(tweetLog)

predictions <- predict(tweetLog, newdata=testSparse, type="response")
table(testSparse$Negative,predictions >= 0.5)
#        FALSE TRUE
#FALSE   253   47
#TRUE     23   32

#accuracy
(253+32)/nrow(testSparse)
# 0.803

# This is worse than the baseline. If you were to compute the accuracy on the training set instead, 
#you would see that the model does really well on the training set - 
#this is an example of over-fitting. The model fits the training set really well, but does not perform well 
#on the test set. A logistic regression model with a large number of variables is particularly at risk for overfitting.

