###Assign 1 wiki
#This is a fascinating assignment showing ways to explore and create secondary data 
# to increase accuracy of the CART model without increasing model complextity. And the use of Grepl command to create secondary data.


# Load the dataset. Fot txt data sets add argument " stringsAsFactors=FALSE"

wiki<- read.csv("wiki.csv", stringsAsFactors=FALSE)

str(wiki)

# # Run Language Settings
Sys.setlocale("LC_ALL", "C")

# Look at emails

wiki$Added[1]
# alternative argument to wrap so its easier to read
strwrap(wiki$Added[1])

# P1.1 Convert Vandal into factor, and id number of cases vandalised
wiki$Vandal<- as.factor(wiki$Vandal)
table(wiki$Vandal)

##P1.2 Bag of Words  

#Load tm package

library(tm)

# Create corpus

corpusAdded <- Corpus(VectorSource(wiki$Added))

corpusAdded[[1]]
strwrap(corpusAdded[1])


# Pre-process data
corpusAdded <- tm_map(corpusAdded, tolower)
corpusAdded <- tm_map(corpusAdded, PlainTextDocument)


corpusAdded <- tm_map(corpusAdded, removePunctuation)

corpusAdded <- tm_map(corpusAdded, removeWords, stopwords("english"))

corpusAdded <- tm_map(corpusAdded, stemDocument)

# Look at first email
corpusAdded[[1]]
strwrap(corpusAdded[1])

#  Now the Added wiki in this corpus are ready for our machine learning algorithms.

# Create matrix: dtm means document-term matricies

dtm <- DocumentTermMatrix(corpusAdded)
dtm


##P1.3 Bag of Words
# Remove sparse terms. In this recitation remove terms that don't appear in at least 0.3% of the documents
sparseAdded <- removeSparseTerms(dtm, 0.997)
sparseAdded
# Now No. of terms=166

##P1.4 part 1: Bag of Words: 
# Create data frame for wordsAdded
wordsAdded <- as.data.frame(as.matrix(sparseAdded))

# Convert sparseAdded to a data frame called wordsAdded, 
# and then prepend all the words with the letter A

colnames(wordsAdded) <- paste("A", colnames(wordsAdded))

##1.4 Bag of Words part 2: Create corpus for words "Removed"

corpusRemoved <- Corpus(VectorSource(wiki$Removed))

corpusRemoved[[1]]
strwrap(corpusRemoved[1])

# Pre-process data for Removed
corpusRemoved <- tm_map(corpusRemoved, tolower)
corpusRemoved <- tm_map(corpusRemoved, PlainTextDocument)


corpusRemoved <- tm_map(corpusRemoved, removePunctuation)

corpusRemoved <- tm_map(corpusRemoved, removeWords, stopwords("english"))

corpusRemoved <- tm_map(corpusRemoved, stemDocument)
#dtm means document-term matricies
dtm2 <- DocumentTermMatrix(corpusRemoved)
dtm2

sparseRemoved <- removeSparseTerms(dtm2, 0.997)
sparseRemoved 
#terms = 162

# Create data frame for wordsRemoved
wordsRemoved <- as.data.frame(as.matrix(sparseRemoved))

# Convert sparseAdded to a data frame called wordsAdded, 
# and then prepend all the words with the letter A

colnames(wordsRemoved) <- paste("R", colnames(wordsRemoved))

##1.5 Bag of Words: Baseline
# Combine data frames

wikiWords <- cbind(wordsAdded, wordsRemoved)
str(wikiWords)

# Add in the outcome variable
wikiWords$Vandal <- wiki$Vandal

## Create test and Train data sets

# Split the data

library(caTools)

set.seed(123)

spl <- sample.split(wikiWords$Vandal, 0.7)

train <- subset(wikiWords, spl == TRUE)
test <- subset(wikiWords, spl == FALSE)

# Baseline accuracy on test set
# What is the accuracy on the test set of a baseline method that always predicts
#"not vandalism" (the most frequent outcome)
table(test$Vandal)
618/nrow(test)

##P1.6
# Build a CART model

library(rpart)
library(rpart.plot)

# Predicting "Vandal" and using all the terms, classification model - therefore method = class
# use default parameters (i.e. don't set values for minibucket or cp)
wikiCART <- rpart(Vandal~., data=train, method="class")

prp(wikiCART)

# Make predictions on the test set

pred <- predict(wikiCART, newdata=test)
pred[1:10,]

# Select predicted probabiities of a wiki being vandalised
pred.prob <- pred[,2]

# Compute accuracy, cut off threshold used 0.5

table(test$Vandal, pred.prob >= 0.5)
(618+12)/nrow(test)
# 0.542
# There is no reason to think there was anything wrong with the split. CART did not overfit,
#which you can check by computing the accuracy of the model on the training set. 
#Over-sparsification is plausible but unlikely, since we selected a very high sparsity parameter.
# The only conclusion left is simply that bag of words didn't work very well in this case.

### P2.1 Problem-specific Knowledge:
# Grepl function
str(wiki)

wikiWords2 <- wikiWords
#Make a new column in wikiWords2 that is 1 if "http" was in Added:

wikiWords2$HTTP <- ifelse(grepl("http",wiki$Added,fixed=TRUE), 1, 0)
table(wikiWords2$HTTP)
# revisions that added a link: 217

###p2.2 New CART model that includes HTTP as an independent variable

wikiTrain2 <- subset(wikiWords2, spl==TRUE)

wikiTest2 <- subset(wikiWords2, spl==FALSE)

wikiCART2 <- rpart(Vandal~., data=wikiTrain2, method="class")

prp(wikiCART2)

# Make predictions on the test set

pred2 <- predict(wikiCART2, newdata=wikiTest2)


# Select predicted probabiities of a wiki being vandalised 
pred.prob2 <- pred2[,2]

# Compute accuracy, cut off threshold used 0.5

table(wikiTest2$Vandal, pred.prob2 >= 0.5)
(609+57)/nrow(wikiTest2)
#accuracy=0.573

### prob 2.3 Numbers of words added and removed analysing dtm (Added) and dtm2 (Removed)
wikiWords2$NumWordsAdded <- rowSums(as.matrix(dtm))

wikiWords2$NumWordsRemoved <- rowSums(as.matrix(dtm2))

# Average number of words added
summary(wikiWords2$NumWordsAdded)

### Prob 2.4 New CART Model with Words added and Words removed


wikiTrain2 <- subset(wikiWords2, spl==TRUE)

wikiTest2 <- subset(wikiWords2, spl==FALSE)

wikiCART2 <- rpart(Vandal~., data=wikiTrain2, method="class")

prp(wikiCART2)

# Make predictions on the test set

pred2 <- predict(wikiCART2, newdata=wikiTest2)


# Select predicted probabiities of a wiki being vandalised 
pred.prob2 <- pred2[,2]

# Compute accuracy, cut off threshold used 0.5

table(wikiTest2$Vandal, pred.prob2 >= 0.5)
(514+248)/nrow(wikiTest2)

# The addition increased accuracy to 0.655

###Prob 3.1 Using non-textual data

wikiWords3 <- wikiWords2

# add the two original variables Minor and Loggedin to this new data frame:
        
wikiWords3$Minor <- wiki$Minor

wikiWords3$Loggedin <- wiki$Loggedin

#New CART Model with added independent variables


wikiTrain3 <- subset(wikiWords3, spl==TRUE)

wikiTest3 <- subset(wikiWords3, spl==FALSE)

wikiCART3 <- rpart(Vandal~., data=wikiTrain3, method="class")

prp(wikiCART3)

# Make predictions on the test set

pred3 <- predict(wikiCART3, newdata=wikiTest3)


# Select predicted probabiities of a wiki being vandalised 
pred.prob3 <- pred3[,2]

# Compute accuracy, cut off threshold used 0.5

table(wikiTest3$Vandal, pred.prob3 >= 0.5)
(595+241)/nrow(wikiTest3)

# The number of stems is three. Not a complex CART model.
