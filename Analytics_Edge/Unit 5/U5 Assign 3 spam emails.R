# # Run Language Settings
Sys.setlocale("LC_ALL", "C")

# Load the dataset. Fot txt data sets add argument " stringsAsFactors=FALSE"

emails <- read.csv("emails.csv", stringsAsFactors=FALSE)

str(emails)
table(emails$spam)

#Q1.5
# Max number of characters in abstrac
max(nchar(emails$text))

which.min(nchar(emails$text))
#1992
emails$text[1992]

###P2 Preparing the Corpus
##P2.1 


# Load tm package

library(tm)


# Create corpus

corpus <- Corpus(VectorSource(emails$text))

corpus[[1]]
strwrap(corpus[1])

# Pre-process data
corpus <- tm_map(corpus, tolower)
corpus <- tm_map(corpus, PlainTextDocument)


corpus <- tm_map(corpus, removePunctuation)

corpus <- tm_map(corpus, removeWords, stopwords("english"))

corpus <- tm_map(corpus, stemDocument)

#Check stop words = 174
length(stopwords("english"))

# Create matrix

dtm <- DocumentTermMatrix(corpus)
dtm
# no of terms=28687

# Remove sparse terms. In this recitation remove terms that don't appear in at least 5% of the documents
spdtm <- removeSparseTerms(dtm, 0.95)
spdtm
# Now no of terms=330

# Create data frame
emailsSparse <- as.data.frame(as.matrix(spdtm))

#Make variable names
colnames(emailsSparse) <- make.names(colnames(emailsSparse))

#P2.3 which.max
which.max(colSums(emailsSparse))

##P2.4
# Add in the outcome variable
emailsSparse$spam <- emails$spam

# We can read the most frequent terms in the ham dataset (ie spam=0) 
sort(colSums(subset(emailsSparse, spam == 0)))
# We can read the most frequent terms in the ham dataset (ie spam=1). Dont count the word "spam" as this is the dependent variable. 
sort(colSums(subset(emailsSparse,spam == 1)))


###P3 Building machine Learning Models
##p3.1
#convert dependent variable "spam" to a factor
emailsSparse$spam <- as.factor(emailsSparse$spam)

# Split the data

library(caTools)

set.seed(123)

spl <- sample.split(emailsSparse$spam, 0.7)

train <- subset(emailsSparse, spl == TRUE)
test <- subset(emailsSparse, spl == FALSE)

##QQ Build a logistic regression model

spamLog<-glm(spam ~ ., data=train, family = "binomial")        
summary(spamLog)

pred.spamLog <- predict(spamLog, data=test, type="response")
table(pred.spamLog)

# Build a CART model

library(rpart)
library(rpart.plot)

spamCART <- rpart(spam ~ ., data=train, method="class")

prp(spamCART)

# Random forest model

library(randomForest)
set.seed(123)

spamRF <- randomForest(spam ~ ., data=train)

# Make predictions on the training set for each model

pred.spamLog <- predict(spamLog, data=train, type="response")
table(train$spam, pred.spamLog >= 0.5)



# Compute accuracy, cut off threshold < 0.00001

table(train$spam, pred.prob.spamLog > 0.99999)
4010-954-3056

#3.4
table(train$spam, pred.spamLog)
#predictRF Confusion table
(3052+954)/nrow(train)

#3.5 AUC spamlog
# AUC accuracy
library(ROCR)
ROCRpred<-prediction(pred.prob.spamLog,train$spam)
as.numeric(performance(ROCRpred, "auc")@y.values)
# 0.999

#3.6 Accuracy CART(note for probailites no argument type="class")
pred.CART <- predict(spamCART, data=train)

table(train$spam, pred.CART)

#accuracy
(2885+894)/nrow(train)

3.7
# ROC curve

library(ROCR)
# Select predicted probabiities of an spam being responsive 
pred.probCART <- pred.CART[,2]

predROCR.CART <- prediction(pred.probCART, train$spam)

# Use performance function to Extract tpr and fpr. tpr means true positive rate.fpr means false positive rate
perfROCR.CART <- performance(predROCR.CART, "tpr", "fpr")

plot(perfROCR.CART, colorize=TRUE)




