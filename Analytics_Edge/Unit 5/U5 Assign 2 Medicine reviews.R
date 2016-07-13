### U5 Automating Reviews in Medicine

#Load the dataset. For txt data sets add argument " stringsAsFactors=FALSE"

trials <- read.csv("clinical_trial.csv", stringsAsFactors=FALSE)

# # Run Language Settings
Sys.setlocale("LC_ALL", "C")

#check "length(stopwords("english"))" = 174. 
length(stopwords("english"))
#174


##Q1
str(trials)

# Max number of characters in abstrac
max(nchar(trials$abstract))

# Number of articles with no abbstract
table(nchar(trials$abstract))

min(nchar(trials$title))
which.min(nchar(trials$title))
#[1258]
trials$title[1258]
#"A decade of letrozole: FACE."

##Q2 Preparing Corpus

# Load tm package

library(tm)


# Create corpus
names(trials)
corpusTitle <- Corpus(VectorSource(trials$title))
corpusAbstract <- Corpus(VectorSource(trials$abstract))

# Pre-process data
corpusTitle <- tm_map(corpusTitle, tolower)
corpusTitle <- tm_map(corpusTitle, PlainTextDocument)

corpusAbstract <- tm_map(corpusAbstract, tolower)
corpusAbstract <- tm_map(corpusAbstract, PlainTextDocument)

corpusTitle <- tm_map(corpusTitle, removePunctuation)
corpusAbstract <- tm_map(corpusAbstract, removePunctuation)

corpusTitle <- tm_map(corpusTitle, removeWords, stopwords("english"))
corpusAbstract <- tm_map(corpusAbstract, removeWords, stopwords("english"))

corpusTitle <- tm_map(corpusTitle, stemDocument)
corpusAbstract <- tm_map(corpusAbstract, stemDocument)

#  Now the Added wiki in this corpus are ready for our machine learning algorithms.
# Create matrix: dtm means document-term matricies

dtmTitle <- DocumentTermMatrix(corpusTitle)
dtmAbstract <- DocumentTermMatrix(corpusAbstract)
dtmTitle
dtmAbstract

# Remove sparse terms. In this recitation remove terms that don't appear in at least 5% of the documents
dtmTitle <- removeSparseTerms(dtmTitle, 0.95)
dtmAbstract <- removeSparseTerms(dtmAbstract, 0.95)
dtmTitle
dtmAbstract
# dtmTitle terms=31
# dtmAbstract terms=335

# Create data frame
dtmTitle <- as.data.frame(as.matrix(dtmTitle))
dtmAbstract <- as.data.frame(as.matrix(dtmAbstract))

##P2.3 What is the most frequent word stem across the abstracts
which.max(colSums(dtmAbstract))


### Prob 3 Building a Model
##P3.1 Add "T" in front of Title terms and "A" in front of Abstract terms to keep separation of common terms
colnames(dtmTitle) <- paste0("T", colnames(dtmTitle))

colnames(dtmAbstract) <- paste0("A", colnames(dtmAbstract))

##P3.2 Combine data frames and add outcome variable "trial"
dtm <- cbind(dtmTitle, dtmAbstract)

# Add in the outcome variable
dtm$trial <- trials$trial

str(dtm)

##P3.3 

library(caTools)

set.seed(144)

spl <- sample.split(dtm$trial, 0.7)

train <- subset(dtm, spl == TRUE)
test <- subset(dtm, spl == FALSE)

# baseline accuracy on training data set=0.561
table(train$trial)
730/(730+572)

##Pr3.4 Build a CART model

# Build a CART model

library(rpart)
library(rpart.plot)

# Predicting "responsive" and using all the terms, classification model - therefore method = class
trialCART <- rpart(trial~., data=train, method="class")

prp(trialCART)

## P3.5 Training set predictions

# Make predictions on the test set

pred <- predict(trialCART, data=train)
pred[1:10,]

# Select predicted probabiities of a document being a trial 
pred.prob <- pred[,2]
max(pred.prob)
#max predicted probability for any result 0.872

##P3.6 (simple question)

##P3.7

# Confusion table (cut off threshold used 0.5). Compute accuracy,sensitivity and Specificity 

table(train$trial, pred.prob >= 0.5)

#    FALSE TRUE
# 0   631   99
# 1   131  441

#overall accuracy=0.823
(631+441)/nrow(train)

#Sensitivity TP/(TP+FN)=0.771
(441)/(131+441)

#Specificity TN/(TN+FP)=0.864
631/(631+99)

###P4 Evaluating model on test set

#4.1 
predTest <- predict(trialCART, newdata=test)
predTest[1:10,]


# Select predicted probabiities of an document being a trial 
pred.prob <- predTest[,2]

# Compute accuracy, cut off threshold used 0.5

table(test$trial, pred.prob >= 0.5)
#FALSE TRUE
#0   261   52
#1    83  162

#accuracy test set = 0.758
(261+162)/nrow(test)

#4.2 Evaluating the Model on the testing data set

library(ROCR)

predROCR <- prediction(pred.prob, test$trial)

# Use performance function to Extract tpr and fpr. tpr means true positive rate.fpr means false positive rate
perfROCR <- performance(predROCR, "tpr", "fpr")

plot(perfROCR, colorize=TRUE)

# refer to U5 recitation transcript notes for interpretation of threshold

# Compute AUC=0.837

performance(predROCR, "auc")@y.values


###P5 Decision-maker tradeoffs

##5.3 Selecting the threshold using ROCR

#A false negative might negatively affect the results of the literature review and analysis, 
# while a false positive is a nuisance (one additional paper that needs to be manually checked).
#As a result, the cost of a false negative is much higher than the cost of a false positive, 
#so much so that many studies actually use no machine learning (aka no Step 1) and 
#have two people manually review each search result in Step 2. 

# As always, we prefer a lower threshold in cases where false negatives are more costly 
# than false positives, since we will make fewer negative predictions.
# That is, We want to maximise sensitivity,the True Positive Rate, which on the ROCR curve
# is a smaller threshold! So read the ROCR curve focusing on increasing the True  positive rate.


