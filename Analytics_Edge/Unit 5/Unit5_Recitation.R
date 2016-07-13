# Unit 5 - Recitation


# Video 2

# Load the dataset. Fot txt data sets add argument " stringsAsFactors=FALSE"

emails <- read.csv("energy_bids.csv", stringsAsFactors=FALSE)

str(emails)

# # Run Language Settings
Sys.setlocale("LC_ALL", "C")

# Look at emails

emails$email[1]
# alternative argument to wrap so its easier to read
strwrap(emails$email[1])
emails$responsive[1]
 
strwrap(emails$email[2])
emails$responsive[2]
#[1] means its response to the inquiry on fraud

# Responsive emails

table(emails$responsive)
# This table identifies the high number of nonresponsive emails (716) to our inquiry
# 0   1 
# 716 139 


# Video 3


# Load tm package

library(tm)


# Create corpus

corpus <- Corpus(VectorSource(emails$email))

corpus[[1]]
strwrap(corpus[1])


# Pre-process data
corpus <- tm_map(corpus, tolower)
corpus <- tm_map(corpus, PlainTextDocument)


corpus <- tm_map(corpus, removePunctuation)

corpus <- tm_map(corpus, removeWords, stopwords("english"))

corpus <- tm_map(corpus, stemDocument)

# Look at first email
corpus[[1]]
strwrap(corpus[1])

#  Now the emails in this corpus are ready for our machine learning algorithms.

# Video 4

# Create matrix

dtm <- DocumentTermMatrix(corpus)
dtm
# no of terms=22167

# Remove sparse terms. In this recitation remove terms that don't appear in at least 3% of the documents
dtm <- removeSparseTerms(dtm, 0.97)
dtm
# Now no of terms=788

# Create data frame
labeledTerms <- as.data.frame(as.matrix(dtm))

# Add in the outcome variable
labeledTerms$responsive <- emails$responsive

str(labeledTerms)

# Find the last variable "responsive"
tail(labeledTerms$responsive)

# Video 5


# Split the data

library(caTools)

set.seed(144)

spl <- sample.split(labeledTerms$responsive, 0.7)

train <- subset(labeledTerms, spl == TRUE)
test <- subset(labeledTerms, spl == FALSE)

# Build a CART model

library(rpart)
library(rpart.plot)

# Predicting "responsive" and using all the terms, classification model - therefore method = class
emailCART <- rpart(responsive~., data=train, method="class")

prp(emailCART)



# Video 6

# Make predictions on the test set

pred <- predict(emailCART, newdata=test)
pred[1:10,]

#pred[1:10,]
# So to recall the structure of pred,
#we can look at the first 10 rows with pred[1:10,].
#So this is the rows we want.We want all the columns.
#So we'll just leave a comma and nothing else afterward.
#So the left column here is the predicted probability
#of the document being non-responsive.
#And the right column is the predicted probability
#of the document being responsive.They sum to 1.
#So in our case, we want to extract
#the predicted probability of the document being responsive.
#So we're looking for the rightmost column.
#                0          1
#character(0)   0.2156863 0.78431373
#character(0).1 0.9557522 0.04424779
#character(0).2 0.9557522 0.04424779
#character(0).3 0.8125000 0.18750000
#character(0).4 0.4000000 0.60000000
#character(0).5 0.9557522 0.04424779
#character(0).6 0.9557522 0.04424779
#character(0).7 0.9557522 0.04424779
#character(0).8 0.1250000 0.87500000
#character(0).9 0.1250000 0.87500000

# Select predicted probabiities of an email being responsive 
pred.prob <- pred[,2]

# Compute accuracy, cut off threshold used 0.5

table(test$responsive, pred.prob >= 0.5)
#  FALSE TRUE
# 0   195   20
# 1    17   25

(195+25)/(195+25+17+20)
#0.856

# Baseline model accuracy

table(test$responsive)
#   0   1 
#  215  42 
215/(215+42)
# .837


# Video 7

# ROC curve

library(ROCR)



predROCR <- prediction(pred.prob, test$responsive)

# Use performance function to Extract tpr and fpr. tpr means true positive rate.fpr means false positive rate
perfROCR <- performance(predROCR, "tpr", "fpr")

plot(perfROCR, colorize=TRUE)

# refer to U5 recitation transcript notes for interpretation of threshold

# Compute AUC

performance(predROCR, "auc")@y.values

#  0.7936
