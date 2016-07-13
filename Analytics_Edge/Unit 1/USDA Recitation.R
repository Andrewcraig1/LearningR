### USDA Data
str(USDA)
summary(USDA)

#Which food associated with the max Ns figure seen  from the summary data.
USDA$Sodium
which.max(USDA$Sodium)
names(USDA)
USDA$Description[265]

HighSodium<- subset(USDA, Sodium>10000)
nrow(HighSodium)
HighSodium$Description

#Find caviar and what is its sodium level
USDA$Sodium[match("CAVIAR", USDA$Description)]

#Summary ofsodium data
summary(USDA$Sodium)
sd(USDA$Sodium, na.rm=TRUE)

# Visualisation - Creating Plots
plot(USDA$Protein, USDA$TotalFat, xlab="Protein", ylab="Fat", main = "Protein vs Fat", col="red")

# Creating histogram specifiying range, and breaks (x  breadth of the histogram steps)
hist(USDA$VitaminC, xlab="Vitamin C", main="Histogram of Vitamen C Levels",xlim =c(0,100),breaks=2000)

#box plots
boxplot(USDA$Sugar, main="Boxplot of Sugar Levels", ylab="Sugar(g)")

## Video 5: Add and construct new variables to data set
# Establish two groups those with sodium above (1) and below average(0)
HighSodium<-USDA$Sodium>mean(USDA$Sodium,na.rm=TRUE)
> str((HighSodium))
# change Logic (TRUE FALSE) to a numeric with the as.numeric function, and add to USDA data frame
USDA$HighSodium<-as.numeric(USDA$Sodium>mean(USDA$Sodium,na.rm=TRUE))
str(USDA)

USDA$HighProtein<-as.numeric(USDA$Protein>mean(USDA$Protein,na.rm=TRUE))
str(USDA)

USDA$HighCarbohydrate<-as.numeric(USDA$Carbohydrate>mean(USDA$Carbohydrate,na.rm=TRUE))
str(USDA)

## Video 6 Explore relationships with Summary Data
# High sodium and High Fat (with two variables this creates a 2x2 table - very nice!)
table(USDA$HighSodium,USDA$HighFat)

# tapply function: Compute average iron in foods with high protein

tapply(USDA$Iron,USDA$HighProtein, mean, na.rm=TRUE)

tapply(USDA$VitaminC,USDA$HighCarbohydrate, max, na.rm=TRUE)

tapply(USDA$VitaminC,USDA$HighCarbohydrate, summary, na.rm=TRUE)
