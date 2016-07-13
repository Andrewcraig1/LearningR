### Basic analysis of Data in data frame.
## WHO data frame
str(WHO)
WHO$Under15
mean(WHO$Under15)
sd(WHO$Under15)
summary(WHO$Under15)
which.min(WHO$Under15)
#The above gives the line number. In this case it is 86.
WHO$Country[86]
which.max(WHO$Under15)
#The above gives the line number. in this case it is 124.
WHO$Country[124]

##Graphs: Plot, Histogram and Boxplot
plot(WHO$GNI,WHO$FertilityRate)

#Outliers subset
Outliers<-subset(WHO, GNI>10000 & FertilityRate>2.5)
nrow(Outliers)
#Select variables within subset
Outliers[c("Country","GNI", "FertilityRate")]


hist(WHO$CellularSubscribers)

boxplot(WHO$LifeExpectancy~WHO$Region, xlab="",ylab="Life Expectancy",main="Life Expectancy of Countries by Region")

##Summary Tables
# Table counts values under variables and works well for variables with only a few possible values.
table(WHO$Region)

#tapply function: Creates table, but the argument included creates a summary statistic etc
tapply(WHO$Over60, WHO$Region, mean)
# In this example a further argument is included to remove NA cases from the anaysis that have missing values.
tapply(WHO$LiteracyRate, WHO$Region, min, na.rm=TRUE)

##Test
mean(WHO$Over60)
which.min(WHO$Over60)
WHO$Country[183]
which.max(WHO$LiteracyRate)
WHO$Country[44]

##Test
tapply(WHO$ChildMortality, WHO$Region, mean)
