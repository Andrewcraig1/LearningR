### Assign 1.3: Demographics & Employment Data Analysis

str(CPS)

# Summarising Data set with sort(table())
sort(table(CPS$Industry))

sort(table(CPS$State))

sort(table(CPS$Citizenship))
(116639+7073)  /131302

table(CPS$Hispanic,CPS$Race)


## Missing Values - use summary
summary(CPS)

## Patterns in missing variables> Is the reported variable missing - table variable with NA against one with no NA
TRUE means missing
table(CPS$Region, is.na(CPS$Married))


table(CPS$Sex, is.na(CPS$Married))
table(CPS$Age, is.na(CPS$Married)) 
table(CPS$Citizenship, is.na(CPS$Married))

table(CPS$State, is.na(CPS$MetroAreaCode))

table(CPS$Region,is.na(CPS$MetroAreaCode))

10674/(20010 +10674)
5609/(20330 + 5609)
9871/(31631 + 9871)
8084/(25093 + 8084)

## Trick: Calculating TRUE FALSE Proportions automatically using means. 
# Proportion of True (i.e. in this case live non-metro)
# Use sort to pareto the data
sort(tapply(is.na(CPS$MetroAreaCode), CPS$State,mean))

##Integrating Metro Area Data
str(MetroAreaMap)
str(CountryMap)

CPS<-merge(CPS,MetroAreaMap,by.x="MetroAreaCode",by.y = "Code",all.x = TRUE)

summary(CPS$MetroArea)

table(CPS$MetroArea)

#3.4 Highest proportion of Hispanic in metro area
sort(tapply(CPS$Hispanic, CPS$MetroArea,mean))

#3.5 creating TRUE FALSE numeric to generate proportions
sort(tapply(CPS$Race=="Asian", CPS$MetroArea,mean))
hist(tapply(CPS$Race=="Asian", CPS$MetroArea,mean))

#3.6 
sort(tapply(CPS$Education=="No high school diploma", CPS$MetroArea,mean, na.rm=TRUE))


# 4 Merge (integrate) data - note link of original  name in CPS "CountryOfBirthCode"

CPS<-merge(CPS,CountryMap,by.x="CountryOfBirthCode",by.y = "Code",all.x = TRUE)

str(CPS)
summary(CPS$Country)
sort(table(CPS$Country))

#4.3 Proportion Here use summary from tapply and table to calc total n for NY-NJ-PA
tapply(CPS$MetroArea=="New York-Northern New Jersey-Long Island, NY-NJ-PA",CPS$Country,summary,na.rm=TRUE)

table(CPS$MetroArea=="New York-Northern New Jersey-Long Island, NY-NJ-PA")

1-(3736/5409)

#4.4 Count - Derive from results using summary
tapply(CPS$Country=="Somalia",CPS$MetroArea,summary,na.rm=TRUE)




