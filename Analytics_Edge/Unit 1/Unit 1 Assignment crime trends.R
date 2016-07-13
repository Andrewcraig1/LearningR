str(mvt)

max(mvt$ID)

min(mvt$Beat)

summary(mvt$Arrest)

# How many observations have a LocationDescription value of ALLEY?
Alley<- subset(mvt, LocationDescription == "ALLEY")
nrow(Alley)


##Prob 2.1 Understanding dates in R

# Converting dates - First note the sequence (m,d,y or d,m,y. Then note adding / between m d y, or d,m,y
DateConvert<-as.Date(strptime(mvt$Date,"%m/%d/%y %H:%M"))
str(DateConvert)
summary(DateConvert)

mvt$Month<-months(DateConvert)
mvt$Weekday<-weekdays(DateConvert)
mvt$Date<-DateConvert

table(mvt$Month)
table(mvt$Weekday)

table(mvt$Month, mvt$Arrest)

# Problem 3 Visualising Crime Trends

hist(mvt$Date, breaks=100)

# 3.2 Create box plotbof the variable Date, sorted by the variable "Arrest"
DateArrest<-subset(mvt, Arrest==TRUE)
str(DateArrest)
boxplot(DateArrest$Date)

#3.3
table(mvt$Year,mvt$Arrest)

2152/(18517+2152)

#3.4
1212/(13068+1212)

#3.5
550/(13542+550)

#4.1 Sort table - cool
sort(table(mvt$LocationDescription))

#4.2 
Top5.1<-subset(mvt, LocationDescription == "STREET")
Top5.2<-subset(mvt, LocationDescription == "ALLEY")
Top5.3<-subset(mvt, LocationDescription == "GAS STATION")
Top5.4<-subset(mvt, LocationDescription == "DRIVEWAY - RESIDENTIAL")
Top5.5<-subset(mvt, LocationDescription == "PARKING LOT/GARAGE(NON.RESID.)")
Top5<-rbind(Top5.1,Top5.2,Top5.3,Top5.4,Top5.5)
str(Top5)

Top5$LocationDescription<-factor(Top5$LocationDescription)
str(Top5)
table(Top5$LocationDescription,Top5$Arrest)

#arrest rate calcs
249/(2059+249)
132/(1543+132)
439/(1672+439)
1603/(13249+   1603)
11595/(144969 + 11595)

#4.4 Top5.3 is Gas Stations data.file
table(Top5.3$Weekday)

#4.5
table(Top5.4$Weekday)

