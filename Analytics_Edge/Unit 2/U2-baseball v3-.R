### Baseball

str(baseball)


##subset data - moneyball

moneyball<-subset(baseball,Year<2002)
str(moneyball)

#create new variable runs difference (runs sccored - runs allowed)
moneyball$RD<-moneyball$RS-moneyball$RA
str(moneyball)

# Predicting Wins and RD. Check relationship between variables RD and Wins"W"
plot(moneyball$RD,moneyball$W)

WinsReg<-lm(W~RD,data = moneyball)
summary(WinsReg)

80.8814+(0.1058*99)

# Video 3 Predicting Runs from on base percentage (OBP), slugging average (SLG) and batting average(BA)
str(moneyball)

RunsReg<-lm(RS~OBP+SLG, data = moneyball)
summary(RunsReg)

# Calc Runs Allowed 
# Runs Allowed =-804.63+2737.77(OBP)+1584.91(SLG)
-804.63+(2737.77*0.311)+(1584.91*0.405)

# Calc Runs Allowed for opposition
#Runs Allowed Opposition = -837.38+2913.60(OOBP)+1514.29(OSLG)
-837.38+(2913.60*0.297)+(1514.29*0.370)


# Video 4 Predicting Runs Scored
# EC $1.4M
-804.63+(2737.77*0.338)+(1584.91*0.540)
# JG  $1.065M
-804.63+(2737.77*0.391)+(1584.91*0.450)
# FM  $295,000
-804.63+(2737.77*0.369)+(1584.91*0.374)
# GM  $800,000
-804.63+(2737.77*0.313)+(1584.91*0.447)
# CP  $300,000
-804.63+(2737.77*0.361)+(1584.91*0.500)

#quick Q
teamRank<-c(1,2,3,3,4,4,4,4,5,5)
wins2012<-c(94,88,95,88,93,94,98,97,93,94)
wins2013<-c(97,97,92,93,92,96,94,96,92,90)

cor(teamRank,wins2013)

