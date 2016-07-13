## Convert Dates into R Format
IBM$Date<-as.Date(IBM$Date,"%m/%d/%y")
GE$Date<-as.Date(GE$Date,"%m/%d/%y")
CocaCola$Date<-as.Date(CocaCola$Date,"%m/%d/%y")
ProcterGamble$Date<-as.Date(ProcterGamble$Date,"%m/%d/%y")
Boeing$Date<-as.Date(Boeing$Date,"%m/%d/%y")

str(GE)

min(GE$Date)
max(GE$Date)

mean(IBM$StockPrice)

min(GE$StockPrice)

max(CocaCola$StockPrice)

median(Boeing$StockPrice)

sd(ProcterGamble$StockPrice)

## P2 Stock Price Coca-Cola and Procter and Gamble
# Adding type="l" converts observation points into a line
plot(CocaCola$Date, CocaCola$StockPrice, xlab="Date", ylab = "Stock Price", main = "Stock Price:Coca-Cola(r) Procter Gamble(bl)", col="red", type="l")
# Keeping the plot open, we can now add additional lines. lty=2 argument will change the line to a dashed line.
lines(ProcterGamble$Date,ProcterGamble$StockPrice, col="blue", lty=2)
# To draw a vertical line at a selected date type the following commmand:
abline(v=as.Date(c("2000-03-01")),lwd=2)
abline(v=as.Date(c("1983-01-01")),lwd=2)

##P3 Plot selected observations: Stock Dynamics 1995-2005
plot(CocaCola$Date[301:432],CocaCola$StockPrice[301:432],type="l",col="red",ylim = c(0,210), 
     main="Stock Price Dynamics 1995-2005: CC(r),PG(bl),Bo(g),GE(bl),IBM(pu)")
lines(ProcterGamble$Date[301:432],ProcterGamble$StockPrice[301:432], col="blue", lty=2)
lines(Boeing$Date[301:432],Boeing$StockPrice[301:432], col="green")
lines(GE$Date[301:432],GE$StockPrice[301:432], col="black")
lines(IBM$Date[301:432],IBM$StockPrice[301:432], col="purple")

abline(v=as.Date(c("1997-09-01")),lwd=2)
abline(v=as.Date(c("1997-11-01")),lwd=2)

abline(v=as.Date(c("2004-01-01")),lwd=2)
abline(v=as.Date(c("2005-12-31")),lwd=2)

## P4 Monthly Trends: Note - second argument sorts analysis into months
tapply(IBM$StockPrice, months(IBM$Date), summary, na.rm=TRUE)
mean(IBM$StockPrice)

tapply(CocaCola$StockPrice, months(CocaCola$Date), summary, na.rm=TRUE)
mean(CocaCola$StockPrice)

tapply(ProcterGamble$StockPrice, months(ProcterGamble$Date), summary, na.rm=TRUE)
mean(ProcterGamble$StockPrice)

tapply(Boeing$StockPrice, months(Boeing$Date), summary, na.rm=TRUE)
mean(Boeing$StockPrice)

tapply(GE$StockPrice, months(GE$Date), summary, na.rm=TRUE)
mean(GE$StockPrice)




