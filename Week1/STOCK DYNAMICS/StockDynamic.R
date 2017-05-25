# Get the current directory
getwd()
setwd("~/Dropbox/MIT Analytics/Week1/STOCK DYNAMICS")
# Read the csv file
IBM = read.csv("IBMStock.csv")
GE = read.csv("GEStock.csv")
CocaCola = read.csv("CocaColaStock.csv")
ProcterGamble = read.csv("ProcterGambleStock.csv")
Boeing = read.csv("BoeingStock.csv")

#convert the dates into a format that R can understand.

IBM$Date = as.Date(IBM$Date, "%m/%d/%y")
GE$Date = as.Date(GE$Date, "%m/%d/%y")
CocaCola$Date = as.Date(CocaCola$Date, "%m/%d/%y")
ProcterGamble$Date = as.Date(ProcterGamble$Date, "%m/%d/%y")
Boeing$Date = as.Date(Boeing$Date, "%m/%d/%y")




# Structure of the dataset
str(CrimeDataChicago)
# Statistical summary
summary(IBM)
# SD
sd(ProcterGamble$StockPrice)

#plot

plot(CocaCola$Date, CocaCola$StockPrice, type="l")

plot(CocaCola$Date, CocaCola$StockPrice, type="l", col="red")

lines(ProcterGamble$Date, ProcterGamble$StockPrice, col="blue", lty=2)

abline(v=as.Date(c("2000-03-01")), lwd=2)

#Let's take a look at how the stock prices changed from 1995-2005 for all five companies

plot(CocaCola$Date[301:432], CocaCola$StockPrice[301:432], type="l", col="red", ylim=c(0,210))

lines(ProcterGamble$Date[301:432], ProcterGamble$StockPrice[301:432], col="blue")

lines(IBM$Date[301:432], IBM$StockPrice[301:432], col="green")

lines(GE$Date[301:432], GE$StockPrice[301:432], col="purple")

lines(Boeing$Date[301:432], Boeing$StockPrice[301:432], col="orange")

abline(v=as.Date(c("2000-03-01")), lwd=2)

abline(v=as.Date(c("1997-09-01")), lwd=2)

abline(v=as.Date(c("1997-11-01")), lwd=2)

mean(IBM$StockPrice)


# Monthly Trends

tapply(IBM$StockPrice, months(IBM$Date), mean)

tapply(GE$StockPrice, months(GE$Date), mean)

tapply(CocaCola$StockPrice, months(CocaCola$Date), mean)
