# Video 2 - Reading in the Dataset
library(lubridate)
library(lattice)
# Get the current directory
  getwd()
  setwd("~/Dropbox/MIT Analytics/Week1/An Analytical Detective")
# Read the csv file
  CrimeDataChicago = read.csv("mvtWeek1.csv")
# Structure of the dataset
  str(CrimeDataChicago)
# Statistical summary
  summary(CrimeDataChicago)

  max(CrimeDataChicago$ID)
  
  CrimeDataChicago$ID
  
  CrimeDataChicago$Date[1]
# Finding the index of the food with highest sodium levels
  which.max(CrimeDataChicago$ID)
# Converting Date
  DateConvert = as.Date(strptime(CrimeDataChicago$Date, "%m/%d/%y %H:%M"))
  summary(DateConvert)
  
  CrimeDataChicago$Month = as.numeric(months(DateConvert))
  
  CrimeDataChicago$Weekday = as.numeric(weekdays(DateConvert))
  
  CrimeDataChicago$year = as.numeric(strftime(DateConvert, "%Y"))
  
  CrimeDataChicago$Date = DateConvert

  CrimeDataChicago$Narrest = ifelse(CrimeDataChicago$Arrest == FALSE,  1, 0) 
  
  which.min(table(CrimeDataChicago$Month))
  
  which.max(table(CrimeDataChicago$Weekday))
  
  
  temp=table(CrimeDataChicago$Month, CrimeDataChicago$Arrest)
  
  as.data.frame(temp)
  
  which.max(table(temp$Weekday))
  
  tapply(CrimeDataChicago$Month, CrimeDataChicago$Arrest, max)
  tapply(CrimeDataChicago$Month, CrimeDataChicago$Arrest, max)
  
  tapply(CrimeDataChicago$Arrest, CrimeDataChicago$Month, max)
  
  hist(CrimeDataChicago$Date, breaks=100)

  jpeg('rplot.jpg')
  dev.off()
  
  
  CrimeDataChicago$ArrestN = as.numeric(CrimeDataChicago$Arrest)
  
  # Boxplots
  
  tapply(CrimeDataChicago$Date, CrimeDataChicago$ArrestN, max)
  
  boxplot(year~ArrestN, data=CrimeDataChicago, main="Chicago Crime Data", xlab="Arrest", ylab="Date")
  
  hist(CrimeDataChicago$Date, breaks=100)

#For what proportion of motor vehicle thefts in 2001 was an arrest made?


  
aggregate(ArrestN~ year , CrimeDataChicago, sum)/(aggregate(ArrestN~ year , CrimeDataChicago, sum)+aggregate(Narrest~ year , CrimeDataChicago, sum))

Consolidated= aggregate(ArrestN~ year , CrimeDataChicago, sum)

NotArrest = aggregate(Narrest~ year , CrimeDataChicago, sum)

Consolidated$Narrest = NotArrest$Narrest

Consolidated$Total =  NotArrest$Narrest + ArrestDone$ArrestN

Consolidated$ArrestToTotal=  ArrestDone$ArrestN / (NotArrest$Narrest + ArrestDone$ArrestN)

Consolidated


CrimeDataChicago$ArrestToCrime = (CrimeDataChicago$ArrestN/(CrimeDataChicago$Narrest+CrimeDataChicago$ArrestN))

table(CrimeDataChicago$year, CrimeDataChicago$ArrestToCrime)


#Which locations are the top five locations for motor vehicle thefts, excluding the "Other" category?

sort(table(CrimeDataChicago$LocationDescription))

#How many observations are in Top5?

Top5 = subset(CrimeDataChicago, LocationDescription=="STREET" | LocationDescription=="PARKING LOT/GARAGE(NON.RESID.)" | LocationDescription=="ALLEY" | LocationDescription=="GAS STATION" | LocationDescription=="DRIVEWAY - RESIDENTIAL")

TopLocations = c("STREET", "PARKING LOT/GARAGE(NON.RESID.)", "ALLEY", "GAS STATION", "DRIVEWAY - RESIDENTIAL")

Top5 = subset(CrimeDataChicago, LocationDescription %in% TopLocations)

Top5$LocationDescription


#One of the locations has a much higher arrest rate than the other locations. Which is it?

Top5$LocationDescription = factor(Top5$LocationDescription)
table(Top5$LocationDescription, Top5$Arrest)

#On which day of the week do the fewest motor vehicle thefts in residential driveways happen?

table(Top5$LocationDescription, Top5$Weekday)

str(Top5)
 
