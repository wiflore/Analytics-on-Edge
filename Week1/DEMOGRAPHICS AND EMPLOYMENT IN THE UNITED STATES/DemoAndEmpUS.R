# Get the current directory
getwd()
setwd("~/Dropbox/MIT Analytics/Week1/DEMOGRAPHICS AND EMPLOYMENT IN THE UNITED STATES")
# Read the csv file
CPS = read.csv("CPSdata.csv")
MetroAreaCodes = read.csv("MetroAreaCodes.csv")
CountryCodes=read.csv("CountryCodes.csv")


# Structure of the dataset
str(CPS)
str(MetroAreaCodes)
str(CountryCodes)

# Statistical summary
summary(CPS)

# Sort by State

sort(table(CPS$State)) 

# Table with Citizenship
table(CPS$Citizenship)

table(CPS$Race, CPS$Hispanic)

#Evaluating Missing Values - How many are missing or State rural o urban

table(CPS$Region, is.na(CPS$Married))

table(CPS$Sex, is.na(CPS$Married))

table(CPS$Age, is.na(CPS$Married))

table(CPS$Citizenship, is.na(CPS$Married))

table(CPS$Citizenship, is.na(CPS$Married))

table(CPS$State, is.na(CPS$MetroAreaCode))

#Taaply proportions TRUE n FALSE 


tapply(is.na(CPS$MetroAreaCode), CPS$State , mean)
sort(tapply(is.na(CPS$MetroAreaCode), CPS$State , mean))

#working with more sources

str(MetroAreaCodes)
str(CountryCodes)

#merging data
CPS = merge(CPS, MetroAreaCodes, by.x="MetroAreaCode", by.y="Code", all.x=TRUE)
str(CPS)

summary(CPS)

sort(table(CPS$MetroArea))

#tapply for proportions

sort(tapply(CPS$Hispanic, CPS$MetroArea, mean))

sort(tapply(CPS$Race == "Asian", CPS$MetroArea, mean))

#Excluding NA data in Education
sort(tapply(CPS$Education == "No high school diploma", CPS$MetroArea, mean, na.rm=TRUE))

#mergering 2
CPS = merge(CPS, CountryCodes, by.x="CountryOfBirthCode", by.y="Code", all.x=TRUE)

table(CPS$MetroArea == "New York-Northern New Jersey-Long Island, NY-NJ-PA", CPS$Country != "United States")
str(CPS)

summary(CPS)\

sort(tapply(CPS$Country == "India", CPS$MetroArea, sum, na.rm=TRUE))

sort(tapply(CPS$Country == "Brazil", CPS$MetroArea, sum, na.rm=TRUE))

sort(tapply(CPS$Country == "Somalia", CPS$MetroArea, sum, na.rm=TRUE))
