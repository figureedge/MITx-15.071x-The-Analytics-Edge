
rm(list=ls())
setwd("~/R/1/An Analytical Detective")


library("utils")
library("tidyverse")
library(stringr)


mvt <- read.csv("mvtWeek1.csv")

# 1.1 
dim(mvt)
head(mvt)

# 1.3
max(mvt$ID)

# 1.4
min(mvt$Beat)

# 1.5
summary(mvt$Arrest)

# 1.6 filter specific content in var 
f1 = filter(mvt, LocationDescription == "ALLEY")
summary(f1)

summary(mvt)

f2 = table(mvt$LocationDescription) # create table handle to count # of unique 

unique(mvt$LocationDescription)
n_distinct(mvt$LocationDescription)

# 2.2 convert to actual date format

DateConvert = as.Date(strptime(mvt$Date, "%m/%d/%y %H:%M"))

# 2.3 convert date format 

mvt$Month = months(DateConvert)
mvt$Weekday = weekdays(DateConvert)

mvt$Date = DateConvert

head(mvt)

table(mvt$Month)

# 2.4
table(mvt$Weekday)

# 2.5
t1 = table(mvt$Month, mvt$Arrest)
max(t1[,2]) # max in table int juts like array 

# 3.1
hist(mvt$Date, breaks=100)

# convert table to data frame 
d1 <- as.data.frame.matrix(t1)
order(d1$"FALSE") # descent order index

d1[order(d1$"FALSE"),] # sort the data frame by assign index 

# convert logic T F to 1 0
?lapply
?sapply

mvt$Arrest01 <- sapply(mvt$Arrest, as.numeric)
head(mvt)

arrest.0 = filter(mvt, mvt$Arrest01 == 0)
arrest.1 = filter(mvt, mvt$Arrest01 == 1)


# 3.2
boxplot(mvt$Date ~ mvt$Arrest)

h1 = filter(mvt, mvt$Date <= as.Date("2007-01-01"))
h2 = filter(mvt, mvt$Date >= as.Date("2007-01-01"))

table(h1$Arrest)
table(h2$Arrest)

# 3.3
h3 = filter(mvt, mvt$Date >= as.Date("2001-01-01") & mvt$Date <= as.Date("2001-12-31"))
t.2001 = table(h3$Arrest)
t.2001[2]/sum(t.2001)


table(mvt$Arrest, mvt$Year) # another way 

# 3.4

h4 = filter(mvt, mvt$Date >= as.Date("2007-01-01") & mvt$Date <= as.Date("2007-12-31"))
t.2007 = table(h4$Arrest)
t.2007[2]/sum(t.2007)

# 3.5

550/(550+13542)

# 4.1

head(mvt)

table(mvt$LocationDescription,mvt$Arrest)

sort(table(mvt$LocationDescription))

# 4.2

f1 = filter(mvt, mvt$LocationDescription == "STREET")
f2 = filter(mvt, mvt$LocationDescription == "PARKING LOT/GARAGE(NON.RESID.)")
f3 = filter(mvt, mvt$LocationDescription == "ALLEY")
f4 = filter(mvt, mvt$LocationDescription == "GAS STATION")
f5 = filter(mvt, mvt$LocationDescription == "DRIVEWAY - RESIDENTIAL")

ff = rbind(f1,f2,f3,f4,f5)
head(ff)

# another way
TopLocations = c("STREET", "PARKING LOT/GARAGE(NON.RESID.)", "ALLEY", "GAS STATION", "DRIVEWAY - RESIDENTIAL")
Top5 = filter(mvt, LocationDescription %in% TopLocations)

# same as above
Top5 = subset(mvt, LocationDescription %in% TopLocations)


# 4.3 
# factor - hide others 
# select - only has specified columns 

Top5$LocationDescription = factor(Top5$LocationDescription)
table(Top5$LocationDescription)

TT = table(Top5$LocationDescription, Top5$Arrest)
Top5.data = as.data.frame.matrix(TT)

Top5.data$arrestio <- Top5.data$"TRUE" / (Top5.data$"FALSE" + Top5.data$"TRUE")
Top5.data

# 4.4
head(mvt)

gas = filter(mvt, mvt$LocationDescription == "GAS STATION")
head(gas)

table(gas$Weekday)

# 4.5

res = filter(mvt, mvt$LocationDescription == "DRIVEWAY - RESIDENTIAL")
table(res$Weekday)
