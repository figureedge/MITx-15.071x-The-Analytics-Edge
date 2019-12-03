


rm(list=ls())


library("utils")
library("tidyverse")
library(stringr)

setwd("~/R/The Analytics Edge/Demographics")

CPS <- read.csv("CPSData.csv")
head(CPS)


# 1.1
dim(CPS)

# 1.2
t1 = table(CPS$Industry)

ind = order(t1) # order index
t1[ind] # sort by index

# same as sort 
sort(t1)

# 1.3
sort(table(CPS$Region)) 

sort(table(CPS$State))

# 1.4
t2 = sort(table(CPS$Citizenship))
1-t2[2]/sum(t2)

# 1.5 filter function need package 

f1 = filter(CPS, CPS$Hispanic == 1)

head(f1)
table(f1$Race)

table(CPS$Hispanic, CPS$Race)

# 2.1

any(is.na(CPS$PeopleInHousehold)) # check one var

sapply(CPS, function(x) any(is.na(x))) # check all var

summary(CPS) # summary also tell you is there any na 

# 2.2

table(CPS$Region, is.na(CPS$Married))
table(CPS$Sex, is.na(CPS$Married))
table(CPS$Age,is.na(CPS$Married))
table(CPS$Citizenship, is.na(CPS$Married))

# 2.3


t2 = table(CPS$State, is.na(CPS$MetroAreaCode))
d2 = as.data.frame.matrix(t2)

head(d2)

filter(d2, d2$"FALSE" == 0) # no missing value = all in city
filter(d2, d2$"TRUE" == 0) # no metro cdoe = all in non-city


# 2.4

t3 = table(CPS$Region, is.na(CPS$MetroAreaCode))
d3 = as.data.frame.matrix(t3)

head(d3)

d3$ratio = d3$"TRUE"/(d3$"FALSE" + d3$"TRUE")
d3

# 2.5

d2
d2[order(d2$ratio),] 

# another way
tapply(is.na(CPS$MetroAreaCode), CPS$State, mean)
sort(tapply(is.na(CPS$MetroAreaCode), CPS$State, mean))

?tapply

# 3.1

MetroAreaCode <- read.csv("MetroAreaCodes.csv")
MetroAreaMap <- read.csv("CountryCodes.csv")


head(MetroAreaCode)
head(MetroAreaMap)

dim(MetroAreaCode)
ncol()
nrow()

# 3.2

CPS = merge(CPS, MetroAreaMap, by.x="MetroAreaCode", by.y="Code", all.x=TRUE) # keep na
head(CPS)

# 3.3

head(CPS)

table(CPS$Region,CPS$MetroAreaCode)
