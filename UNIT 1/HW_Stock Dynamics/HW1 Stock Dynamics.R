

rm(list=ls())
setwd("~/R/1/Stock Dynamics")


library("utils")
library("tidyverse")
library(stringr)


# read
IBM <- read.csv("IBMStock.csv")
GE <- read.csv("GEStock.csv")
ProcterGamble <- read.csv("ProcterGambleStock.csv")
CocaCola <- read.csv("CocaColaStock.csv")
Boeing <- read.csv("BoeingStock.csv")


# formulate date format
IBM$Date = as.Date(IBM$Date, "%m/%d/%y")
GE$Date = as.Date(GE$Date, "%m/%d/%y")
CocaCola$Date = as.Date(CocaCola$Date, "%m/%d/%y")
ProcterGamble$Date = as.Date(ProcterGamble$Date, "%m/%d/%y")
Boeing$Date = as.Date(Boeing$Date, "%m/%d/%y")


# 1.2 
# 1.3
min(IBM$Date)
max(IBM$Date)


summary(IBM)
summary(GE)
summary(CocaCola)
summary(Boeing)

# 1.8
sd(ProcterGamble$StockPrice)

# 2.1 
plot(CocaCola$Date, CocaCola$StockPrice, type = "l", col = "red")

# 2.2
lines(ProcterGamble$Date, ProcterGamble$StockPrice, col = "blue")
lines(ProcterGamble$Date, ProcterGamble$StockPrice, col = "blue", lty = 2) # dash line 

abline(v=as.Date(c("2000-03-01")), lwd=2)

# 2.3
abline(v=as.Date(c("1983-03-01")), lwd=1)

filter(CocaCola, Date == c("1983-03-01")) 
filter(ProcterGamble, Date == c("1983-03-01")) 

# 3.1
# 3.2
plot(CocaCola$Date[301:432], CocaCola$StockPrice[301:432], type="l", col="red", ylim=c(0,210))

abline(v=as.Date(c("2000-03-01")), lwd=1)

lines(ProcterGamble$Date, ProcterGamble$StockPrice)
lines(IBM$Date, IBM$StockPrice)
lines(GE$Date, GE$StockPrice)
lines(Boeing$Date, Boeing$StockPrice)

# 3.3
abline(v=as.Date(c("1997-09-01")), lwd=1)
abline(v=as.Date(c("1997-11-01")), lwd=1)

# 4.1 Apply a Function Over a Ragged Array

mean(IBM$StockPrice) # all average

index = months(IBM$Date) # extract month as index
avg = tapply(IBM$StockPrice, index, mean) # group by month

ind = order(avg) # order index
avg[ind] # sort by index


# 4.2 GE

mean(GE$StockPrice) # all average

index = months(GE$Date) # extract month as index
avg = tapply(GE$StockPrice, index, mean) # group by month

ind = order(avg) # order index
avg[ind] # sort by index

# 4.2 coca

mean(CocaCola$StockPrice) # all average

index = months(CocaCola$Date) # extract month as index
avg = tapply(CocaCola$StockPrice, index, mean) # group by month

ind = order(avg) # order index
avg[ind] # sort by index




