# VIDEO 2

# Read in data
baseball = read.csv("baseball.csv")
str(baseball)
head(baseball)

# Subset to only include moneyball years
moneyball = subset(baseball, Year < 2002)
str(moneyball)
head(moneyball)

# Compute Run Difference
moneyball$RD = moneyball$RS - moneyball$RA
str(moneyball)


# Scatterplot to check for linear relationship
plot(moneyball$RD, moneyball$W)

# Regression model to predict wins
WinsReg = lm(W ~ RD, data=moneyball)
summary(WinsReg)
coef(WinsReg)


# VIDEO 3

str(moneyball)

# Regression model to predict runs scored
RunsReg = lm(RS ~ OBP + SLG + BA, data=moneyball)
summary(RunsReg)

RunsReg = lm(RS ~ OBP + SLG, data=moneyball)
summary(RunsReg)

RunsReg = lm(RS ~ OBP + BA, data=moneyball)
summary(RunsReg)

RunsReg = lm(RS ~ SLG + BA, data=moneyball)
summary(RunsReg)

# another vars 
head(moneyball)

RunsReg = lm(RS ~ OBP + SLG, data=moneyball)
summary(RunsReg)
coef(RunsReg)[1] + coef(RunsReg)[2]*0.311 + coef(RunsReg)[3]*0.405


# playoffs 
head(baseball)
playoffs = subset(baseball, Year == 2002 | Year == 2003)
playoffs


# corr

teamRank = c(1,2,3,3,4,4,4,4,5,5)
wins2012 = c(94,88,95,88,93,94,98,97,93,94)
wins2013 = c(97,97,92,93,92,96,94,96,92,90)

cor(teamRank,wins2012)
cor(teamRank,wins2013)






