
FluTrain = read.csv("FluTrain.csv")
head(FluTrain)
tail(FluTrain)

FluTest = read.csv("FluTest.csv")
head(FluTest)
tail(FluTest)

# 
hist(FluTrain$ILI)

# Question ID: 30102
plot(FluTrain$Queries, FluTrain$ILI)
plot(FluTrain$Queries, log(FluTrain$ILI))

# Question ID: 30104
index = which.max(FluTrain$ILI)
FluTrain[index,]

# Question ID: 30105

index = which.max(FluTrain$Queries)
FluTrain[index,]

# Question ID: 30201

m1 = lm(ILI ~ Queries, data = FluTrain)
m1
summary(m1)

m2 = lm(log(ILI) ~ Queries, data = FluTrain)
m2
summary(m2)

# Question ID: 30106
c = cor(FluTrain$Queries, log(FluTrain$ILI))
c^2

# Question ID: 30203
FluTrend1 = lm(log(ILI) ~ Queries, data = FluTrain)
summary(FluTrend1)


PredTest1 = predict(FluTrend1, newdata=FluTest)
PredTest1


PredTest1 = exp(predict(FluTrend1, newdata=FluTest))
PredTest1

which(FluTest$Week == "2012-03-11 - 2012-03-17")
PredTest1[11]

# Question ID: 30301
# two weeks lag, why?

# Question ID: 30204
(FluTest$ILI[11] - PredTest1[11])/FluTest$ILI[11]

# Question ID: 30302
install.packages("zoo")
library(zoo)

ILILag2 = lag(zoo(FluTrain$ILI), -2, na.pad=TRUE)
FluTrain$ILILag2 = coredata(ILILag2)

head(FluTrain)
summary(FluTrain)

# Question ID: 30303
FluTrend2 = lm(log(ILI) ~ Queries + log(ILILag2), data = FluTrain)
FluTrend2
summary(FluTrend2)

# Question ID: 30401
ILILag2 = lag(zoo(FluTest$ILI), -2, na.pad=TRUE)
FluTest$ILILag2 = coredata(ILILag2)
summary(FluTest)

# Question ID: 30402
tail(FluTrain)
head(FluTest)

# Question ID: 30304
plot(log(FluTrain$ILILag2), log(FluTrain$ILI))
plot(log(FluTest$ILILag2), log(FluTest$ILI))

# Question ID: 30404
FluTest$ILILag2[1] = FluTrain$ILI[416]
FluTest$ILILag2[2] = FluTrain$ILI[417]
head(FluTest)

# Question ID: 30405

PredTest2 = exp(predict(FluTrend2, newdata=FluTest))
SSE = sum((PredTest2-FluTest$ILI)^2)
RMSE = sqrt(SSE / nrow(FluTest))
RMSE

sqrt(mean((PredTest2-FluTest$ILI)^2))

sqrt(mean((PredTest1-FluTest$ILI)^2))



