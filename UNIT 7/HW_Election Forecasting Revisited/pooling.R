
install.packages("maps")
install.packages("ggmap")

library(ggplot2)
library(maps)
library(ggmap)

statesMap = map_data("state")

# 1.1
str(statesMap)
table(statesMap$group)

# 1.2
ggplot(statesMap, aes(x = long, y = lat, group = group)) + geom_polygon(fill = "white", color = "black") 

# 2.1
polling = read.csv("PollingImputed.csv")
table(polling$Year)


# split train set and test set 
Train = subset(polling, Year < "2012")
Test = subset(polling, Year=="2012")

# glm and predict
mod2 = glm(Republican ~ SurveyUSA + DiffCount, data=Train, family="binomial")
TestPrediction = predict(mod2, newdata=Test, type="response")



# create the data frame 
TestPredictionBinary = as.numeric(TestPrediction > 0.5)

# data frame with three vars, P(Y=1|x), test threahold, state 
predictionDataFrame = data.frame(TestPrediction, TestPredictionBinary, Test$State)

table(predictionDataFrame$TestPredictionBinary)
22/45

mean(TestPrediction)


# 2.2

# trans region and merge
predictionDataFrame$region = tolower(predictionDataFrame$Test.State)
predictionMap = merge(statesMap, predictionDataFrame, by = "region")

# Reorder the data
predictionMap = predictionMap[order(predictionMap$order),]


# 2.4

ggplot(predictionMap, aes(x = long, y = lat, group = group, fill = TestPredictionBinary)) + geom_polygon(color = "black")

# 2.5 Coloring the States by Predictions

ggplot(predictionMap, aes(x = long, y = lat, group = group, fill = TestPredictionBinary)) + 
  geom_polygon(color = "black") + 
  scale_fill_gradient(low = "blue", high = "red", guide = "legend", breaks= c(0,1), labels = c("Democrat", "Republican"), name = "Prediction 2012")


table(TestPrediction>0.9)
table(TestPrediction<0.1)


################################################


# 3.1 plot 
ggplot(predictionMap, aes(x = long, y = lat, group = group, fill = TestPredictionBinary)) + geom_polygon(color = "black")

predictionDataFrame$Test.State
predictionDataFrame[6,]


# 3.2 plot
ggplot(predictionMap, aes(x = long, y = lat, group = group, fill = TestPredictionBinary))+ geom_polygon(color = "black") + scale_fill_gradient(low = "blue", high = "red", guide = "legend", breaks= c(0,1), labels = c("Democrat", "Republican"), name = "Prediction 2012")

ggplot(predictionMap, aes(x = long, y = lat, group = group, fill = TestPrediction))+ geom_polygon(color = "black") + scale_fill_gradient(low = "blue", high = "red", guide = "legend", name = "Prediction 2012")

# 4

# plot 1
ggplot(predictionMap, aes(x = long, y = lat, group = group, fill = TestPrediction))+ geom_polygon(color = "black", linetype=3) + scale_fill_gradient(low = "blue", high = "red", guide = "legend", name = "Prediction 2012")

# plot 2
ggplot(predictionMap, aes(x = long, y = lat, group = group, fill = TestPrediction))+ geom_polygon(color = "black", size=3) + scale_fill_gradient(low = "blue", high = "red", guide = "legend", name = "Prediction 2012")

# plot 3
ggplot(predictionMap, aes(x = long, y = lat, group = group, fill = TestPrediction))+ geom_polygon(color = "black", alpha=0.3) + scale_fill_gradient(low = "blue", high = "red", guide = "legend", name = "Prediction 2012")
