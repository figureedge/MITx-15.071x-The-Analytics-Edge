
letters = read.csv("letters_ABPR.csv")

head(letters)
str(letters)

# 1.1

letters$isB = as.factor(letters$letter == "B")
head(letters)

# Split the data
library(caTools)
set.seed(1000)

spl = sample.split(letters$isB, SplitRatio = 0.5)
Train = subset(letters, spl==TRUE)
Test = subset(letters, spl==FALSE)

# check which is more common
table(Train$isB)

T = table(Test$isB)
T
T[1]/sum(T)

#####################################

# 1.2 CART model 

library(rpart)
library(rpart.plot)


CARTb = rpart(isB ~ . - letter, data=Train, method="class")
prp(CARTb)

# same as above
CARTb = rpart(isB ~ . - letter, data=Train)
prp(CARTb)

# of not classification problem 
CARTb = rpart(isB ~ . - letter, data=Train, cp=0.0)
prp(CARTb)


# Make predictions
PredictTest = predict(CARTb, newdata = Test, type = "class")

T = table(Test$isB, PredictTest)
T

# accuracy
sum(diag(T))/sum(T)

#########################################

# 1.3 random forest model
# In lecture, we noted that random forests tends to improve on CART in terms of predictive accuracy. 
# Sometimes, this improvement can be quite significant, as it is here.


library(randomForest)

# Build random forest model
Forest = randomForest(isB ~. - letter, data = Train)

# Make predictions
PredictForest = predict(Forest, newdata = Test)
T = table(Test$isB, PredictForest)
T

# accuracy
sum(diag(T))/sum(T)


################################## Predicting the letters A, B, P, R

# 2.1 baseline 

letters$letter = as.factor(letters$letter)
head(letters)
str(letters)

set.seed(2000)

spl = sample.split(letters$letter, SplitRatio = 0.5)
Train = subset(letters, spl==TRUE)
Test = subset(letters, spl==FALSE)

T = table(Test$letter)
T
T[3]/sum(T)

# 2.2 CART

CARTletter = rpart(letter ~ . - isB, data=Train, method="class")
prp(CARTletter)

# Make predictions
PredictTest = predict(CARTletter, newdata = Test, type = "class")

T = table(Test$letter, PredictTest)
T

# accuracy
sum(diag(T))/sum(T)



# 2.3 random forest 


# Build random forest model
set.seed(1000)
Forest = randomForest(letter ~. - isB, data = Train)

# Make predictions
PredictForest = predict(Forest, newdata = Test)
T = table(Test$letter, PredictForest)
T

# accuracy
sum(diag(T))/sum(T)













