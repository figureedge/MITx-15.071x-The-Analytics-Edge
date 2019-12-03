

census = read.csv("census.csv")

str(census)
summary(census)
head(census)

# 1.1

set.seed(2000)

spl = sample.split(census$over50k, SplitRatio = 0.6)
Train = subset(census, spl==TRUE)
Test = subset(census, spl==FALSE)

m1 = glm(over50k ~. , data = Train, family = "binomial")
summary(m1)


# convert back to character 

install.packages("varhandle")
library(varhandle)

census.nonf = unfactor(census)
str(census.nonf)


# 1.2 glm

# glm prediction
PredTest.glm = predict(m1, newdata = Test, type="response")

T = table(Test$over50k, PredTest.glm >= 0.5)
T

# glm accuracy
sum(diag(T))/sum(T)


# baseline accuracy
base = table(Test$over50k)
base
base[1]/sum(base)


# AUC
library(ROCR)

# ROC prediction
ROCRpredTest = prediction(PredTest.glm, Test$over50k)

# AUC for ROC 
auc = as.numeric(performance(ROCRpredTest, "auc")@y.values)
auc


############################ CART

# 2.1 CART

library(rpart)
library(rpart.plot)

CART = rpart(over50k ~ ., data=Train, method="class")
prp(CART)

# 2.4

# Make predictions
PredTest.cart = predict(CART, newdata = Test, type = "class")

T = table(Test$over50k, PredTest.cart)
T

# accuracy
sum(diag(T))/sum(T)

# 2.5

# ROC curve
library(ROCR)

# remove type="class" to evaluate probability 
PredictROC = predict(CART, newdata = Test)
PredictROC

# ROC for cart
pred = prediction(PredictROC[,2], Test$over50k)
perf = performance(pred, "tpr", "fpr")
plot(perf)

# ROC for glm
pred = prediction(PredTest.glm, Test$over50k)
perf = performance(pred, "tpr", "fpr")
plot(perf)


# 2.6

# AUC for ROC 
auc = as.numeric(performance(pred, "auc")@y.values)
auc

####################### random forest 

# 3.1

set.seed(1)
trainSmall = Train[sample(nrow(Train), 2000), ]


library(randomForest)

# Build random forest model
Forest = randomForest(over50k ~. , data = trainSmall)

# Make predictions
PredictForest = predict(Forest, newdata = Test)
T = table(Test$over50k, PredictForest)
T

# accuracy
sum(diag(T))/sum(T)

# 3.2

MODEL = Forest

vu = varUsed(MODEL, count=TRUE)

vusorted = sort(vu, decreasing = FALSE, index.return = TRUE)

dotchart(vusorted$x, names(MODEL$forest$xlevels[vusorted$ix]))

# 3.3

varImpPlot(MODEL)


########################################## Selecting cp by Cross-Validation

# 4.1


# Load libraries for cross-validation
library(caret)
library(e1071)

# Number of folds
tr.control = trainControl(method = "cv", number = 10)

# cp values
set.seed(2)
cartGrid = expand.grid( .cp = seq(0.002,0.1,0.002))


# Cross-validation
tr = train(over50k~. , data = Train, method = "rpart", trControl = tr.control, tuneGrid = cartGrid)
tr


# 4.2 predict cart with opt cp 


# run CART model again
CART = rpart(over50k ~ . , data=Train, cp=0.002)
prp(CART)


# Make predictions
PredictTest = predict(CART, newdata = Test, type = "class")

T = table(Test$over50k, PredictTest)
T

# accuracy
sum(diag(T))/sum(T)


# 4.3

# Extract tree
best.tree = tr$finalModel
prp(best.tree)









