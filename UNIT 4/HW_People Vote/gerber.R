
gerber = read.csv("gerber.csv")

head(gerber)
summary(gerber)

# 1.1

T = table(gerber$voting)
T

T[2]/sum(T)

# 1.2

table(gerber$voting,gerber$civicduty)

tapply(gerber$voting, gerber$civicduty, mean)

tapply(gerber$voting, gerber$hawthorne, mean)

tapply(gerber$voting, gerber$self, mean)

tapply(gerber$voting, gerber$neighbors, mean)

# 1.3

# Logistic Regression Model
m1 = glm(voting ~. - sex - yob - control, data = gerber, family = binomial)
summary(m1)

# 1.4 Exploration and Logistic Regression

# if there is new data to predicy 
pred = predict(m1, newdata = gerber,type="response")

# or it will apply the same data for the glm model 
pred = predict(m1, type="response")

T = table(gerber$voting, pred >= 0.3)
T

# accuracy
sum(diag(T))/sum(T)

# 1.5 Exploration and Logistic Regression

T = table(gerber$voting, pred >= 0.5)
T
# accuracy
sum(diag(T))/sum(T)

# 1.6

# baseline accuracy
base = table(gerber$voting)
base[1]/sum(base)

# AUC
library(ROCR)

m1 = glm(voting ~. - sex - yob - control, data = gerber, family = binomial)

# glm prediction
predTest = predict(m1, newdata = gerber,type="response")

# ROC prediction
ROCRpredTest = prediction(predTest, gerber$voting)

# AUC for ROC 
auc = as.numeric(performance(ROCRpredTest, "auc")@y.values)
auc



################################ Trees #################################

# 2.1

library(rpart)
library(rpart.plot)

CARTmodel = rpart(voting ~ civicduty + hawthorne + self + neighbors, data=gerber)
prp(CARTmodel)

summary(CARTmodel)


# 2.2

CARTmodel2 = rpart(voting ~ civicduty + hawthorne + self + neighbors, data=gerber, cp=0.0)
prp(CARTmodel2)

summary(CARTmodel2)

# 2.4

CARTmodel3 = rpart(voting ~ civicduty + hawthorne + self + neighbors + sex, data=gerber, cp=0.0)
prp(CARTmodel3)

summary(CARTmodel3)

############################### Interaction Terms in glm #######################

# 3.1

CARTmodel4 = rpart(voting ~ control, data=gerber, cp=0.0)
prp(CARTmodel4, digits = 6)

0.34 - 0.296638

CARTmodel5 = rpart(voting ~ sex, data=gerber, cp=0.0)
prp(CARTmodel5, digits = 6)

0.321889 - 0.309893

# 3.2

CARTmodel6 = rpart(voting ~ sex + control, data=gerber, cp=0.0)
prp(CARTmodel6, digits = 6)

0.345818 - 0.302795
0.334176 - 0.290456

# 3.3 compare to glm model

LogModelSex = glm(voting ~ sex + control, data=gerber, family = binomial)
summary(LogModelSex)

# 3.4
Possibilities = data.frame(sex=c(0,0,1,1),control=c(0,1,0,1))
Possibilities

predict(LogModelSex, newdata=Possibilities, type="response")

0.290456 - 0.2908065

# 3.5

LogModel2 = glm(voting ~ sex + control + sex:control, data=gerber, family="binomial")
summary(LogModel2)

# 3.6
predict(LogModel2, newdata=Possibilities, type="response")

0.2904558 - 0.290456











