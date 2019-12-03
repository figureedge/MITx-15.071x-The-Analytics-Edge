
loans = read.csv("loans.csv")

head(loans)


# 1.1
table(loans$not.fully.paid)
1533/(8045+1533)


# 1.2
summary(loans)

# list the name with at least one na 
colnames(loans)[colSums(is.na(loans)) > 0]

# 1.4

library(mice)

set.seed(144)

vars.for.imputation = setdiff(names(loans), "not.fully.paid")

imputed = complete(mice(loans[vars.for.imputation]))

loans[vars.for.imputation] = imputed

summary(loans)

# Imputation predicts missing variable values for a given observation using the variable values that are reported. 
# We called the imputation on a data frame with the dependent variable not.fully.paid removed, 
# so we predicted the missing values using only other independent variables.


# 2.1
# because imputed data are different 
# so use loans_imputed.csv

loans = read.csv("loans_imputed.csv")

library(caTools)
set.seed(144)

split = sample.split(loans$not.fully.paid, SplitRatio = 0.7)
train = subset(loans, split == TRUE)
test = subset(loans, split == FALSE)

m1 = glm(not.fully.paid~. , data = train, family = "binomial")
summary(m1)

# 2.2 prediction 
Xbeta = coef(m1)[13]*(700-710)
Xbeta # glm or any lm will provide to you 

odds = exp(Xbeta)
odds

# 2.3
# test set prediction 
predictTest = predict(m1, type="response", newdata=test)
max(predictTest)

# add to test set 
test$predicted.risk = predictTest
head(test)

# Confusion matrix with threshold of 0.5
table(test$not.fully.paid, predictTest > 0.5)

# accuracy
(2400+3)/(2400+3+13+457)

# baseline - not paid in full
(2400+13)/(2400+3+13+457)

# 2.4 Test set AUC 
# The AUC deals with differentiating between a randomly selected positive and negative example. 
# It is independent of the regression cutoff selected.
library(ROCR)
ROCRpred = prediction(predictTest, test$not.fully.paid)
as.numeric(performance(ROCRpred, "auc")@y.values)



####################################### bivariate model #############################

# 3.1
m2 = glm(not.fully.paid ~ int.rate, data = train, family = "binomial")
summary(m2)

# 3.2
predictTest = predict(m2, type="response", newdata=test)
max(predictTest)

table(test$not.fully.paid, predictTest > 0.5)

# 3.3
ROCRpred = prediction(predictTest, test$not.fully.paid)
as.numeric(performance(ROCRpred, "auc")@y.values)

# 4.1

r = 0.06
t =3
c = 10

c*exp(r*t)

# 5.1
test$profit = exp(test$int.rate*3) - 1
test$profit[test$not.fully.paid == 1] = -1

head(test)
max(test$profit)

# 6.1 An Investment Strategy Based on Risk

highInterest = subset(test, test$int.rate >= 0.15)
head(highInterest)

summary(highinterest$profit)

table(highInterest$not.fully.paid)
110/(110+327)


# 6.2 An Investment Strategy Based on Risk

cutoff = sort(highInterest$predicted.risk, decreasing=FALSE)[100]
cutoff

# consisting of the high-interest loans with predicted risk not exceeding the cutoff

selectedLoans = subset(test, test$predicted.risk <= cutoff)
head(selectedLoans)

selectint = selectedLoans[order(selectedLoans$int.rate, decreasing = TRUE),]
head(selectint)

select100 = selectint[1:100,]
head(select100)

summary(select100$profit)
sum(select100$profit)


table(select100$not.fully.paid)



