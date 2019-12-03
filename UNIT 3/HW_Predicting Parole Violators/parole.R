
parole = read.csv("parole.csv")

# 1.1
str(parole)
head(parole)
summary(parole)

# 1.2
table(parole$violator)

# 2.1 unordered factor 
table(parole$crime)
table(parole$state)

# 2.2 similar to table()
race = as.factor(parole$race)
summary(race)

table(parole$race)
summary(parole$race)





# 3.1 Splitting into a Training and Testing Set

library(caTools)

set.seed(144)
split = sample.split(parole$violator, SplitRatio = 0.7)
train1 = subset(parole, split == TRUE)
test1 = subset(parole, split == FALSE)

# wo recall seed the set will be different
split = sample.split(parole$violator, SplitRatio = 0.7)
train2 = subset(parole, split == TRUE)
test2 = subset(parole, split == FALSE)

sum(train1 != train2)




# 4.1

# convert to factor so that you don't need to create dummy var 
parole$state = as.factor(parole$state)
parole$crime = as.factor(parole$crime)

# run above split to create train and test 

# glm
m1 = glm(violator ~. , data=train1, family="binomial")
summary(m1)

# 4.3 logit function 
logit = coef(m1)[1] + coef(m1)[2] + coef(m1)[3] + coef(m1)[4]*50 + coef(m1)[8]*3 + coef(m1)[9]*12 + coef(m1)[11]

# odds
odds = exp(logit)
odds

# convert logits to probability
# just look back to parameter E[Y|X=x] = mu(x) = p  
prob = odds/(1+odds)
prob

# anova
anova(m1, exp(c), test="Chisq")




# 5.1

predictTest = predict(m1, type="response", newdata=test1)
max(predictTest)

# 5.2

# Confusion matrix with threshold of 0.5
table(test1$violator, predictTest > 0.5)

# sensitivity
12/(11+12)

# specificity
167/(167+12)

# accuracy
# sum(diag())/sum(all)
(167+12)/(167+12+11+12)

# 5.3 Evaluating the Model on the Testing Set
# sum(row 0)/sum(all)
(167+12)/(167+12+11+12)

# 5.4
table(test1$violator, predictTest > 0.3)

# 5.5
table(test1$violator, predictTest > 0.9)

# 5.6 Test set AUC 
# The AUC deals with differentiating between a randomly selected positive and negative example. 
# It is independent of the regression cutoff selected.
library(ROCR)
ROCRpred = prediction(predictTest, test1$violator)
as.numeric(performance(ROCRpred, "auc")@y.values)

# 6.1









