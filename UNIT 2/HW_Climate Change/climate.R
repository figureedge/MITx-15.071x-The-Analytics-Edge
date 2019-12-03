
climate = read.csv("climate_change.csv")
head(climate)
summary(climate)


train = subset(climate, Year <= 2006)
test = subset(climate, Year > 2006)

# full data set 
m1 = lm(Temp ~ MEI + CO2 + CH4 + N2O + CFC.11 + CFC.12 + TSI + Aerosols, data = climate)
summary(m1)


## corr and colinearity 

cor(climate$CO2, climate$N2O)
cor(climate)

cor(train)
cor(test)

## training - full model 

m2 = lm(Temp ~ MEI + CO2 + CH4 + N2O + CFC.11 + CFC.12 + TSI + Aerosols, data = train)
summary(m2)

# It is interesting to note that the step function does not address the collinearity of the variables, 
# except that adding highly correlated variables will not improve the R2 significantly.

# The consequence of this is that the step function will not necessarily produce a very interpretable model - 
# just a model that has balanced quality and simplicity for a particular weighting of quality and simplicity (AIC).


m22 = step(m2)
summary(m22)

##############

# Make predictions on test set
PointsPredictions = predict(m22, newdata=test)

# Compute out-of-sample R^2
SSE = sum((PointsPredictions - test$Temp)^2)
SST = sum((mean(train$Temp) - test$Temp)^2)
R2 = 1 - SSE/SST
R2



###############

# Sum of Squared Errors
m2$residuals
SSE = sum(m2$residuals^2)
SSE


# Root mean squared error
RMSE = sqrt(SSE/nrow(train))
RMSE

sqrt(mean(m2$residuals^2))

# 
m3 = lm(Temp ~ MEI + N2O + TSI + Aerosols, data = train)
summary(m3)


