

pisaTrain = read.csv("pisa2009train.csv") 
pisaTest = read.csv("pisa2009test.csv")


# Question ID: 20102

head(pisaTrain)

tapply(pisaTrain$readingScore, pisaTrain$male, mean)


# Question ID: 20103

summary(pisaTrain)

# list the name with at least one na 
colnames(pisaTrain)[colSums(is.na(pisaTrain)) > 0]


# Question ID: 20104
head(pisaTrain)

# Question ID: 20106

summary(pisaTrain$minutesPerWeekEnglish)

minset = pisaTrain$minutesPerWeekEnglish
maxset = pisaTrain$minutesPerWeekEnglish

minset[is.na(minset)] = 0
maxset[is.na(maxset)] = 2400

summary(minset)
summary(maxset)

# Question ID: 20201

pisaTrain = na.omit(pisaTrain) 
pisaTest = na.omit(pisaTest)

# Question ID: 20202

table(pisaTrain$raceeth)
sort(table(pisaTrain$raceeth))

# Question ID: 20301

str(pisaTrain$raceeth)
head(pisaTrain$raceeth)

pisaTrain$raceeth = relevel(pisaTrain$raceeth, "White")
pisaTest$raceeth = relevel(pisaTest$raceeth, "White")

# including all vars 
lmScore = lm(readingScore ~ ., data = pisaTrain)
lmScore
summary(lmScore)

# Question ID: 20401
summary(pisaTest$readingScore) # compare to test set 

predTest = predict(lmScore, newdata = pisaTest)
summary(predTest) # prediction 

