
wiki = read.csv("wiki.csv", stringsAsFactors=FALSE)
wiki = read.csv("wiki.csv")


head(wiki)
str(wiki)

# 1.1
wiki$Vandal = as.factor(wiki$Vandal)

summary(wiki$Vandal)
table(wiki$Vandal)

# 1.2

# Install new packages

install.packages("tm")
library(tm)
install.packages("SnowballC")
library(SnowballC)


# 1) Create the corpus for the Added column, and call it "corpusAdded".
corpusAdded = VCorpus(VectorSource(wiki$Added))

# 2) Remove the English-language stopwords.
corpusAdded = tm_map(corpusAdded, removeWords, stopwords("english"))

# 3) Stem the words.
corpusAdded = tm_map(corpusAdded, stemDocument)

# 4) Build the DocumentTermMatrix, and call it dtmAdded.
dtmAdded = DocumentTermMatrix(corpusAdded)
dtmAdded



# 1.3

# Check for sparsity

findFreqTerms(dtmAdded, lowfreq=20)
findFreqTerms(dtmAdded, lowfreq=100)

# Remove sparse terms

sparseAdded = removeSparseTerms(dtmAdded, 0.997)
sparseAdded


# 1.4

# Convert to a data frame

wordsAdded = as.data.frame(as.matrix(sparseAdded))
str(wordsAdded)
head(wordsAdded)

colnames(wordsAdded) = paste("A", colnames(wordsAdded))

# removed bags of words 

corpusRemoved = VCorpus(VectorSource(wiki$Removed))
corpusRemoved = tm_map(corpusRemoved, removeWords, stopwords("english"))
corpusRemoved = tm_map(corpusRemoved, stemDocument)

dtmRemoved = DocumentTermMatrix(corpusRemoved)

sparseRemoved = removeSparseTerms(dtmRemoved, 0.997)
wordsRemoved = as.data.frame(as.matrix(sparseRemoved))

colnames(wordsRemoved) = paste("R", colnames(wordsRemoved))

# 1.5

wikiWords = cbind(wordsAdded, wordsRemoved)

# Add dependent variable
wikiWords$Vandal = wiki$Vandal

# Split the data

library(caTools)

set.seed(123)

spl = sample.split(wikiWords$Vandal, SplitRatio = 0.7)
wikiTrain = subset(wikiWords, spl==TRUE)
wikiTest = subset(wikiWords, spl==FALSE)


# baseline accuracy for test set 

table(wikiTest$Vandal)

618/(618+545)


# 1.6

# Build a CART model

library(rpart)
library(rpart.plot)

wikiCART = rpart(Vandal ~ ., data=wikiTrain, method = "class")
prp(wikiCART)


# Evaluate the performance of the model

wikiCART = rpart(Vandal ~ ., data=wikiTrain, method="class")

# prediction output 
testPredictCART = predict(wikiCART, newdata=wikiTest, type="class")

# matrix 
T =table(wikiTest$Vandal, testPredictCART)
T

# Compute accuracy
sum(diag(T))/sum(T)


###############################################


# 2.1
wikiWords2 = wikiWords
wikiWords2$HTTP = ifelse(grepl("http",wiki$Added,fixed=TRUE), 1, 0)

table(wikiWords2$HTTP)

# 2.2

wikiTrain2 = subset(wikiWords2, spl==TRUE)
wikiTest2 = subset(wikiWords2, spl==FALSE)

wikiCART2 = rpart(Vandal ~ ., data=wikiTrain2, method = "class")
prp(wikiCART2)


# Evaluate the performance of the model

wikiCART2 = rpart(Vandal ~ ., data=wikiTrain2, method="class")

# prediction output 
testPredictCART2 = predict(wikiCART2, newdata=wikiTest2, type="class")

# matrix 
T = table(wikiTest2$Vandal, testPredictCART2)
T

# Compute accuracy
sum(diag(T))/sum(T)


# 2.3

wikiWords2$NumWordsAdded = rowSums(as.matrix(dtmAdded))
wikiWords2$NumWordsRemoved = rowSums(as.matrix(dtmRemoved))

summary(wikiWords2$NumWordsAdded)

# 2.4 adding more indep vars in data set 

wikiTrain2 = subset(wikiWords2, spl==TRUE)
wikiTest2 = subset(wikiWords2, spl==FALSE)


# Evaluate the performance of the model
wikiCART2 = rpart(Vandal ~ ., data=wikiTrain2, method="class")
prp(wikiCART2)


# prediction output 
testPredictCART2 = predict(wikiCART2, newdata=wikiTest2, type="class")

# matrix 
T = table(wikiTest2$Vandal, testPredictCART2)
T

# Compute accuracy
sum(diag(T))/sum(T)


#################################################

# 3.1

wikiWords3 = wikiWords2
wikiWords3$Minor = wiki$Minor
wikiWords3$Loggedin = wiki$Loggedin


wikiTrain3 = subset(wikiWords3, spl==TRUE)
wikiTest3 = subset(wikiWords3, spl==FALSE)


# Evaluate the performance of the model

wikiCART3 = rpart(Vandal ~ ., data=wikiTrain3, method="class")
prp(wikiCART3)


# prediction output 
testPredictCART3 = predict(wikiCART3, newdata=wikiTest3, type="class")

# matrix 
T = table(wikiTest3$Vandal, testPredictCART3)
T

# Compute accuracy
sum(diag(T))/sum(T)










