
# 1.1
emails = read.csv("emails.csv",stringsAsFactors=FALSE)

# 1.2
table(emails$spam)

# 1.3
emails$text[1]

# 1.4

# 1.5
max(nchar(emails$text))

# 1.6
which.min(nchar(emails$text))


# 2.1

# Load tm package
library(tm)

# 1) Build a new corpus variable called corpus.
corpus = VCorpus(VectorSource(emails$text))

# 2) Using tm_map, convert the text to lowercase.
corpus = tm_map(corpus, content_transformer(tolower))

# 3) Using tm_map, remove all punctuation from the corpus.
corpus = tm_map(corpus, removePunctuation)

# 4) Using tm_map, remove all English stopwords from the corpus.
corpus = tm_map(corpus, removeWords, stopwords("english"))

# 5) Using tm_map, stem the words in the corpus.
corpus = tm_map(corpus, stemDocument)

# 6) Build a document term matrix from the corpus, called dtm.
dtm = DocumentTermMatrix(corpus)
dtm

# 2.2
spdtm = removeSparseTerms(dtm, 0.95)
spdtm

# 2.3
emailsSparse = as.data.frame(as.matrix(spdtm))
colnames(emailsSparse) = make.names(colnames(emailsSparse))

sort(colSums(emailsSparse))

# 2.4
emailsSparse$spam = emails$spam

sort(colSums(subset(emailsSparse, spam == 0)))

# 2.5
sort(colSums(subset(emailsSparse, spam == 1)))

# 2.6

#########################################################

# 3.1
emailsSparse$spam = as.factor(emailsSparse$spam)
set.seed(123)
library(caTools)

spl = sample.split(emailsSparse$spam, 0.7)
train = subset(emailsSparse, spl == TRUE)
test = subset(emailsSparse, spl == FALSE)


# model
spamLog = glm(spam ~., data = train, family = "binomial")
spamCART = rpart(spam~., data=train, method="class")
spamRF = randomForest(spam~., data=train)

# prediction 
predTrainLog = predict(spamLog, type="response")
predTrainCART = predict(spamCART)[,2]
predTrainRF = predict(spamRF, type="prob")[,2]


# prediction - glm
table(predTrainLog < 0.00001)
table(predTrainLog > 0.99999)
table(predTrainLog >= 0.00001 & predTrainLog <= 0.99999)


##########################################################

# 3.2
options(max.print=2000)
summary(spamLog)

coef(summary(spamLog))[,4]
table(coef(summary(spamLog))[,4] < 0.05)

# 3.3
prp(spamCART)


# 3.4 Evaluate the performance of the model - training set - glm

T = table(train$spam, predTrainLog >= 0.5)
T

# Compute accuracy
sum(diag(T))/sum(T)


##########################################################

# 3.5

library(ROCR)

predROCR = prediction(predTrainLog, train$spam)
perfROCR = performance(predROCR, "tpr", "fpr")
plot(perfROCR, colorize=TRUE)

# Compute AUC
performance(predROCR, "auc")@y.values

##########################################################


# 3.6 Evaluate the performance of the model - training set - cart

# (Remember that if you used the type="class" argument when making predictions, you automatically used a threshold of 0.5. 
# If you did not add in the type argument to the predict function, the probabilities are in the second column of the predict output.)

T = table(train$spam, predTrainCART > 0.5)
T

# Compute accuracy
sum(diag(T))/sum(T)

# 3.7
predROCR = prediction(predTrainCART, train$spam)
perfROCR = performance(predROCR, "tpr", "fpr")
plot(perfROCR, colorize=TRUE)

# Compute AUC
performance(predROCR, "auc")@y.values


##########################################################


# 3.8 Evaluate the performance of the model - training set - RF

T = table(train$spam, predTrainRF > 0.5)
T

# Compute accuracy
sum(diag(T))/sum(T)

# 3.9
predROCR = prediction(predTrainRF, train$spam)
perfROCR = performance(predROCR, "tpr", "fpr")
plot(perfROCR, colorize=TRUE)

# Compute AUC
performance(predROCR, "auc")@y.values







########################################################## glm, cart, RF


predTestLog = predict(spamLog, newdata = test, type="response")
predTestCART = predict(spamCART, newdata = test)[,2]
predTestRF = predict(spamRF, newdata = test, type="prob")[,2]

                                           
########################################################## glm

# 4.1

# prediction 
T = table(test$spam, predTestLog >= 0.5)
T

# Compute accuracy
sum(diag(T))/sum(T)

# 4.2
predROCR = prediction(predTestLog, test$spam)
perfROCR = performance(predROCR, "tpr", "fpr")
plot(perfROCR, colorize=TRUE)

# Compute AUC
performance(predROCR, "auc")@y.values


########################################################## cart

# 4.3

# prediction 
T = table(test$spam, predTestCART > 0.5)
T

# Compute accuracy
sum(diag(T))/sum(T)

# 4.4
predROCR = prediction(predTestCART, test$spam)
perfROCR = performance(predROCR, "tpr", "fpr")
plot(perfROCR, colorize=TRUE)

# Compute AUC
performance(predROCR, "auc")@y.values



########################################################## RF

# 4.1

# prediction 
T = table(test$spam, predTestRF > 0.5)
T

# Compute accuracy
sum(diag(T))/sum(T)

# 4.2
predROCR = prediction(predTestRF, test$spam)
perfROCR = performance(predROCR, "tpr", "fpr")
plot(perfROCR, colorize=TRUE)

# Compute AUC
performance(predROCR, "auc")@y.values

########################################################## 

# 6.1
wordCount = rowSums(as.matrix(dtm))
wordCount

wordCount = rowSums(as.matrix(spdtm))
wordCount

# 6.2
hist(wordCount)

# 6.3
hist(log(wordCount))

# 6.4
emailsSparse$logWordCount = log(wordCount)
boxplot(emailsSparse$logWordCount ~ emailsSparse$spam)

table(emailsSparse$spam)

# 6.5
train2 = subset(emailsSparse, spl == TRUE)
test2 = subset(emailsSparse, spl == FALSE)

# model
spam2CART = rpart(spam~., data=train2, method="class")
spam2RF = randomForest(spam~., data=train2)

prp(spamCART)
prp(spam2CART)


########################################################## 

predTrain2CART = predict(spam2CART)[,2]
predTrain2RF = predict(spam2RF, type="prob")[,2]


########################################################## 

predTest2CART = predict(spam2CART, newdata = test2)[,2]
predTest2RF = predict(spam2RF, newdata = test2, type="prob")[,2]

########################################################## 


# 6.6

# prediction 
T = table(test2$spam, predTest2CART > 0.5)
T

# Compute accuracy
sum(diag(T))/sum(T)

# 6.7
predROCR = prediction(predTest2CART, test2$spam)
perfROCR = performance(predROCR, "tpr", "fpr")
plot(perfROCR, colorize=TRUE)

# Compute AUC
performance(predROCR, "auc")@y.values


########################################################## 


# 6.6

# prediction 
T = table(test2$spam, predTest2RF > 0.5)
T

# Compute accuracy
sum(diag(T))/sum(T)

# 6.7
predROCR = prediction(predTest2RF, test2$spam)
perfROCR = performance(predROCR, "tpr", "fpr")
plot(perfROCR, colorize=TRUE)

# Compute AUC
performance(predROCR, "auc")@y.values
