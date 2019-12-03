
trials = read.csv("clinical_trial.csv",stringsAsFactors=FALSE)
summary(trials)
str(trials)

# 1.1
max(nchar(trials$abstract))

# 1.2
table(nchar(trials$abstract) == 0)

# 1.3

min(nchar(trials$title))
index = which.min(nchar(trials$title))
trials$title[index]


# 2.1

# Load tm package
library(tm)

# 1) Convert the title variable to corpusTitle and the abstract variable to corpusAbstract.
corpusTitle = VCorpus(VectorSource(trials$title)) 
corpusAbstract = VCorpus(VectorSource(trials$abstract))

# 2) Convert corpusTitle and corpusAbstract to lowercase.
corpusTitle = tm_map(corpusTitle, content_transformer(tolower))
corpusAbstract = tm_map(corpusAbstract, content_transformer(tolower))

corpusTitle[[1]]$content

# 3) Remove the punctuation in corpusTitle and corpusAbstract.
corpusTitle = tm_map(corpusTitle, removePunctuation)
corpusAbstract = tm_map(corpusAbstract, removePunctuation)

corpusTitle[[1]]$content

# 4) Remove the English language stop words from corpusTitle and corpusAbstract.
corpusTitle = tm_map(corpusTitle, removeWords, stopwords("english"))
corpusAbstract = tm_map(corpusAbstract, removeWords, stopwords("english"))

corpusTitle[[1]]$content

# 5) Stem the words in corpusTitle and corpusAbstract (each stemming might take a few minutes).
corpusTitle = tm_map(corpusTitle, stemDocument)
corpusAbstract = tm_map(corpusAbstract, stemDocument)

corpusTitle[[1]]$content

# 6) Build a document term matrix called dtmTitle from corpusTitle and dtmAbstract from corpusAbstract.
dtmTitle = DocumentTermMatrix(corpusTitle)
dtmAbstract = DocumentTermMatrix(corpusAbstract)

# 7) Limit dtmTitle and dtmAbstract to terms with sparseness of at most 95% (aka terms that appear in at least 5% of documents).
sparseTitle = removeSparseTerms(dtmTitle, 0.95)
sparseAbstract = removeSparseTerms(dtmAbstract, 0.95)

# 8) Convert dtmTitle and dtmAbstract to data frames (keep the names dtmTitle and dtmAbstract).
dtmTitle = as.data.frame(as.matrix(sparseTitle))
dtmAbstract = as.data.frame(as.matrix(sparseAbstract))

str(dtmTitle)
head(dtmTitle)

#######################################################################

# 2.2

# direct sort
sort(colSums(dtmAbstract))

# or find max index
csAbstract = colSums(dtmAbstract)
index = which.max(csAbstract)
csAbstract[index]

# 3.1
colnames(dtmTitle) = paste0("T", colnames(dtmTitle))
colnames(dtmAbstract) = paste0("A", colnames(dtmAbstract))

# 3.2
dtm = cbind(dtmTitle, dtmAbstract)
dtm$trial = trials$trial

# 3.3

library(caTools)

set.seed(144)

split = sample.split(dtm$trial, SplitRatio = 0.7)

trainSparse = subset(dtm, split==TRUE)
testSparse = subset(dtm, split==FALSE)

# Baseline accuracy 

table(testSparse$Negative)
T = table(dtm$trial)
T
T[1]/sum(T)


# 3.4
# Build a CART model

library(rpart)
library(rpart.plot)

trialCART = rpart(trial ~., data=trainSparse, method = "class")
prp(trialCART)

# 3.5
pred = predict(trialCART)
pred.prob = pred[,2]
max(pred.prob)


# 3.6 how do you expect the maximum predicted probability to differ in the testing set
pred = predict(trialCART, newdata = testSparse)
pred.prob = pred[,2]
max(pred.prob)



# 3.7 Evaluate the performance of the model - training set
predictCART = predict(trialCART)

T = table(trainSparse$trial, predictCART[,2] >= 0.5)
T

# Compute accuracy
sum(diag(T))/sum(T)

# sensitivity
T[2,2]/sum(T[2,])

# specificity 
T[1,1]/sum(T[1,])


# 4.1 Evaluating the model on the testing set
pred = predict(trialCART, newdata = testSparse)
pred.prob = pred[,2]

# Compute accuracy
T = table(testSparse$trial, pred.prob >= 0.5)
T

sum(diag(T))/sum(T)


# 4.2 Evaluating the model on the testing set - ROC curve

library(ROCR)

predROCR = prediction(pred.prob, testSparse$trial)

perfROCR = performance(predROCR, "tpr", "fpr")

plot(perfROCR, colorize=TRUE)

# Compute AUC

performance(predROCR, "auc")@y.values


