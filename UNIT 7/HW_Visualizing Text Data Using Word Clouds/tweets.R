
install.packages("tm")
install.packages("SnowballC")

library(tm)
library(SnowballC)


# 1.1 load and preprocess text
tweets = read.csv("tweets.csv", stringsAsFactors=FALSE)

head(tweets)
str(tweets)


corpus = VCorpus(VectorSource(tweets$Tweet))

corpus = tm_map(corpus, content_transformer(tolower))

corpus = tm_map(corpus, PlainTextDocument)

corpus = tm_map(corpus, removePunctuation)

removedCorpus = tm_map(corpus, removeWords, stopwords("english"))

dtm = DocumentTermMatrix(removedCorpus)
allTweets = as.data.frame(as.matrix(dtm))
ncol(allTweets)



# 1.2
install.packages("wordcloud")
library(wordcloud)


##################################################


# 2.1
# 2.2
# 2.3
wordcloud(colnames(allTweets), colSums(allTweets), scale=c(2,0.25))

wordcloud(colnames(allTweets), colSums(allTweets), scale=c(4,0.5))



# 2.4
removedCorpus = tm_map(corpus, removeWords, c("apple",stopwords("english")))
dtm = DocumentTermMatrix(removedCorpus)
updateTweets = as.data.frame(as.matrix(dtm))
wordcloud(colnames(updateTweets), colSums(updateTweets),scale=c(4, 0.5))

# 3.1
negativeTweets = subset(allTweets, tweets$Avg <= -1)
wordcloud(colnames(negativeTweets), colSums(negativeTweets)) 

negativeTweets = subset(updateTweets, tweets$Avg <= -1)
wordcloud(colnames(negativeTweets), colSums(negativeTweets)) 


# 3.2
wordcloud(colnames(negativeTweets), colSums(negativeTweets), min.freq = 4)

wordcloud(colnames(updateTweets), colSums(updateTweets))


# 3.3

wordcloud(colnames(negativeTweets), colSums(negativeTweets), min.freq = 5, random.order=FALSE)

wordcloud(colnames(updateTweets), colSums(updateTweets), min.freq = 5, random.order=TRUE)
wordcloud(colnames(updateTweets), colSums(updateTweets), min.freq = 5, random.order=FALSE)


# 3.4
wordcloud(colnames(updateTweets), colSums(updateTweets), min.freq = 4, rot.per=0.1)
wordcloud(colnames(updateTweets), colSums(updateTweets), min.freq = 4)

# 3.5
wordcloud(colnames(negativeTweets), colSums(negativeTweets), min.freq = 4, 
          colors=c("red", "black"), random.color=TRUE)

# 4
install.packages("RColorBrewer")
library(RColorBrewer)

# 4.1
wordcloud(colnames(negativeTweets), colSums(negativeTweets), scale=c(5, 0.5), colors=brewer.pal(9, "Accent")[c(-1,-2,-3,-4)])
wordcloud(colnames(negativeTweets), colSums(negativeTweets), scale=c(5, 0.5), colors=brewer.pal(9, "Set2")[c(-1,-2,-3,-4)])
wordcloud(colnames(negativeTweets), colSums(negativeTweets), scale=c(5, 0.5), colors=brewer.pal(9, "YlOrRd")[c(-1,-2,-3,-4)])

# 4.2
wordcloud(colnames(negativeTweets), colSums(negativeTweets), scale=c(5, 0.5), colors=brewer.pal(9, "Greys")[c(-1,-2,-3,-4)])

# 4.3
wordcloud(colnames(negativeTweets), colSums(negativeTweets), scale=c(5, 0.5), colors=brewer.pal(9, "Blues")[c(-1,-2,-3,-4)])
wordcloud(colnames(negativeTweets), colSums(negativeTweets), scale=c(5, 0.5), colors=brewer.pal(9, "Blues")[c(5,6,7,8)])

c(-1,-2,-3,-4)
