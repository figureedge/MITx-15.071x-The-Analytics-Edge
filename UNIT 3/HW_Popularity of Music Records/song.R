
songs = read.csv("songs.csv")
head(songs)
str(songs)

# 1.1
a = subset(songs, year == 2010)
head(a)
table(songs$year)

# 1.2

library(dplyr)
b = filter(songs, grepl("Michael Jackson", artistname))
head(b)


MJ = subset(songs, artistname == "Michael Jackson")
head(MJ)
str(MJ)

# 1.3 
MJ[c("songtitle","Top10")]

# 1.4
table(songs$timesignature)

# 1.5
index = which.max(songs$tempo)
songs$songtitle[index]

# 2.1
SongsTrain = subset(songs, year != 2010)
SongsTest = subset(songs, year == 2010)

# 2.2


# remore vars with characters
# lm and glm can only take numbers 
nonvars = c("year", "songtitle", "artistname", "songID", "artistID")

SongsTrain = SongsTrain[ , !(names(SongsTrain) %in% nonvars) ]
SongsTest = SongsTest[ , !(names(SongsTest) %in% nonvars) ]
head(SongsTrain)

SongsLog1 = glm(Top10 ~ ., data = SongsTrain, family = binomial)
summary(SongsLog1)

# 2.3
# Problem 2.3 - Creating Our Prediction Model
# If you look at the output summary(model), where model is the name of your logistic regression model, 
# you can see that the coefficient estimates for the confidence variables (timesignature_confidence, 
# key_confidence, and tempo_confidence) are positive. This means that higher confidence leads to a 
# higher predicted probability of a Top 10 hit.

# 2.4
# Since the coefficient values for timesignature_confidence, tempo_confidence, and key_confidence are 
# all positive, lower confidence leads to a lower predicted probability of a song being a hit. 
# So mainstream listeners tend to prefer less complex songs.

# 2.5
# The coefficient estimate for loudness is positive, meaning that mainstream listeners prefer louder songs, 
# which are those with heavier instrumentation. However, the coefficient estimate for energy is negative, 
# meaning that mainstream listeners prefer songs that are less energetic, which are those with light instrumentation. 
# These coefficients lead us to different conclusions!


# 3.1
cor(SongsTrain$loudness, SongsTrain$energy)

step(SongsLog1)

# 3.2
SongsLog2 = glm(Top10 ~ . - loudness, data = SongsTrain, family = binomial)
summary(SongsLog2)

# The coefficient estimate for energy is positive in Model 2, suggesting that songs with higher energy levels 
# tend to be more popular. However, note that the variable energy is not significant in this model.


# 3.3
SongsLog3 = glm(Top10 ~ . - energy, data = SongsTrain, family = binomial)
summary(SongsLog3)

# 4.1 

# Predictions on the test set
predictTest = predict(SongsLog3, type="response", newdata=SongsTest)

# Confusion matrix with threshold of 0.45
table(SongsTest$Top10, predictTest >= 0.45)

# Accuracy
(309+19)/(309+19+5+40)

# 4.2 baseline model 
table(SongsTest$Top10)
314/(314+59)


# 4.4
# Model 3 has a very high specificity, meaning that it favors specificity over sensitivity. 
# While Model 3 only captures less than half of the Top 10 songs, it still can offer a competitive edge, 
# since it is very conservative in its predictions.

# sensitivity 
19/(40+19)

# specificity 
309/(309+5)




