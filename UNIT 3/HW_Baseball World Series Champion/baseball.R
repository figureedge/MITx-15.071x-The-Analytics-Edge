
baseball = read.csv("baseball.csv")
head(baseball)

# 1.2
table(baseball$Year)
length(table(baseball$Year))

unique(baseball$Year)

# 1.3

baseball = subset(baseball, baseball$Playoffs == 1)
head(baseball)

# 1.4
table(baseball$Year)
table(table(baseball$Year))

# 2.1

PlayoffTable = table(baseball$Year)
names(PlayoffTable)
str(names(PlayoffTable)) 

# 2.2

PlayoffTable["1962"]
PlayoffTable[c("1962","2001")]

# 2.3 Adding an Important Predictor

as.character(baseball$Year)

baseball$NumCompetitors = PlayoffTable[as.character(baseball$Year)]
head(baseball)

# 2.4

cc = subset(baseball, baseball$NumCompetitors == 8)
table(baseball$NumCompetitors)

# 3.1

baseball$WorldSeries = as.numeric(baseball$RankPlayoffs == 1)
table(baseball$WorldSeries)

# 3.2

m1 = glm(WorldSeries ~ League, data = baseball, family = "binomial")
summary(m1)

# 4.1

m2 = glm(WorldSeries ~ Year + RA + RankSeason + NumCompetitors, data = baseball, family = "binomial")
summary(m2)

# 4.2
# Often, variables that were significant in bivariate models are no longer significant in multivariate analysis 
# due to correlation between the variables. 

cor(baseball$Year,baseball$RA)
cor(baseball$Year,baseball$RankSeason)
cor(baseball$Year,baseball$NumCompetitors)

cor(baseball$RA,baseball$RankSeason)
cor(baseball$RA,baseball$NumCompetitors)

cor(baseball$RankSeason,baseball$NumCompetitors)

# 4.3

step(m2)



