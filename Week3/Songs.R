setwd("~/Dropbox/MIT Analytics/Week3")

#reading and summary
songs = read.csv("songs.csv")
summary(songs)
str(songs)

#Subset
MJtop = subset(songs, artistname == "Michael Jackson" & Top10 == TRUE)
table(songs$timesignature)

#Ordering

a = songs[order(songs$tempo, decreasing = TRUE),]
head(a)

#subset dataframe
SongsTrain = subset(songs, year <= 2009)
SongsTest2 = subset(songs, year >= 2010)

#exluding variable for build model
nonvars = c("year", "songtitle", "artistname", "songID", "artistID")

SongsTrain = SongsTrain[ , !(names(SongsTrain) %in% nonvars) ]
SongsTest = SongsTest[ , !(names(SongsTest) %in% nonvars) ]

#Creating glm model
Model1 = glm(Top10 ~ ., data=SongsTrain, family=binomial)
summary(Model1)

#correlation

cor(SongsTrain)

#Creating glm model ex loudness
Model2 = glm(Top10 ~ . - loudness, data=SongsTrain, family=binomial)
summary(Model2)

#Creating glm model ex energy
Model3 = glm(Top10 ~ . - energy, data=SongsTrain, family=binomial)
summary(Model3)


predictTrain = predict(Model3, data=SongsTrain, type="response")
table(SongsTrain$Top10, predictTrain > 0.45)
summary(predictTrain)

#Accuracy of TEST 
predictTest = predict(Model3, newdata=SongsTest, type="response")
table(SongsTest$Top10, predictTest > 0.45)
309/(5+309)

19/(40+19)


summary(predictTest)
(309+19)/(309+19+5+40)
#Accuracy of baseline
table(SongsTest$Top10)


19/(40+19)
314/(314+59)

  