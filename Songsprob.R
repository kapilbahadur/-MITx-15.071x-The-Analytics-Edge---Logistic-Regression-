##Homework Assignment #Unit 3 Logistic Regresssion
#Music Problem

#Loading DataSets
songs <- read.csv("songs.csv")

str(songs)

#No. of rows in songs in year 2010
nrow(songs[songs$year == 2010, ])

table(songs$artistname)

#Micheal Jackson songs
MichaelJackson <- subset(songs, artistname == "Michael Jackson")

#Songs of MJ making it to TOP 10
m1 <- MichaelJackson$songtitle[MichaelJackson$Top10 == 1]
m1

table(songs$timesignature)
max(songs$tempo)

songs$songtitle[songs$tempo == 244.307]

#Splitting into training and testing datasets
SongsTest <- subset(songs, year == 2010)
SongsTrain <- subset(songs, year != 2010)

#Removing Variable we don't want in model
nonvars <- c("year", "songtitle", "songID", "artistID", "artistname")
names(SongsTrain)
SongsTrain <- SongsTrain[,!(names(SongsTrain)%in%nonvars)]
SongsTest <- SongsTest[,!(names(SongsTest)%in%nonvars)]

SongsLog1 <- glm(Top10 ~., data = SongsTrain, family = "binomial")
summary(SongsLog1)

#MUlticollinearity
cor(SongsTrain$loudness, SongsTrain$energy)

#Creatin 2nd Model

SongsLog2 <- glm(Top10 ~ .-loudness, data = SongsTrain, family = "binomial")
summary(SongsLog2)

#Creating 3rd Model
SongsLog3 <- glm(Top10 ~.-energy, data = SongsTrain, family = "binomial")
summary(SongsLog3)

#Making Predictions
SongsPredict <- predict(SongsLog3, newdata = SongsTest, type = "response")

table(SongsTest$Top10, SongsPredict >= 0.45)
328/(328+45) #Accuracy of model 3

table(SongsTest$Top10)

