songs = read.csv("songs.csv")
summary(songs)
songs_2010 = subset(songs, year == 2010)
str(songs_2010)

songs_michael = subset(songs, artistname == "Michael Jackson")
str(songs_michael)
songs_michael[c("songtitle", "Top10")]

table(songs$timesignature)

summary(songs$tempo)
subset(songs, tempo == max(songs$tempo))

#split the subset to training and test data
SongsTrain = subset(songs, year <= 2009 )
SongsTest = subset(songs, year == 2010)

str(SongsTrain)

#Using all attr as an independent variable
SongsLog1 = glm(Top10 ~ ., data=SongsTrain, family=binomial)

