### U3 Assignment 1-Songs
str(songs)
summary(songs)
table(songs$year)
summary(songs$Top10)
names(songs)

mj<- subset(songs, artistname == "Michael Jackson")
which(mj$Top10=="1")
mj$songtitle[18]

table(songs$timesignature)

