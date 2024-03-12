batting <- read.csv('Batting.csv')

# Check batting with head()
head(batting)

# Check structure with str()
str(batting)

# head() of the first five rows of AB (At Bats) column
head(batting$AB)

# head of the doubles (X2B) column
head(batting$X2B)

# Creating Batting Average (BA)
batting$BA <- batting$H / batting$AB

# Checking last 5 entries of BA
tail(batting$BA,5)

# Creating On Base Percentage
batting$OBP <- (batting$H + batting$BB + batting$HBP)/(batting$AB + batting$BB + batting$HBP + batting$SF)

# Creating X1B (Singles)
batting$X1B <- batting$H - batting$X2B - batting$X3B - batting$HR

# Creating Slugging Average (SLG)
batting$SLG <- ((1 * batting$X1B) + (2 * batting$X2B) + (3 * batting$X3B) + (4 * batting$HR) ) / batting$AB

# Checking data frame using str()
str(batting)

# Load the Salaries.csv file into a dataframe called sal
sal <- read.csv('Salaries.csv')

# Use subset() to reassign batting from 1985 and onwards
summary(batting)
batting <- subset(batting,yearID >= 1985)

# Check with summary()
summary(batting)

# Use the merge() function to merge the batting and sal data frames 
# by c('playerID','yearID'). Call the new data frame combo
combo <- merge(batting,sal,by=c('playerID','yearID'))
summary(combo)

# Use the subset() function to get a data frame called lost_players from the 
# combo data frame consisting of those 3 players
lost_players <- subset(combo,playerID %in% c('giambja01','damonjo01','saenzol01') )
lost_players

# Use subset again to only grab the rows where the yearID was 2001.
lost_players <- subset(lost_players,yearID == 2001)

# Reduce the lost_players data frame to different columns
lost_players <- lost_players[,c('playerID','H','X2B','X3B','HR','OBP','SLG','BA','AB')]
head(lost_players)

# Grab available players from year 2001
library(dplyr)
avail.players <- filter(combo,yearID==2001)

# Quick plot to see cut-off for salary for OBP
library(ggplot2)
ggplot(avail.players,aes(x=OBP,y=salary)) + geom_point()

# Remove salary of over 8 million and OBP = 0
avail.players <- filter(avail.players,salary<8000000,OBP>0)

# Cut AB to 500
avail.players <- filter(avail.players,AB >= 500)

# Grab specific columns and sort by OBP
possible <- head(arrange(avail.players,desc(OBP)),10)
possible <- possible[,c('playerID','OBP','AB','salary')]
possible

# Select players 2, 3, and 4
possible[2:4,]
