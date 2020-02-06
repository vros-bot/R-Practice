
NBAStats <- read.csv(file = 'nba.games.stats 3.csv') # Reading the data file
NBAStats

df <- split(NBAStats, NBAStats$Team) # splitting the data by team
df

TorontoStats <- df$TOR # Contains the data of Toronto team

df1 <- split(TorontoStats, TorontoStats$Home) # splitting data based on Home games and Away games
df1

THome <- df1$Home # contains the data of Home games
TAway <- df1$Away # contains the data of Away games
THome
TAway

THomeProp <- table(THome$WINorLOSS) # number of wins and losses at home
TAwayProp <- table(TAway$WINorLOSS) # number of wins and losses away

Proptable <- rbind(THomeProp,TAwayProp) # Constructing proportion table
TProptable

prop.test(TProptable, alternative = "less") # proportion test for the hypothesis; "losing percentage in home games is less than away games"

prop.test(c(121,94), c(164,164), alternative = "greater") # Proportion test for the hypothesis; " Winning chances are more in home games compared to the ones played away"

# Repeated for Boston team

BostonStats <- df$BOS
df2 <- split(BostonStats, BostonStats$Home)
df2
BHome <- df2$Home
BAway <- df2$Away
BHome
BAway
BHomeProp <- table(BHome$WINorLOSS)
BAwayProp <- table(BAway$WINorLOSS)
BProptable <- rbind(BHomeProp,BAwayProp)
BProptable
prop.test(BProptable, alternative = "less")
prop.test(c(106,90), c(164,164), alternative = "greater")

