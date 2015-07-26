# Rmarkdown: Baseball App ideas.
# -------------------------------------------------------------------------- 

# Documentation Examples:
# -------------------------------------------------------------------------- 
# https://rajuvarghese.shinyapps.io/tiapp/
	# See: Data Source, Help, About

# Top10 Statistics by Year:
# -------------------------------------------------------------------------- 
#----------------------------Data Normalization-------------------------------#
require('Lahman')
require('dplyr')
require('lazyeval')
data(Batting)
data(Master)
bnames <- Master[ , c('playerID','nameLast','nameFirst','nameGiven', 'birthYear')]
bnames$nameLong <- paste0(bnames$nameLast, ', ', bnames$nameFirst, 
			  ' (b:', bnames$birthYear, ')')
bstats <- battingStats()
bstats <- bstats[is.element(bstats$lgID, c('AL', 'NL')),]
bstats <- merge(bstats, bnames)
bstats2 <- select(bstats, -c(nameLast,nameFirst, birthYear, G_old))
#--------------------------------Variables------------------------------------#
yearinterest <- 1985
stat1 <- "SB"
#------------------------------Function Body----------------------------------#
testfun <- function(yearinterest, stat1) {
    cutoffAB <- filter(bstats2, yearID == yearinterest) %>% select(AB) %>% quantile(probs = .6, na.rm=T)
    sortby <- interp(~desc(var), var = as.name(stat1))
    bstats2 %>% filter(yearID == yearinterest, AB > cutoffAB) %>% 
	arrange_(sortby) %>% 
	select_("nameLong", stat1) %>% 
	head(10)
}
testfun(1971, "H")
#                   nameLong   H
# 1      Torre, Joe (b:1940) 230
# 2     Garr, Ralph (b:1945) 219
# 3    Tovar, Cesar (b:1940) 204
# 4      Brock, Lou (b:1939) 200
# 5   Davis, Willie (b:1940) 198
# 6     Alou, Matty (b:1938) 192
# 7      Rose, Pete (b:1941) 192
# 8    Staub, Rusty (b:1944) 186
# 9  Beckert, Glenn (b:1940) 181
# 10  Alomar, Sandy (b:1943) 179

bstats2 %>% filter(yearID == yearinterest, AB > cutoffAB) %>% arrange(desc(HR)) %>% head(10)
bstats2 %>% filter(yearID == yearinterest, AB > cutoffAB) %>% arrange(desc(BB)) %>% head(10)
bstats2 %>% filter(yearID == yearinterest, AB > cutoffAB) %>% arrange(desc(SO)) %>% head(10)
bstats2 %>% filter(yearID == yearinterest, AB > cutoffAB) %>% arrange(desc(BA)) %>% head(10)
bstats2 %>% filter(yearID == yearinterest, AB > cutoffAB) %>% arrange(desc(SlugPct)) %>% head(10)
bstats2 %>% filter(yearID == yearinterest, AB > cutoffAB) %>% arrange(desc(SB)) %>% head(10)

# Player Career Arc:
# -------------------------------------------------------------------------- 
#----------------------------Data Normalization-------------------------------#
require('Lahman')
require('dplyr')
require('ggplot2')
require('reshape2')

data(Batting)
data(Master)
bnames <- Master[ , c('playerID','nameLast','nameFirst','nameGiven', 'birthYear')]
bnames$nameFull <- paste(bnames$nameLast, bnames$nameFirst, sep=', ') 
#bnames$nameLong <- paste0(bnames$nameLast, ', ', bnames$nameFirst, 
			  #' (b:', bnames$birthYear, ')')
bstats <- battingStats()
bstats <- bstats[is.element(bstats$lgID, c('AL', 'NL')),]
bstats <- merge(bstats, bnames)
bstats <- select(bstats, -c(nameLast,nameFirst, birthYear, G_old))
#--------------------------------Variables------------------------------------#
pID <- "aaronha01"
pID <- "bondsba01"
pID <- "ruthba01"
pID <- "cansejo01"
pID <- "rosepe01"
vars00 <- c("H", "SB", "HR", "SlugPct", "BA")
varsofinterest <- c("H", "SB", "HR", "SlugPct", "BA")
#------------------------------Function Body----------------------------------#
myplayerplot <- function(pID, varsofinterest) {
    player <- filter(bstats, playerID == pID) %>% 
	select_(.dots = c(varsofinterest, 'yearID'))
    #------------------------------------------------------------------
    medyr <- median(player$yearID)
    n.vars <- length(varsofinterest) 
    #playermax <- apply(player, 2, max)                                                                                  
    playermax <- data.frame(Max = apply(player, 2, max)[1:n.vars], 
			    Min = apply(player, 2, min)[1:n.vars], 
			    variable = varsofinterest, 
			    medyr = rep(medyr, n.vars))
    idx <- grep(pID, Master$playerID)
    playerName <- paste(Master$nameFirst, Master$nameLast)[idx]
    print(playerName)
    #------------------------------------------------------------------
    player.long <- melt(player, id.vars='yearID')
    p <- ggplot(player.long, aes(x = yearID, y = value))
    p <- p + facet_grid(variable ~ ., scales = "free")
    p <- p + xlab("Year") + ylab("")
    p <- p + ggtitle(paste(playerName,"-- Career Statistics"))
    p + geom_line(aes(colour = variable), linetype = 1, size = 3) + 
	geom_hline(aes(yintercept = Max), data = playermax) + 
	geom_text(aes(y = Min + (Max - Min) * 1.10, x = medyr, 
		      label=paste("Career Maximum:", Max)), data = playermax)
}
myplayerplot("aaronha01", c("H", "SB", "HR", "SlugPct", "BA"))


# Player Comparisons:
# -------------------------------------------------------------------------- 
#----------------------------Data Normalization-------------------------------#
require('Lahman')
require('dplyr')
require('ggplot2')
require('reshape2')

data(Batting)
data(battingLabels)
data(Master)

bnames <- Master[ , c('playerID','nameLast','nameFirst','nameGiven', 'birthYear')]
bnames$nameLong <- paste0(bnames$nameLast, ', ', bnames$nameFirst, 
		  ' (b:', bnames$birthYear, ')')
bstats <- battingStats()
bstats <- bstats[is.element(bstats$lgID, c('AL', 'NL')),]
bstats <- merge(bstats, bnames)
bstats <- select(bstats, -c(nameLast,nameFirst, birthYear, G_old))
allbatLabels <- battingLabels[c(6,8:23),]
morestats <- as.data.frame(cbind(c('BA', 'PA', 'TB', 'SlugPct', 'OBP', 'OBS', 'BABIP'),
					  c('Batting Average', 'Plate Appearances', 'Total Bases', 
					    'Slugging Percentage', 'On-base Percentage', 
					    'On-base Percentage + Slugging', 'Batting Average on Balls in Play')
					  ))
names(morestats) <- names(allbatLabels)
allbatLabels <- rbind(allbatLabels, morestats)
#--------------------------------Variables------------------------------------#
statsofinterest <- c('H', 'SO', 'HR', 'BA') #, 'SlugPct', 'SO', 'BB')
yearofinterest <- 1996
#pID <- "aaronha01"
#pID <- "ruthba01"
#pID <- "rosepe01"
pID1 <- "bondsba01"
pID2 <- "cansejo01"
#------------------------------Function Body----------------------------------#
plotcompare <- function(pID1, pID2, yearofinterest, statsofinterest) {
    cutoffAB <- filter(bstats, yearID == yearofinterest) %>% select(AB) %>% quantile(probs = .6, na.rm=T)
    maxes <- (bstats %>% filter(yearID==yearofinterest, AB > cutoffAB) %>% 
	    select(one_of(statsofinterest)) %>%
	    apply(2, max, na.rm=T))
    names(maxes) <- allbatLabels[match(statsofinterest, allbatLabels$variable),2]
    maxesDf <- data.frame(Max = maxes, variable=statsofinterest)
    pcompare <- select(filter(bstats, is.element(playerID, c(pID1,pID2)), yearID==yearofinterest), playerID, one_of(statsofinterest))
    pcompare.long <- melt(pcompare, id.vars='playerID')
    p <- ggplot(pcompare.long, aes(x=variable, y=value))
    p <- p + facet_wrap(~ variable, nrow = 1, scales = "free") + 
	xlab('Compared Statistics') + ylab(paste('Statistics from', yearofinterest, 'Season'))
    p + geom_bar(stat="identity", position = "dodge", aes(fill=playerID)) +
	geom_hline(aes(yintercept = Max), data = maxesDf) +
	geom_text(aes(y = Max * 1.03, label=paste("league best:", Max)), data = maxesDf)
}
plotcompare("bondsba01", "cansejo01", 1990, c('H', 'HR', 'SlugPct', 'BA', 'OBP'))


# Verify Calculations:
# -------------------------------------------------------------------------- 
require('Lahman')
data(Batting)
bstats <- battingStats()

BA <- round(Batting$H / Batting$AB,3)
TB <- round(Batting$H + 1*Batting$X2B + 2*Batting$X3B + 3*Batting$HR, 3)
TB[is.na(TB)] <- 0
identical(TB, bstats[,'TB'])
# [1] TRUE

SlugPct <- round(TB / Batting$AB, 3)
SlugPct[is.na(SlugPct)] <- NA
identical(SlugPct, bstats[,'SlugPct'])
# [1] TRUE

sumNaAsZero <- function(...) {
	mat <- cbind(...)
	rowSums(mat, na.rm=T)
}

PA <- as.numeric(sumNaAsZero(Batting$AB, Batting$BB, Batting$HBP, 
			     Batting$SH, Batting$SF))
identical(PA, bstats[,'PA'])
# [1] TRUE

OBP <- as.numeric(sumNaAsZero(Batting$H, Batting$BB, Batting$HBP))
OBPattempts <- as.numeric(sumNaAsZero(Batting$AB, Batting$BB, Batting$HBP,
				      Batting$SF))
OBP <- round(OBP / OBPattempts, 3)
OBP[is.na(OBP)] <- NA
bstats[is.na(bstats$OBP),'OBP'] <- NA
identical(OBP, bstats[,'OBP'])
# [1] TRUE

# Exploring Data:
# -------------------------------------------------------------------------- 
max(Batting$yearID[Batting$lgID == 'NA'])
# [1] 1875
max(Batting$yearID[Batting$lgID == 'UA'])
# [1] 1884
max(Batting$yearID[Batting$lgID == 'PL'])
# [1] 1890
max(Batting$yearID[Batting$lgID == 'AA'])
# [1] 1891
max(Batting$yearID[Batting$lgID == 'FL'])
# [1] 1915

min(Batting$yearID[Batting$lgID == 'AL'])
# [1] 1901
min(Batting$yearID[Batting$lgID == 'NL'])
# [1] 1876

data(Master)
Master[Master$nameLast == 'Ruth',]
#       playerID birthYear birthMonth birthDay birthCountry birthState birthCity deathYear deathMonth deathDay deathCountry deathState deathCity nameFirst nameLast     nameGiven
# 14413 ruthba01      1895          2        6          USA         MD Baltimore      1948          8       16          USA         NY  New York      Babe     Ruth George Herman
#       weight height bats throws      debut  finalGame  retroID  bbrefID  deathDate  birthDate
# 14413    215     74    L      L 1914-07-11 1935-05-30 ruthb101 ruthba01 1948-08-16 1895-02-06

Batting[Batting$playerID == 'ruthba01',]
#       playerID yearID stint teamID lgID   G G_batting  AB   R   H X2B X3B HR RBI SB CS  BB SO IBB HBP SH SF GIDP G_old
# 75896 ruthba01   1914     1    BOS   AL   5         5  10   1   2   1   0  0   2  0 NA   0  4  NA   0  0 NA   NA     5
# 75897 ruthba01   1915     1    BOS   AL  42        42  92  16  29  10   1  4  21  0 NA   9 23  NA   0  2 NA   NA    42
# 75898 ruthba01   1916     1    BOS   AL  67        67 136  18  37   5   3  3  15  0 NA  10 23  NA   0  4 NA   NA    67
# 75899 ruthba01   1917     1    BOS   AL  52        52 123  14  40   6   3  2  12  0 NA  12 18  NA   0  7 NA   NA    52
# 75900 ruthba01   1918     1    BOS   AL  95        95 317  50  95  26  11 11  66  6 NA  58 58  NA   2  3 NA   NA    95
# 75901 ruthba01   1919     1    BOS   AL 130       130 432 103 139  34  12 29 114  7 NA 101 58  NA   6  3 NA   NA   130
# 75902 ruthba01   1920     1    NYA   AL 142       142 457 158 172  36   9 54 137 14 14 150 80  NA   3  5 NA   NA   142
# 75903 ruthba01   1921     1    NYA   AL 152       152 540 177 204  44  16 59 171 17 13 145 81  NA   4  4 NA   NA   152
# 75904 ruthba01   1922     1    NYA   AL 110       110 406  94 128  24   8 35  99  2  5  84 80  NA   1  4 NA   NA   110
# 75905 ruthba01   1923     1    NYA   AL 152       152 522 151 205  45  13 41 131 17 21 170 93  NA   4  3 NA   NA   152
# 75906 ruthba01   1924     1    NYA   AL 153       153 529 143 200  39   7 46 121  9 13 142 81  NA   4  6 NA   NA   153
# 75907 ruthba01   1925     1    NYA   AL  98        98 359  61 104  12   2 25  66  2  4  59 68  NA   2  6 NA   NA    98
# 75908 ruthba01   1926     1    NYA   AL 152       152 495 139 184  30   5 47 150 11  9 144 76  NA   3 10 NA   NA   152
# 75909 ruthba01   1927     1    NYA   AL 151       151 540 158 192  29   8 60 164  7  6 137 89  NA   0 14 NA   NA   151
# 75910 ruthba01   1928     1    NYA   AL 154       154 536 163 173  29   8 54 142  4  5 137 87  NA   3  8 NA   NA   154
# 75911 ruthba01   1929     1    NYA   AL 135       135 499 121 172  26   6 46 154  5  3  72 60  NA   3 13 NA   NA   135
# 75912 ruthba01   1930     1    NYA   AL 145       145 518 150 186  28   9 49 153 10 10 136 61  NA   1 21 NA   NA   145
# 75913 ruthba01   1931     1    NYA   AL 145       145 534 149 199  31   3 46 163  5  4 128 51  NA   1  0 NA   NA   145
# 75914 ruthba01   1932     1    NYA   AL 133       133 457 120 156  13   5 41 137  2  2 130 62  NA   2  0 NA   NA   133
# 75915 ruthba01   1933     1    NYA   AL 137       137 459  97 138  21   3 34 103  4  5 114 90  NA   2  0 NA   NA   137
# 75916 ruthba01   1934     1    NYA   AL 125       125 365  78 105  17   4 22  84  1  3 104 63  NA   2  0 NA   NA   125
# 75917 ruthba01   1935     1    BSN   NL  28        28  72  13  13   0   0  6  12  0 NA  20 24  NA   0  0 NA    2    28

# -------------------------------------------------------------------------- 

# Layout:
# -------------------------------------------------------------------------- 
# Examples:
	# See https://github.com/RajuVarghese/DevDataProducts/
	# for setting up tabs...
	# also for select (type and dropdown), radio, and slider input types

	# See https://github.com/geojsg/winequality
	# Tabs on whole page, different UI elements each tab

# Plotting:
# -------------------------------------------------------------------------- 
# Examples:
	# See StatInfer peer3 for custom plotting formulas using base plotting

# tips:
# -------------------------------------------------------------------------- 
# runApp(display.mode='showcase')  --- this will show files in app dir on page
## as page executes those lines of code will highlight yellow

# References:
# -------------------------------------------------------------------------- 
# https://baseballwithr.wordpress.com/2015/04/11/graphing-hr-totals/
# http://shiny.rstudio.com/tutorial/
