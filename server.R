# Code that you put before shinyServer in the server.R function 
# gets called once when you do runApp()

#----------------------------Load and Clean Data------------------------------#
require(shiny)
require(dplyr)
require(lazyeval)
require(ggplot2)
require(scales)
require(reshape2)

datafile = "serverdatafile.RData"
if (!file.exists(datafile)) {
    require(Lahman)
    data(Master)
    data(battingLabels)
    bstats <- battingStats()
    bstats <- filter(bstats, is.element(lgID, c("AL","NL")))
    playersubset <- filter(bstats, AB >= 400) %>% (function(x) x[, 'playerID']) %>% unique
    bstats <- filter(bstats, is.element(playerID, playersubset))
    bnames <- transmute(Master, playerID=playerID, 
                        nameFull=paste(nameLast, nameFirst, sep=', '), 
                        namePlayer=paste(nameFirst, nameLast))
    bstats <- merge(bstats, bnames)
    allbatLabels <- battingLabels[c(6,8:23),]
    morestats <- as.data.frame(cbind(c('BA', 'PA', 'TB', 'SlugPct', 'OBP', 'OBS', 'BABIP'),
    					  c('Batting Average', 'Plate Appearances', 'Total Bases', 
    					    'Slugging Percentage', 'On-base Percentage', 
    					    'On-base Percentage + Slugging', 'Batting Average on Balls in Play')
    					  ))
    names(morestats) <- names(allbatLabels)
    allbatLabels <- rbind(allbatLabels, morestats)
#     bnames <- data.table(bnames)
#     bstats <- data.table(bstats)
#     allbatLabels <- data.table(allbatLabels)
    save(bstats, bnames, allbatLabels, file = datafile)
} else {
    #load datafile
    load(file = datafile)
}

#----------------------------Function Definitions-----------------------------#
topten <- function(yearinterest, stat1) {
    minAB <- filter(bstats, yearID == yearinterest) %>% select(AB) %>% quantile(probs = .6, na.rm=T)
    sortby <- interp(~desc(var), var = as.name(stat1))
    bstats %>% filter(yearID == yearinterest, AB > minAB) %>% 
	arrange_(sortby) %>% 
	select_("nameFull", stat1) %>% 
	head(10) -> toptentable
    names(toptentable) <- c("Player",allbatLabels[allbatLabels[,1]==stat1,2])
    toptentable
}
#-----------------------------------------------------------------------------#
playercareerPlot <- function(pID, varsofinterest) {
    player <- filter(bstats, playerID == pID) %>% 
	select_(.dots = c(varsofinterest, 'yearID'))
    medyr <- median(player$yearID)
    n.vars <- length(varsofinterest) 
    playermax <- data.frame(Max = apply(player, 2, max)[1:n.vars], 
			    Min = apply(player, 2, min)[1:n.vars], 
			    variable = varsofinterest, 
			    medyr = rep(medyr, n.vars))
    idx <- grep(pID, bnames$playerID)
    playerName <- bnames$namePlayer[idx]
    player.long <- melt(player, id.vars='yearID')
    p <- ggplot(player.long, aes(x = yearID, y = value))
    ## Test Theming
#     p <- p + theme(plot.background = element_rect(fill = alpha("grey20", 0.4)),
# 		   text = element_text(size = 20)
# 		   )
    p <- p + theme_minimal() + theme(text = element_text(size = 25, colour = "grey20"))
    ##
    p <- p + facet_grid(variable ~ ., scales = "free")
    p <- p + xlab("Year") + ylab("")
    p <- p + ggtitle(paste(playerName,"-- Career Statistics\n"))
    p + geom_line(aes(colour = variable), linetype = 1, size = 3) + 
	geom_hline(aes(yintercept = Max), data = playermax, 
		   linetype = 2, size = 3, colour="grey20") + 
	geom_text(aes(y = Min + (Max - Min) * 1.10, x = medyr, 
		      label=paste("Career Maximum:", Max)), 
		      colour="grey20", size=7, fontface = 2, data = playermax)
}
#-----------------------------------------------------------------------------#
playercomparePlot <- function(pID1, pID2, yearofinterest, statsofinterest) {
    cutoffAB <- filter(bstats, yearID == yearofinterest) %>% select(AB) %>% quantile(probs = .6, na.rm=T)
    maxes <- (bstats %>% filter(yearID==yearofinterest, AB > cutoffAB) %>% 
	    select(one_of(statsofinterest)) %>%
	    apply(2, max, na.rm=T))
    names(maxes) <- allbatLabels[match(statsofinterest, allbatLabels$variable),2]
    maxesDf <- data.frame(Max = maxes, variable=statsofinterest)
    pcompare <- select(filter(bstats, is.element(playerID, c(pID1,pID2)), yearID==yearofinterest), playerID, one_of(statsofinterest))
    pcompare.long <- melt(pcompare, id.vars='playerID')
    idx1 <- grep(pID1, bnames$playerID)
    idx2 <- grep(pID2, bnames$playerID)
    player1Name <- bnames$namePlayer[idx1]
    player2Name <- bnames$namePlayer[idx2]
    p <- ggplot(pcompare.long, aes(x=variable, y=value))
    p <- p + ggtitle(paste(player1Name, "vs.", player2Name,
                           "\n",yearofinterest,"Season Comparison\n"))
    ## Test Theming
#     p <- p + theme(plot.background = element_rect(fill = alpha("grey20", 0.4)),
# 		   text = element_text(size = 20)
# 		   )
    p <- p + theme_minimal() + theme(text = element_text(size = 25, colour = "grey20"))
    ##
    p <- p + facet_wrap(~ variable, nrow = 1, scales = "free") + 
	xlab('Compared Statistics') + ylab(paste('Statistics from', yearofinterest, 'Season'))
    p + geom_bar(stat="identity", position = "dodge", aes(fill=playerID)) +
#       scale_fill_discrete(name = "Player Name", 
#                           breaks = c(idx1, idx2), 
#                           labels = c(player1Name, player2Name)) +
#       labs(fill = "Player Name") +
	geom_hline(aes(yintercept = Max), data = maxesDf, size = 3, colour="grey20") +
	geom_text(aes(y = Max * 1.05, label=paste("League Most:", Max)), 
	          colour="grey20", size=7, fontface=2, data = maxesDf)
}
#-----------------------------------------------------------------------------#
fn_playeryears <- function(pID) {
  bstats %>% select(one_of('yearID', 'playerID')) %>%
  filter(playerID == pID) %>% (function(x) x[, 'yearID'])
}
#-----------------------------------------------------------------------------#
fn_playedsametime <- function(pID) {
  bnamesNyrs <- bstats %>% select(one_of('yearID', 'playerID', 'nameFull'))
  years <- bnamesNyrs %>% filter(playerID == pID) %>% (function(x) x[, 'yearID'])
  playedsametime <- bnamesNyrs %>% filter(is.element(yearID, years)) %>% 
    ( function(y) y[, c('playerID', 'nameFull')] ) %>% unique 
  fullnames <- playedsametime$nameFull
  playedsametime <- playedsametime$playerID
  names(playedsametime) <- fullnames
  playedsametime
}
#-----------------------------------------------------------------------------#

shinyServer(
  #Code inside the unnamed function of shinyServer(function(input, output){ but not in a
  #reactive statement will run once for every new user (or page refresh)
  function(input, output, session) {
    # player career stats
    output$playcareergraph <- renderPlot({
      if (input$goCareer) {
      isolate({
  	    playercareerPlot(input$playercareer, input$statinterest1)
        })
      } else {NULL}
	    }, bg="transparent")
    # AdaptiveUI
    observe({ updateNumericInput(session, "yearbestof", value = input$yearcomp) })
    observe({ updateSelectInput(session, "playercomp1", selected = input$playercareer) })
    observe({ updateSelectInput(session, "playercareer", selected = input$playercomp1) })
    observe({ 
      updateSelectInput(session, "playercomp2", 
                        choices = fn_playedsametime(input$playercomp1)
                        ) 
      })
    compareyrscommon <- reactive({
      p1yrs <- fn_playeryears(input$playercomp1)
      p2yrs <- fn_playeryears(input$playercomp2)
#       ## TODO fix
#       print(paste("player1:",input$playercomp1, sep=' '))
#       print(paste("player2:",input$playercomp2, sep=' '))
#       print(sort(intersect(p1yrs, p2yrs)))
#       ## TODO fix
      sort(intersect(p1yrs, p2yrs))
      })
    observe({
      yrs <- compareyrscommon()
      updateSelectInput(
        session, "yearcomp", choices = yrs
        # choices = ifelse(length(yrs) > 0, yrs, 1999)
                        )
      })
    # top ten tab i/o
    output$topten1 <- renderTable ({
      if (input$goTopten) {
      isolate({
        topten(input$yearbestof,input$topstat1)
        })
      } else {NULL}
      })
    output$topten2 <- renderTable ({
      if (input$goTopten) {
      isolate({
        topten(input$yearbestof,input$topstat2)
        })
      } else {NULL}
      })
    output$topten3 <- renderTable ({
      if (input$goTopten) {
      isolate({
        topten(input$yearbestof,input$topstat3)
        })
      } else {NULL}
      })
    output$topten4 <- renderTable ({
      if (input$goTopten) {
      isolate({
        topten(input$yearbestof,input$topstat4)
        })
      } else {NULL}
      })
#     output$topten3 <- renderTable ({topten(input$yearbestof,input$topstat3)})
#     output$topten4 <- renderTable ({topten(input$yearbestof,input$topstat4)})
    # player comparison tab i/o
    output$compareplot <- renderPlot({
      if (input$goCompare) {
      isolate({
        playercomparePlot(input$playercomp1, 
          input$playercomp2, input$yearcomp, input$statinterest2)
        })
      } else {NULL}
    	}, bg="transparent")
#     output$comparableyears <- renderPrint({  
#       ifelse(length(compareyrscommon())>0, 
#              "Select a year to compare batting statistics:", 
#              "No common seasons between these 2 players, select another second player to compare!"
#              )
#         })
    # Code in reactive functions of shinyServer get run repeatedly as needed when new values are
    # entered (reactive functions are those like render* )
  }
)
