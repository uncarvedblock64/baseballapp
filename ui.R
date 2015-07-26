# Libraries needed for UI
#-----------------------------------------------------------------------------#
require(shiny)
require(dplyr)
require('markdown')

# Data needed for selecting inputs
#-----------------------------------------------------------------------------#
datafile = "inputdatafile.RData"
if (!file.exists(datafile)) {
    require(Lahman)
    data(Batting)
    playersubset <- Batting %>% filter(AB >= 400, is.element(lgID, c('AL','NL'))) %>% (function(x) x[,1]) %>% unique()
    data(Master)
    Master <- with(Master, Master[is.element(playerID,playersubset),])
    players <- Master$playerID
    names(players) <- paste(Master$nameLast, Master$nameFirst, sep=', ')
    # batLabels is the list of stats abreviated and full name
    data(battingLabels)
    batLabels <- battingLabels[c(6,8:23),1]
    names(batLabels) <- battingLabels[c(6,8:23),2]
    batLabels <- c(batLabels, 'BA', 'PA', 'TB', 'SlugPct', 'OBP', 'OBS', 'BABIP')
    names(batLabels)[length(batLabels) - (6:0)] <- 
        c('Batting Average', 'Plate Appearances', 'Total Bases', 
          'Slugging Percentage', 'On-base Percentage', 
          'On-base Percentage + Slugging', 'Batting Average on Balls in Play')
    save(batLabels, players, file = datafile)
} else {
    #load datafile
    load(file = datafile)
}
startyear <- 1993

#-----------------------------------------------------------------------------#
# Shiny UI Itself:
#-----------------------------------------------------------------------------#
shinyUI(
    navbarPage("Professional Baseball Offensive Statistics Visualizations", 
               theme="slate2.css",
               position="static-top", 
               inverse=T,
 	       tabPanel("Instructions", 
               includeMarkdown ("instructions.md")
           ),
	       tabPanel("Player Career Stats",
                        column(3,
			       fluidRow(
				   	selectInput('playercareer',
						       "Select a player:",
						       choices=players,
						       selected="bondsba01"
						       )
             ),
			       fluidRow(
				   	   actionButton('goCareer',
						       "Generate Graphs!"
						       )
             ),
			       fluidRow(
				       checkboxGroupInput("statinterest1", 
							  "Statistics to Compare:",
							  batLabels,
							  selected = c("H", "HR", "BA", "OBP")
							  )
				       )
                               ),
			column(9,
#				h4("caption of plot", align = "center"),
				plotOutput("playcareergraph", height = "750px")
			       )
			),
               tabPanel("League Leaders",
                        h2("Top Ten Leaders for Selected Offensive Statistics."),
                        fluidRow(
				p("Select statistics by typing in the dialog or clicking 
				  the arrow and selecting from the dropdown menu.")
                            ),
                        fluidRow(
                          column(3,
				                   numericInput("yearbestof", 
					                   "Top Ten's of Year:",
					                   value = startyear,
					                   min = 1876,
                             max = 2013,
					                   step = 1
					                   )
                          ),
                          column(9,
				   	                actionButton('goTopten',
						                "Generate Tables!"
						                  )
                          )
                            ),
                        fluidRow(
                            column(3, 
				   selectInput('topstat1',
					       "Statistic 1:",
					       choices=batLabels,
					       selected="H"
					       )
				   ),
                            column(3, 
				   selectInput('topstat2',
					       "Statistic 2:",
					       choices=batLabels,
					       selected="HR"
					       )
				   ),
                            column(3, 
				   selectInput('topstat3',
					       "Statistic 3:",
					       choices=batLabels,
					       selected="BA"
					       )
				   ),
                            column(3, 
				   selectInput('topstat4',
					       "Statistic 4:",
					       choices=batLabels,
					       selected="SlugPct"
					       )
				   )
                            ),
                        fluidRow(
                            column(3, tableOutput("topten1")),
                            column(3, tableOutput("topten2")),
                            column(3, tableOutput("topten3")),
                            column(3, tableOutput("topten4"))
                            )
                        ),
               tabPanel("Player Comparison",
                        column(3,
                               checkboxGroupInput("statinterest2", 
                                                  "Statistics to Compare:",
                                                  batLabels,
                                                  selected = c("H", "HR", "BA", "OBP")
                                                  )
                               ),
                        column(9,
                               fluidRow(
                                   column(4,
					  fluidRow(
						   selectInput('playercomp1',
							       "First Player to Compare",
							       choices=players,
							       selected="bondsba01"
							       )
						   ),
					  fluidRow(
						   selectInput('playercomp2',
							       "Second Player to Compare",
							       choices=players,
							       selected="bondsba01"
							       )
						   )
                                          ),
                                   column(8,
                                          fluidRow(
                                            selectInput('yearcomp',
                                                        'Season to Compare Stats',
                                                        choices=1876:2013,
                                    						        selected=startyear)
                                          ),
                                          fluidRow(
                                            actionButton('goCompare',
                                            "Generate Graphs!")
                                          )
                                          )
                                   ),
                               fluidRow(
#					h4("caption of plot", align = "center"),
					plotOutput("compareplot", height = "500px")
                                   )
                               )
                        ),
 	       tabPanel("About", 
 			 h1("Information on the Data Used in this Application"),
               includeMarkdown ("about.md"))
               )
)
