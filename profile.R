# Profile the shiny app

library(shiny)
Rprof()
runApp()
Rprof(NULL)