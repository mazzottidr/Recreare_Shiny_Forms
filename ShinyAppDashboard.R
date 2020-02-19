##
# Packages
##
library(shiny)
library(plotly)
library(shinydashboard)

##
# Directory with Programs
##
programdat <- "programs"

##
# Functions
##
source(paste(programdat,"functions.R", sep="/"))

##
# UI
##
source(paste(programdat,"ui.R", sep="/"))

##
# Server
##
source(paste(programdat,"server.R", sep="/"))

##
# Launch Shiny App
##
shinyApp(ui = ui, server = server)  


