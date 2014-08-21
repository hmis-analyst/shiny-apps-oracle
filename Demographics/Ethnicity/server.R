# APP: ETHNICITY
#------------------------------------
#------------------------------------

# Load shiny, RODBC, and ggplot2 packages
library(shinyIncubator)
library(RJDBC)
library(ggplot2)
library(stringr)
library(shiny)

libPath1 <- "~/HMIS Data Analyst/lib/"
libPath2 <- "../../lib/"

# Establish JDBC connection using RJDBC
source(paste(libPath1,"conn-Ora-Georgia_Base.r",sep=""),local=TRUE)

# Define server logic required to query/graph HMIS ethnicity data
shinyServer(function(input, output, session) {

  # Import Data Options server code
  source(paste(libPath2,"DataOptions-Ora.server.r",sep=""), local=TRUE)
  source("data.R",local=TRUE)
  source("plots.R",local=TRUE)
  source("mainPanel.R",local=TRUE)

})