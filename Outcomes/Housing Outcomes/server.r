# APP: HOUSING OUTCOMES
# File: server.r
#------------------------------------
#------------------------------------

# .libPaths("~/R/win-library/3.0")
# install.packages("RJDBC",repos="http://cran.rstudio.com/")
# install.packages("ggplot2",repos="http://cran.rstudio.com/")
# install.packages("reshape2",repos="http://cran.rstudio.com/")
# install.packages("stringr",repos="http://cran.rstudio.com/")
# devtools::install_github("shiny-gridster", "wch")

# Load packages
library(shinyIncubator)
library(RJDBC)
library(ggplot2)
library(reshape2)
library(stringr)
library(plyr)
suppressPackageStartupMessages(library(googleVis))

# Set library paths
libPath1 <- "~/HMIS Data Analyst/lib/"
libPath2 <- "../../lib/"
libPath3 <- "lib/"

#######################################################################
# Run preparatory code
#----------------------------------------------------------------------
# Establish JDBC connection using RJDBC
source(paste(libPath1,"conn-Ora-Georgia_Base.r",sep=""),local=TRUE)
# Create custom functions
source(paste(libPath2,"customFunctions.r",sep=""),local=TRUE)
#######################################################################

# Define server logic required to query/graph HMIS gender data
shinyServer(function(input, output, session) {
  
  # Run various server.r segments
  source(paste(libPath2,"DataOptions-Ora.server.r",sep=""), local=TRUE)
  source("errors.r",local=TRUE)
  source("destinationsData.r",local=TRUE)
  source("returnsData.r",local=TRUE)
  source("outcomeScore.r",local=TRUE)
  source("tables.r",local=TRUE)
  source("plots.r",local=TRUE)
  source("downloads.r",local=TRUE)
  source("mainPanel.r",local=TRUE)
  
})
