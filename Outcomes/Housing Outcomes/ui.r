# APP: HOUSING OUTCOMES
# File: ui.r
#------------------------------------
#------------------------------------

# .libPaths("~/R/win-library/3.1")
# install.packages("devtools",repos="http://cran.rstudio.com/")
# devtools::install_github("shiny-incubator", "rstudio")
# install.packages("RJDBC",repos="http://cran.rstudio.com/")

# Load packages
library(shinyIncubator)
library(RJDBC)

# Set library paths
libPath1 <- "~/HMIS Data Analyst/lib/"
libPath2 <- "../../lib/"

#######################################################################
# Run preparatory code
#----------------------------------------------------------------------
# Establish JDBC connection using RJDBC
source(paste(libPath1,"conn-Ora-Georgia_Base.r",sep=""),local=TRUE)
# Create custom functions
source(paste(libPath2,"customFunctions.r",sep=""),local=TRUE)
#######################################################################

#######################################################################
# Specify what kind of app this by tweaking the indicators below.
# This will affect the app's appearance and/or function.
#----------------------------------------------------------------------
# Does this app analyze exits only?
exitsApp <- TRUE
# Does this app calculate returns to homelessness?
returnsApp <- TRUE
# Does this app need an "APR" option?
APR <- FALSE
# Does this app give users access to client-level data?
# c(via group report=TRUE/FALSE, via agency report=TRUE/FALSE, via program report=TRUE/FALSE)
passkey <- c(TRUE,TRUE,TRUE)
#######################################################################

shinyUI(basicPage(
  progressInit(),
  includeHTML("lib/page.html"),
  br(),
  fluidRow(
    column(4,
      tags$form(class="well noprint",
        h3("Housing Outcomes",align="center"),
        tabsetPanel(selected="Data Options",
          tabPanel("Instructions",
            p('1. FIRST select your data options.'),
            p('2. Click the "Analyze" button.'),
            p('3. When the analysis is complete, the Main Panel will update. Upon completion, you will
               also be able to download your data as a spreadsheet.'),
            p('4. If you are not satisfied with the appearance of the report, 
               you can modify the viewing options.')
          ),
          tabPanel("Data Options",
            div(actionButton("update",strong("ANALYZE"),icon=icon("arrow-circle-right")),align="right"),
            # Import "Data Options" ui code
            source(paste(libPath2,"DataOptions-Ora.ui.r",sep=""), local=TRUE)
          ),
          tabPanel("Viewing Options",
            checkboxInput("printable", "Printable", FALSE),
            checkboxInput("mdkr", 'Omit unknown destinations (missing, "Don\'t know," and "Refused")',FALSE)
          ),
          tabPanel("About",
            p(strong("Title:"),"Georgia HMIS Housing Outcomes"),
            p(strong("Version:"),"1.0.0"),
            p(strong("Date:"),"16 August 2014"),
            p(strong("Description:"),"Set of customizable reports and charts for the purpose of reporting
              and improving housing outcomes among homeless service providers in the state of Georgia"),
            p(strong("Bug reports:"),"Send to ",a(href="mailto:jason.m.rodriguez@vanderbilt.edu","jason.m.rodriguez@vanderbilt.edu")),
            p(div(strong("Source code:"),"View on",a(href="https://github.com/hmis-analyst/shiny-apps-oracle/tree/master/Outcomes","GitHub"))),
            p(strong('Changes since last version:')),
            p(' - None so far; this is the first version'),
            p(strong('Other notes:')),
            p(' - The group choices in the "GROUP" drop-down are drawn from administrator groups created in Pathways COMPASS.
              Groups can be added to the drop-down upon request.')
          )
        )
      )
    ),
    column(8,
      uiOutput("mainPanel")
    )
  )
))
