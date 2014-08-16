# APP: DATA ELEMENTS
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
APR <- TRUE
# Does this app give users access to client-level data?
# c(group level=TRUE/FALSE, agency level=TRUE/FALSE, program level=TRUE/FALSE)
passkey <- c(FALSE,FALSE,TRUE)
#######################################################################

shinyUI(basicPage(
  progressInit(),
  # Call a separate HTML file to create bars, headers, layouts
  includeHTML("lib/page.html"),
  fluidRow(
    column(4,
      tags$form(class="well noprint",
        h3("Data Elements",align="center"),
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
            checkboxInput("printable", "Printable", FALSE)
          ),
          tabPanel("About",
            p(strong("Title:"),"Georgia HMIS Data Quality: Data Elements"),
            p(strong("Version:"),"2.5.1"),
            p(strong("Date:"),"15 August 2014"),
            p(strong("Description:"),"Set of customizable reports and charts for the purpose of reporting
              and improving data quality among homeless service providers in the state of Georgia"),
            p(strong("Bug reports:"),"Send to ",a(href="mailto:jason.m.rodriguez@vanderbilt.edu","jason.m.rodriguez@vanderbilt.edu")),
            p(div(strong("Source code:"),"View on",a(href="https://github.com/hmis-analyst/shiny-apps-oracle/tree/master/Data%20Quality","GitHub"))),
            p(strong('Changes since last version:')),
            p(' - Minor functional changes')
          )
        )
      )
    ),
    column(8,
      div(style="width: 100%; height: 400px;",
        uiOutput("mainPanel")
      )
    )
  )
))
