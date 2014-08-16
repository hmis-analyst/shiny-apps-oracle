# APP: VETERANS
#------------------------------------
#------------------------------------

# Load shiny and RODBC packages
library(shinyIncubator)
library(RJDBC)

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
exitsApp <- FALSE
# Does this app calculate returns to homelessness?
returnsApp <- FALSE
# Does this app need an "APR" option?
APR <- FALSE
# c(group level=TRUE/FALSE, agency level=TRUE/FALSE, program level=TRUE/FALSE)
passkey <- c(FALSE,FALSE,FALSE)
#######################################################################

# Define UI for HMIS Veteran trends application
shinyUI(basicPage(
  #Initiate progress indicators
  progressInit(),
  fluidRow(
    # SIDE PANEL
    column(4,
      wellPanel(
        h3("Veteran",align="center"),
        # Tab panels within side panel
        tabsetPanel(selected="Data Options",
          tabPanel("Instructions",
            p('1. FIRST select your data options.'),
            p('2. Click the "Analyze" button.'),
            p('3. When the analysis is complete, the Main Panel will update.')
          ),
          tabPanel("Data Options",
            # Require user to click "ANALYZE" button in order for graph to update
            div(actionButton("update",strong("ANALYZE"),icon=icon("arrow-circle-right")),align="right"),
            # Import "Data Options" ui code
            source(paste(libPath2,"DataOptions-Ora.ui.r",sep=""), local=TRUE)
          ),
          tabPanel("About",
            p(strong("Title:"),"Georgia HMIS Demographics - Veteran"),
            p(strong("Version:"),"1.1.0"),
            p(strong("Date:"),"06 Aug 2014"),
            p(strong("Description:"),"Set of customizable reports and charts for the purpose of reporting
              gender demographic trends among homeless service providers in the state of Georgia"),
            p(strong("Bug reports:"),"Send to ",a(href="mailto:sara.demas@pcni.org","sara.demas@pcni.org")),
            p(div(strong("Source code:"),"View on",a(href="https://github.com/hmis-analyst/shiny-apps-oracle/tree/master/Demographics/Veterans","GitHub"))),
            p(strong('Changes since last version:')),
            p('- Data options panel simplified'),
            p('- App files reorganized')
          )
        )
      )
    ),
    # MAIN PANEL
    column(8,
      # Call "Plot" (reactive plot, defined in server.R)
      uiOutput("mainPanel")
    )
  )
))
