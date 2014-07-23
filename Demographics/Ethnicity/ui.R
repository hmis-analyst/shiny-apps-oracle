# Load shiny and RODBC packages
library(shinyIncubator)
library(RJDBC)

libPath1 <- "~/HMIS Data Analyst/lib/"
libPath2 <- "../../lib/"

# Establish JDBC connection using RJDBC
source(paste(libPath1,"conn-Ora-Georgia_Base.r",sep=""),local=TRUE)

# Define UI for HMIS Ethnicity trends application
shinyUI(basicPage(
  #Initiate progress indicators
  progressInit(),
  fluidRow(
    # SIDE PANEL
    column(4,
           wellPanel(
             h3("Ethnicity",align="center"),
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
                                  p(strong("Title:"),"Georgia HMIS Demographics - Ethnicity"),
                                  p(strong("Version:"),"1.0"),
                                  p(strong("Date:"),"23 July 2014"),
                                  p(strong("Description:"),"Set of customizable reports and charts for the purpose of reporting
              ethnicity demographic trends among homeless service providers in the state of Georgia"),
                                  p(strong("Bug reports:"),"Send to ",a(href="mailto:katherine.arce@dca.ga.gov","katherine.arce@dca.ga.gov")),
                                  p(div(strong("Source code:"),"View on",a(href="https://github.com/hmis-analyst/shiny-apps-oracle/tree/master/Demographics/Ethnicity","GitHub"))),
                                  p(strong('Changes since last version:')),
                                  p('None! This is the first version.')
                         )
             )
           )
    ),
    # MAIN PANEL
    column(8,
           # Call "Plot" (reactive plot, defined in server.R)
           plotOutput("Plot")
    )
  )
))