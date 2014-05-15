#
#  Age Demo ui.r 
#
# Load shiny and RODBC packages
library(shinyIncubator)
library(RJDBC)


libPath1 <- "~/HMIS Data Analyst/lib/"
libPath2 <- "../../lib/"

# Establish JDBC connection using RJDBC
source(paste(libPath1,"conn-Ora-Georgia_Base.r",sep=""),local=TRUE)


# Define UI for HMIS age trends application
shinyUI(basicPage(
  #Initiate progress indicators
  progressInit(),
  fluidRow(
    # SIDE PANEL
    column(4,
      wellPanel(
        h3("Age DEMO",align="center"),
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
            source(paste(libPath2,"DataOptions-Ora-DEMO.ui.r",sep=""), local=TRUE)

          ),
          tabPanel("About",
            p(strong("Title:"),"Georgia HMIS Demographics - Age DEMO"),
            p(strong("Version:"),"1.0.03"),
            p(strong("Date:"),"15 May 2014"),
            p(strong("Description:"),"Demonstration application for age demographic analysis"),
            p(strong("Bug reports:"),"Send to ",a(href="mailto:jsn.rgz@gmail.com","jsn.rgz@gmail.com")),
            p(div(strong("Source code:"),"View on",a(href="https://github.com/hmis-analyst/shiny-apps-oracle/tree/master/Demos/Age-DEMO","GitHub"))),
            p(strong('Changes since last version:')),
            p(' - Initial Version')
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