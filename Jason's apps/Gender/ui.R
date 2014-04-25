# Load shiny and RODBC packages
library(shinyIncubator)
library(RJDBC)


# Establish JDBC connection using RJDBC
drv <- JDBC("oracle.jdbc.OracleDriver",classPath="~/GitHub/shiny-apps-oracle/lib/ojdbc6.jar", " ")
source("~/HMIS Data Analyst/lib/connectionkey.r",local=TRUE)

# Define UI for HMIS gender trends application
shinyUI(basicPage(
  #Initiate progress indicators
  progressInit(),
  fluidRow(
    # SIDE PANEL
    column(4,
      wellPanel(
        h3("Gender (Jason's version)",align="center"),
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
            source("../../lib/Data Options.ui.r",local=TRUE)
          ),
          tabPanel("About",
            p(strong("Title:"),"Georgia HMIS Demographics - Gender"),
            p(strong("Version:"),"2.0.0"),
            p(strong("Date:"),"21 April 2014"),
            p(strong("Description:"),"Set of customizable reports and charts for the purpose of reporting
              gender demographic trends among homeless service providers in the state of Georgia"),
            p(strong("Bug reports:"),"Send to ",a(href="mailto:jsn.rgz@gmail.com","jsn.rgz@gmail.com")),
            p(div(strong("Source code:"),"View on",a(href="https://github.com/hmis-analyst/shiny-apps-oracle/tree/master/Demographics/Gender","GitHub"))),
            p(strong('Changes since last version:')),
            p(' - Upgraded to new page layout and data options')
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
