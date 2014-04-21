# Load shiny and RODBC packages
library(shiny)
library(RJDBC)


# Establish ODBC connection using RJDBC
drv <- JDBC("oracle.jdbc.OracleDriver",classPath="../../lib/ojdbc6.jar", " ")
source("~/HMIS Data Analyst/lib/connectionkey.r",local=TRUE)

# Define UI for HMIS gender trends application
shinyUI(basicPage(
  fluidRow(
    column(4,
      tags$form(class="well noprint",
        h3("Gender",align="center"),
        tabsetPanel(selected="Data Options",
          tabPanel("Instructions",
            p('1. FIRST select your data options.'),
            p('2. Click the "Analyze" button.'),
            p('3. When the analysis is complete, the Main Panel will update.')
          ),
          tabPanel("Data Options",
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
    column(8,
      div(style="width: 100%; height: 400px;",
        # Call "Plot" (reactive plot, defined in server.R)
        # Display on main panel
        plotOutput("Plot")
      )	
    )
  )
))