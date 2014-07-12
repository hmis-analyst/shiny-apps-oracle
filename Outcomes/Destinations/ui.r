#.libPaths("~/R/win-library/3.0")
#install.packages("RJDBC",repos="http://cran.rstudio.com/")

# Load shiny and RODBC packages
library(shinyIncubator)
library(RJDBC)
library(shinyGridster)

libPath1 <- "~/HMIS Data Analyst/lib/"
libPath2 <- "../../lib/"

# Establish JDBC connection using RJDBC
source(paste(libPath1,"conn-Ora-Georgia_Base.r",sep=""),local=TRUE)

source('dashwidgets.r')

shinyUI(basicPage(
  progressInit(),
  tags$head(    
    tags$link(rel = 'stylesheet', type = 'text/css', href = 'styles.css'),
    # For JustGage, http://justgage.com/
    tags$script(src = 'js/raphael.2.1.0.min.js'),
    tags$script(src = 'js/justgage.1.0.1.min.js'),

    # For Highcharts, http://www.highcharts.com/
    tags$script(src = 'js/highcharts.js'),
    # For the Shiny output binding for status text and JustGage
    tags$script(src = 'shiny_status_binding.js'),
    tags$script(src = 'justgage_binding.js')
  ),
  br(),
  fluidRow(
    column(4,
      tags$form(class="well noprint",
        h3("Housing Destinations",align="center"),
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
            p(strong("Title:"),"Georgia HMIS Housing Destinations"),
            p(strong("Version:"),"3.1.0"),
            p(strong("Date:"),"11 July 2014"),
            p(strong("Description:"),"Set of customizable reports and charts for the purpose of reporting
              and improving program outcomes among homeless service providers in the state of Georgia"),
            p(strong("Bug reports:"),"Send to ",a(href="mailto:jason.m.rodriguez@vanderbilt.edu","jason.m.rodriguez@vanderbilt.edu")),
            p(div(strong("Source code:"),"View on",a(href="https://github.com/hmis-analyst/shiny-apps-oracle/tree/master/Outcomes","GitHub"))),
            p(strong('Changes since last version:')),
            p(' - Added Adjusted Outcome Score reporting for Prevention programs'),
            p(' - Minor aesthetic changes')
          )
        )
      )
    ),
    column(8,
      uiOutput("mainPanel")
    )
  )
))
