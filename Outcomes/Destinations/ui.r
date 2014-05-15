#.libPaths("~/R/win-library/3.0")
#install.packages("RJDBC",repos="http://cran.rstudio.com/")

# Load shiny and RODBC packages
library(shinyIncubator)
library(RJDBC)

libPath1 <- "~/HMIS Data Analyst/lib/"
libPath2 <- "../../lib/"

# Establish JDBC connection using RJDBC
source(paste(libPath1,"conn-Ora-Georgia_Base.r",sep=""),local=TRUE)

shinyUI(basicPage(
  progressInit(),
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
            source(paste(libPath2,"DataOptions-Ora-DEMO.ui.r",sep=""), local=TRUE)
          ),
          tabPanel("Viewing Options",
            checkboxInput("printable", "Printable", FALSE),
            checkboxInput("mdkr", 'Omit unknown destinations (missing, "Don\'t know," and "Refused")',FALSE)
          ),
          tabPanel("About",
            p(strong("Title:"),"Georgia HMIS Housing Destinations"),
            p(strong("Version:"),"2.1.0"),
            p(strong("Date:"),"9 April 2014"),
            p(strong("Author:"),"Jason Rodriguez (Georgia Department of Community Affairs)"),
            p(strong("Description:"),"Set of customizable reports and charts for the purpose of reporting
              and improving program outcomes among homeless service providers in the state of Georgia"),
            p(strong("Bug reports:"),"Send to ",a(href="mailto:jason.rodriguez@dca.ga.gov","jason.rodriguez@dca.ga.gov")),
            p(strong("Source code:"),a(href="https://github.com/jrodri30/HMIS/tree/master/Housing Outcomes",
              "https://github.com/jrodri30/HMIS/tree/master/Housing Outcomes")
            ),
            p(strong('Changes since last version:')),
            p(' - Changed format of summary table'),
            p(' - Minor bug fixes')
          )
        )
      )
    ),
    column(8,
      uiOutput("mainPanel")
    )
  )
))
