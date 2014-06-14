# TITLE: HOMELESSNESS RECURRENCE
#------------------------------------
#------------------------------------

#.libPaths("~/R/win-library/3.1")
#install.packages("RJDBC",repos="http://cran.rstudio.com/")

# Load shiny and RODBC packages
library(shinyIncubator)
library(RJDBC)

libPath1 <- "~/HMIS Data Analyst/lib/"
libPath2 <- "../../lib/"

# Establish JDBC connection using RJDBC
source(paste(libPath1,"conn-Ora-Georgia_Base.r",sep=""),local=TRUE)

Sys.Year <- as.numeric(format(Sys.Date(),"%Y"))
Sys.Month <- as.numeric(format(Sys.Date(),"%m"))
Sys.Day <- as.numeric(format(Sys.Date()-1,"%d"))

shinyUI(basicPage(
  progressInit(),
  includeHTML("lib/page.html"),
  br(),
  fluidRow(
    column(4,
      tags$form(class="well noprint",
        h3("Homelessness Recurrence",align="center"),
        tabsetPanel(selected="Data Options",
          tabPanel("Instructions",
            p('1. FIRST select your data options.'),
            p('2. Click the "Analyze" button.'),
            p('3. When the analysis is complete, the Main Panel will update. Upon completion, you will
               also be able to download your data as a spreadsheet.'),
            p('4. If you are not satisfied with the appearance of the report, 
               you can modify the viewing options.'),
            p('5. If you would like to increase the recurrence window, move the report end date further back. Do the opposite to decrease the recurrence window.')
          ),
          tabPanel("Data Options",
            div(actionButton("update",strong("ANALYZE"),icon=icon("arrow-circle-right")),align="right"),
            # Display date range input with default values
            dateRangeInput("daterange","Analyze exits between...", format='mm/dd/yyyy',start="2012-07-01",end=paste(Sys.Year-1,Sys.Month,Sys.Day,sep="-"),
              min=paste(Sys.Year-5,Sys.Month,Sys.Day,sep="-"),max=Sys.Date()),
            br(),
            # Display radio buttons with 3 defined report levels
            radioButtons("reportLevel", "Report level",list("Group","Agency","Program"),selected="Program"),
            br(),
            # Display "Group Name" select list when user chooses "Group" report level
            conditionalPanel(
              # Define condition of this display
              condition = "input.reportLevel == 'Group'",
              # Query Pathways group names to populate select list
              selectInput("groupSelect", "Group name",as.list(as.character(
                dbGetQuery(connection,"
                  SELECT unique Group_Name 
                  FROM Community_Group_Information CGI
                  JOIN Program_Community_Information PCI
                    on CGI.Group_Key = PCI.Group_Key
                  JOIN Program_Profile_Info PPI
                    on PCI.Program_Key = PCI.Program_Key
                  WHERE Program_Type_Code in (1,2,3,14)
                  ORDER BY Group_Name"
                )[[1]]
              )))
            ),
            # Display "Agency Name" select list when user chooses "Agency" or "Program" report levels
            conditionalPanel(
              # Define condition of this display
              condition = "input.reportLevel == 'Agency' | input.reportLevel == 'Program'",
              # Query agency names to populate select list
              selectInput("agencySelect", "Agency name",as.list(as.character(
                dbGetQuery(connection,"
                  SELECT unique Agency_Name 
                  FROM Program_Profile_Info
                  WHERE Program_Type_Code in (1,2,3,14)
                  ORDER BY Agency_Name"
                )[[1]]
              )))
            ),
            # Display "Program Name" select list when user chooses "Program" report level
            conditionalPanel(
              # Define condition of this display
              condition = "input.reportLevel == 'Program'",
              # Call "programChoices" (reactive select list, defined in server.R)
              # Display on sidebar
              uiOutput("programChoices")
            ),
            br(),
            uiOutput("programTypes"),
            br(),br(),br(),br(),br(),br(), br()
          ),
          tabPanel("Viewing Options",
            checkboxInput("printable", "Printable", FALSE)
          ),
          tabPanel("About",
            p(strong("Title:"),"Georgia HMIS Homelessness Recurrence"),
            p(strong("Version:"),"1.2.0"),
            p(strong("Date:"),"14 June 2014"),
            p(strong("Description:"),"Set of customizable reports and charts on recurrence, for the purpose of reporting
              and improving program outcomes among homeless service providers in the state of Georgia"),
            p(strong("Bug reports:"),"Send to ",a(href="mailto:jason.m.rodriguez@vanderbilt.edu","jason.m.rodriguez@vanderbilt.edu")),
            p(strong("Source code:"),"See ",a(href="https://github.com/hmis-analyst/shiny-apps-oracle/tree/master/Outcomes/Recurrence", target="_blank",
              "Github")
            ),
            p(strong("Documentation:"),a(href="https://github.com/hmis-analyst/shiny-apps-oracle/wiki/Homelessness-Recurrence", target="_blank",
              "wiki")
            ),
            p(strong('Changes since last version:')),
            p(' - Minor tweaks to the algorithm'),
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
