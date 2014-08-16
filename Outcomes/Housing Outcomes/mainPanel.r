  # APP: HOUSING OUTCOMES
  # File: mainPanel.r
  #------------------------------------
  # Global dependencies:
  #   validSelect()
  #   input$printable
  #   input$reportLevel
  #   input$groupSelect
  #   input$agencySelect
  #   input$programSelect
  #   beginSelect()
  #   endSelect()
  # Local dependencies:
  #   output$downloadEnrolls
  #   output$destTable
  #   output$destBar
  #   output$downloadReturns
  #   output$returnRate
  #   returnwin()
  #   output$exitNum
  #   output$returnsTable
  #   output$returnsBar
  #   output$downloadProgs
  #   output$progsTable
  #   output$wrongProgType_1
  #   output$wrongProgType_2
  #   output$wrongProgType_3
  #   output$tooSmall_1
  #   output$tooSmall_2
  #   output$tooMissing_1
  #   output$tooMissing_2
  #   output$DoD_Gauge
  #   output$predictors
  #   output$performance
  #   output$significance
  # Objects created:
  #   output$mainPanel
  #------------------------------------
  #------------------------------------

  #################################
  # MAIN PANEL UI
  #################################
  
  output$mainPanel <- renderUI({
    # Take a dependency on input$update by reading it. (Nothing is actually done with the value.)
    input$update
    if(validSelect()==FALSE) {
      return(p("Please make your selections in the side panel and click ANALYZE"))
    } else if(input$printable==TRUE) {
      #Reactivity is invalidated unless update button is pressed
      isolate(div(
        h2("HOUSING OUTCOMES REPORT",align="center"), br(),
        (if(input$reportLevel=="Group") 
        {h3(input$groupSelect,align="center")}),
        (if(input$reportLevel %in% c("Agency","Program")) 
        {h3(input$agencySelect,align="center")}),
        (if(input$reportLevel=="Program") 
        {h3(input$programSelect,align="center")}),
        br(),
        h4(paste("Report Period:  ",dateMod(beginSelect()),"-",dateMod(endSelect())),align="center"),
        h3(class="break","Destinations",align="center"),
        dataTableOutput("destTable"),
        plotOutput("destBar"),
        h3(class="break","Returns to Homelessness",align="center"),
        textOutput("wrongProgType_1"),
        h4(textOutput("returnRate")),
        HTML(
          paste("<p>Based on your <i>end date</i> selection, the <a href='https://github.com/hmis-analyst/shiny-apps-oracle/wiki/Homeless-Returns#terms-to-remember' target='_blank'>return window</a> 
            was<strong>",returnWin(),"days</strong></p>"
          )
        ),
        p(textOutput("exitNum")),
        dataTableOutput("returnsTable"),
        plotOutput("returnsBar"),
        if(input$reportLevel != "Program") {
          div(
            h3(class="break","Program-Level Statistics",align="center"),
            dataTableOutput("progsTable")
          )
        },
        if(input$reportLevel == "Program") {
          div(
            h3(class="break","Degree of Difficulty",align="center"),
            textOutput("wrongProgType_2"),
            textOutput("tooSmall_1"),
            textOutput("tooMissing_1"),
            htmlOutput("DoD_gauge"),
            br(),
            dataTableOutput("predictors"),
            h3(class="break","Program Performance",align="center"),
            textOutput("wrongProgType_3"),
            textOutput("tooSmall_2"),
            textOutput("tooMissing_2"),
            htmlOutput("performance"),
            br(),
            br(),
            textOutput("significance")
          )
        }
      ))
    } else {
      isolate(div(
        h4("Main Panel",align="center"),
        if(input$reportLevel!="Program") {
          tabsetPanel(
            tabPanel("Destinations",
              div(downloadButton("downloadEnrolls","Download Enrollments"),align="right"),
              fluidRow(
                column(5,
                  dataTableOutput("destTable")
                ),
                column(6,offset=1,
                  plotOutput("destBar")  
                )
              )
            ),
            tabPanel("Returns",
              div(downloadButton("downloadReturns","Download Returns"),align="right"),
              h4(textOutput("returnRate")),
              HTML(
                paste("<p>Based on your <i>end date</i> selection, the 
                  <a href='https://github.com/hmis-analyst/shiny-apps-oracle/wiki/Homeless-Returns#terms-to-remember' target='_blank'>return window</a> 
                  was<strong>",returnWin(),"days</strong></p>"
                )
              ),
              p(textOutput("exitNum")),
              br(),
              fluidRow(
                column(5,
                  dataTableOutput("returnsTable")
                ),
                column(6,offset=1,
                  plotOutput("returnsBar")
                )
              )
            ),
            tabPanel("Programs",
              div(
                div(downloadButton("downloadProgs","Download Programs"),align="right"),
                br(),
                dataTableOutput("progsTable")
              )
            )
          )
        } else {
          tabsetPanel(id="main",
            tabPanel("Destinations",
             div(downloadButton("downloadEnrolls","Download Enrollments"),align="right"),
             fluidRow(
               column(5,
                 dataTableOutput("destTable")
               ),
               column(6,offset=1,
                 plotOutput("destBar")  
               )
             )
            ),
            tabPanel("Returns",
              textOutput("wrongProgType_1"),
              div(downloadButton("downloadReturns","Download Returns"),align="right"),
              h4(textOutput("returnRate")),
              HTML(
                paste("<p>Based on your <i>end date</i> selection, the <a href='https://github.com/hmis-analyst/shiny-apps-oracle/wiki/Homeless-Returns#terms-to-remember' target='_blank'>return window</a> 
                  was<strong>",returnWin(),"days</strong></p>"
                )
              ),
              p(textOutput("exitNum")),
              br(),
              fluidRow(
                column(5,
                  dataTableOutput("returnsTable")
                ),
                column(6,offset=1,
                  plotOutput("returnsBar")
                )
              )  
            ),
            tabPanel("Difficulty",
              textOutput("wrongProgType_2"),
              textOutput("tooSmall_1"),
              textOutput("tooMissing_1"),
              htmlOutput("DoD_gauge"),
              br(),
              dataTableOutput("predictors")
            ),
            tabPanel("Performance",
              textOutput("wrongProgType_3"),
              textOutput("tooSmall_2"),
              textOutput("tooMissing_2"),
              htmlOutput("performance"),
              br(),
              br(),
              textOutput("significance")
            )
          )
        }
      ))
    } 
  })

 

