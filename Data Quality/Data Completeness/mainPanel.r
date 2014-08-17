  # APP: DATA COMPLETENESS
  #------------------------------------
  #------------------------------------
  
  #################################
  # MAIN PANEL UI
  ################################# 
  
  output$text1 <- renderText({ 
    # Take a dependency on input$update by reading it. (Nothing is actually done with the value.)
    input$update
    #Reactivity is invalidated unless update button is pressed
    isolate(ifelse(input$reportLevel=="Program","Clients","Programs"))
  })
  
  output$mainPanel <- renderUI({
    # Take a dependency on input$update by reading it. (Nothing is actually done with the value.)
    input$update
    if(validSelect()==FALSE) {
      return(p("Please make your selections in the side panel and click ANALYZE"))
    } else if (input$printable==TRUE) {
      #Reactivity is invalidated unless update button is pressed
      isolate(mainPanel(
        h2("DATA COMPLETENESS REPORT",align="center"), br(),
        (if(input$reportLevel=="Group") 
        {h3(input$groupSelect,align="center")}),
        (if(input$reportLevel %in% c("Agency","Program")) 
        {h3(input$agencySelect,align="center")}),
        (if(input$reportLevel=="Program") 
        {h3(input$programSelect,align="center")}),
        br(),
        h4(paste("Report Period:  ",dateMod(beginSelect()),"-",dateMod(endSelect())),align="center"),
        h3(class="break","Summary",align="center"),
        dataTableOutput("dqTable_short"),
        h3(class="break","Data Elements",align="center"),
        dataTableOutput("dqTable"),
        h3(class="break",textOutput("text1"),align="center"),
        if(input$reportLevel=="Program") {dataTableOutput("Violations")}
        else {dataTableOutput("progsTable")},
        br(), br(),
        div(align="center",plotOutput("plot"))
      ))
    }
    else {
      isolate(div(
        h4("Main Panel",align="center"),
        tabsetPanel(
          tabPanel("Summary",
            div(downloadButton("downloadSummary","Download Summary"),align="right"),
            dataTableOutput("dqTable_short")
          ),
          tabPanel("Data Elements",
            div(downloadButton("downloadDQ","Download Elements"),align="right"),
            dataTableOutput("dqTable")
          ),
          tabPanel(textOutput("text1"),
            if(input$reportLevel=="Program") {
              if(is.null(groupKeys())) {
                div(
                  p(strong("UNAUTHORIZED USER")),
                  p("You must enter a valid passkey to access this program's client-level data.")
                  #p("If you have lost your passkey or have never received one, please contact ",a(href="mailto:dave.totten@dca.ga.gov","Dave Totten"))
                )
              }
              else {
                div(
                  div(downloadButton("downloadClients","Download Clients"),align="right"),
                  dataTableOutput("Violations")
                )
              }
            }
            else {
              div(
                div(downloadButton("downloadProgs","Download Programs"),align="right"),
                br(),
                dataTableOutput("progsTable")
              )
            }
          ),
          tabPanel("Plot",
            plotOutput("plot")
          )
        )
      ))
    } 
  })
  
  
