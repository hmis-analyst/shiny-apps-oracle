  # APP: DATA CONSISTENCY
  # File: mainPanel.r
  #------------------------------------
  # Global dependencies:
  #   input$reportLevel
  #   validSelect()
  #   input$printable
  #   input$reportLevel
  #   input$groupSelect
  #   input$agencySelect
  #   input$programSelect
  #   beginSelect()
  #   endSelect()
  #   dateMod()
  #   groupKeys()
  # Local dependencies:
  #   output$downloadSummary
  #   output$summaryTable
  #   output$text1
  #   output$downloadClients
  #   output$clientsTable
  #   output$downloadProgs
  #   output$progsTable
  # Objects created:
  #   output$text1
  #   output$mainPanel
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
        h2("DATA CONSISTENCY REPORT",align="center"), br(),
        (if(input$reportLevel=="Group") 
        {h3(input$groupSelect,align="center")}),
        (if(input$reportLevel %in% c("Agency","Program")) 
        {h3(input$agencySelect,align="center")}),
        (if(input$reportLevel=="Program") 
        {h3(input$programSelect,align="center")}),
        br(),
        h4(paste("Report Period:  ",dateMod(beginSelect()),"-",dateMod(endSelect())),align="center"),
        h3(class="break","Summary",align="center"),
        dataTableOutput("summaryTable"),
        h3(class="break",textOutput("text1"),align="center"),
        if(input$reportLevel=="Program") {dataTableOutput("clientsTable")}
        else {dataTableOutput("progsTable")}
      ))
    }
    else {
      isolate(div(
        h4("Main Panel",align="center"),
        tabsetPanel(
          tabPanel("Home",
            p(paste("Your data options have been submitted as of ",format(Sys.time(), "%X %Z"),". Click through the tabs above to view your results!",sep="")),
            br(),
            h4("Notes"),
            tags$ul(
              tags$li("The ",strong("Summary"),paste(" tab summarizes the data inconsistencies in your selected ",tolower(input$reportLevel),".",sep="")),
              if(input$reportLevel=="Program") {
                tags$li("For users with a valid passkey, the ",strong("Clients")," tab lists every client who had one or more inconsistencies. The column headers are IDs that 
                  refer to specific inconsistencies; you can see what these are by switching to the Summary tab. Clients are initially ordered by number of inconsistencies,
                  starting with the clients who have the most. You can sort by a ",em("specific type")," of inconsistency by clicking one of the column headers.")
              } else {
                tags$li("The ",strong("Programs"),paste(" tab lists all the programs in your selected ",tolower(input$reportLevel),", along with their 
                  inconsistency rates. You can sort by any column by clicking on the column header. You can also search for specific agencies or programs by 
                  typing into the (case sensitive) search box.",sep=""))
              },
              tags$li("All tabs contain a ",strong("download")," feature that will convert the queried data into a CSV file."),
              tags$li('You can create a ',strong('printable report'),' by selecting "printable" under the "Viewing Options" tab in the side panel.')
            )
          ),
          tabPanel("Summary",
            div(downloadButton("downloadSummary","Download Summary"),align="right"),
            dataTableOutput("summaryTable")
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
                  dataTableOutput("clientsTable")
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
          )
        )
      ))
    } 
  })
  
  
