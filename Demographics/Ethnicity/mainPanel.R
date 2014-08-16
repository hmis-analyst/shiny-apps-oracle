  # APP: ETHNICITY
  #------------------------------------
  #------------------------------------ 

  output$mainPanel <- renderUI ({
    input$update
    if(validSelect()==FALSE) {
      return(p("Please make your selections in the side panel and click ANALYZE"))
    } else {
      # Call "Plot" (reactive plot, defined in server.R)
      plotOutput("Plot")
    }
  })