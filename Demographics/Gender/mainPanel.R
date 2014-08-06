  # APP: HOUSING OUTCOMES
  #------------------------------------
  #------------------------------------

  #################################
  # MAIN PANEL
  #################################
  
  output$mainPanel <- renderUI({
    if(validSelect()==FALSE) {
      return(p("Please make your selections in the side panel and click ANALYZE"))
    }
    plotOutput("Plot")
  })
  


