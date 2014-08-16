  # APP: HOUSING OUTCOMES
  # File: errors.r
  #------------------------------------
  # Global dependencies:
  #   input$programTypes
  # Local dependencies:
  #   Exits()
  #   Outcome_predicted()
  # Objects created:
  #   output$wrongProgType_1
  #   output$wrongProgType_2
  #   output$wrongProgType_3
  #   output$tooSmall_1
  #   output$tooSmall_2
  #   output$tooMissing_1
  #   output$tooMissing_2
  #------------------------------------
  #------------------------------------
  
  #################################
  # ERROR MESSAGES
  #################################
  
  # Wrong program type selected
  output$wrongProgType_1 <- renderText({
    isolate(if(wrongProgType(input$programTypes)==TRUE) return("Unable to calculate difficulty and performance due to invalid program type."))
  })
  output$wrongProgType_2 <- renderText({
    isolate(if(wrongProgType(input$programTypes)==TRUE) return("Unable to calculate difficulty and performance due to invalid program type."))
  })
  output$wrongProgType_3 <- renderText({
    isolate(if(wrongProgType(input$programTypes)==TRUE) return("Unable to calculate difficulty and performance due to invalid program type."))
  })
  
  # Selected program is too small
  output$tooSmall_1 <- renderText({
    isolate(if(wrongProgType(input$programTypes)==TRUE) return())
    if(length(Exits()[,1])<10) return("Insufficient data. In order to calculate difficulty and performance, at least 10 clients must have exited during the report period.")
  })
  output$tooSmall_2 <- renderText({
    isolate(if(wrongProgType(input$programTypes)==TRUE) return())
    if(length(Exits()[,1])<10) return("Insufficient data. In order to calculate difficulty and performance, at least 10 clients must have exited during the report period.")
  })
  
  # Selected program has too much missing data
  output$tooMissing_1 <- renderText({
    isolate(if(length(Exits()[,1])<10 | wrongProgType(input$programTypes)==TRUE | input$reportLevel!="Program") return())
    if(Outcome_predicted()[1]==999) return(paste("Only ",Outcome_predicted()[2],"% of exiting households
      in this program had complete data during the report period. 90% is required to calculate difficulty and performance.",sep=""))
  })
  output$tooMissing_2 <- renderText({
    isolate(if(length(Exits()[,1])<10 | wrongProgType(input$programTypes)==TRUE | input$reportLevel!="Program") return())
    if(Outcome_predicted()[1]==999) return(paste("Only ",Outcome_predicted()[2],"% of exiting households
      in this program had complete data during the report period. 90% is required to calculate difficulty and performance.",sep=""))
  })
  

