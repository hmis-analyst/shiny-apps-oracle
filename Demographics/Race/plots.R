  # APP: RACE
  #------------------------------------
  #------------------------------------

  #################################
  # PLOT 
  #################################

  # Create a reactive plot based on queryResults()
  # Then make available for output into UI
  output$Plot <- renderPlot({
    if (progCount2()==0) return()
    input$update
    graphData <- data.frame(Race = c('Black','White','Asian', 'Multi Racial or Other'),Value=c(queryResults()[[1]],queryResults()[[2]], queryResults()[[3]], queryResults()[[4]]))
    progress <- Progress$new(session)
    progress$set(message="Creating chart",detail="Please wait a moment...")
    # Transform queryResults() into a format acceptable for plotting
    isolate(  
      print(
        # Begin plotting with ggplot()
        # Define variables
        ggplot(graphData,aes(x=factor(Race,levels=Race),y=Value)) + 
          # Define as bar chart
          geom_bar(fill=c("mediumpurple2","steelblue1", "orange","mediumaquamarine"),stat='identity') + 
          # Define title
          ggtitle(paste("Race Breakdown for ", ifelse(input$reportLevel!="Program","Programs in ",paste(input$agencySelect,": ",sep="")),
                        finalSelect_Text(),"\nReport Period: ",substr(beginSelect(),6,7),"/",substr(beginSelect(),9,10),"/",
                        substr(beginSelect(),1,4)," - ",substr(endSelect(),6,7),"/",substr(endSelect(),9,10),"/",
                        substr(endSelect(),1,4),sep="")) + 
          # Define axis labels
          xlab("Race") + 
          ylab("Number of Unique Clients")
      )
    )
    progress$close()
  }) 
