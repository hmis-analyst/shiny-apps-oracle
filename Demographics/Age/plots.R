  # APP: AGE
  #------------------------------------
  #------------------------------------

  #################################
  # PLOT
  #################################


  # Create a reactive plot based on queryResults()
  # Then make available for output into UI
  output$Plot <- renderPlot({
    a <- c(1,2,3,4,5,6,7,8)
    b <- c(0,0,0,0,0,0,0,0)
    rs <- data.frame(a,b)
  
    if (progCount2()==0) return()
    input$update
    x <- queryResults()[1]
    y <- queryResults()[2]
    j <- 1
    for (i in x) {
      rs[i,2] <- y[j]
      j <- j+1
    }
  
    graphData <- data.frame(Age = c(
      '0-4','5-17','18-24','25-34','35-44',
      '45-54','55-64','65+'),Value=rs[[2]])
    # graphData <- data.frame(Age = c('0-12','13-18','19-35','36-65','66-99'),Value=c(queryResults()[[1]],queryResults()[[2]]))
    #graphData <- data.frame(Age = c('0-12','13-18','19-35','36-65','66-99'),c(rs()[2]))
  
    progress <- Progress$new(session)
    progress$set(message="Creating chart",detail="Please wait a moment...")
    # Transform queryResults() into a format acceptable for plotting
    isolate(
      print(
        # Begin plotting with ggplot()
        # Define variables
        ggplot(graphData,aes(x=factor(Age,levels=Age),y=Value)) +
          # Define as bar chart
          geom_bar(fill=c(
            "lightslateblue","lightslateblue","lightslateblue","lightslateblue",
            "lightslateblue","lightslateblue","lightslateblue","lightslateblue"),stat='identity',color="black") +
          # Define title
          ggtitle(paste("Age Breakdown for ", ifelse(input$reportLevel!="Program","Programs in ",paste(input$agencySelect,": ",sep="")),
                        finalSelect_Text(),
                        "\nReport Period: ",substr(beginSelect(),6,7),"/",substr(beginSelect(),9,10),"/",
                        substr(beginSelect(),1,4)," - ",substr(endSelect(),6,7),"/",substr(endSelect(),9,10),"/",
                        substr(endSelect(),1,4),sep="")) +
          # Define axis labels
          xlab("Age") +
          ylab("Number of Unique Clients")
      )
    )
    progress$close()
  })

