  # APP: DATA COMPLETENESS
  #------------------------------------
  #------------------------------------

  #################################
  # PLOT
  #################################
    
  progsPlot <- reactive({
    if (progCount2()==0 | input$reportLevel=="Program") return()
    # Take a dependency on input$update by reading it. (Nothing is actually done with the value.)
    input$update
    progress <- Progress$new(session)
    progress$set(message="Creating chart",detail="Please wait a moment...")
      #Reactivity is invalidated unless update button is pressed
      isolate(
        print(
          ggplot(dataQuality(),aes(x=factor(PROGRAM_KEY),y=PCT_MISSDKR*100,group=1)) + 
            geom_point(size=4,stat="identity",col=dataQuality()[,"PLOT_COL"],lwd=3) + 
            theme(axis.text.x=element_text(angle=90,vjust=.5)) + 
            geom_abline(intercept=5, slope=0,col="red") + 
            geom_abline(intercept=2.5, slope=0,col="darkorange") +
            ggtitle(paste("Overall Data Quality for Programs in ", finalSelect_Text(),"\nReport Period: ",
              substr(beginSelect(),6,7),"/",substr(beginSelect(),9,10),"/",substr(beginSelect(),1,4),
              " - ",substr(endSelect(),6,7),"/",substr(endSelect(),9,10),"/",
              substr(endSelect(),1,4),sep="")) + 
            xlab("Program Key") + 
            ylab("Missing, Don't know, and Refused Responses (%)")
        )
      )
    progress$close()
  })
  
  
  elementsPlot <- reactive({
    progress <- Progress$new(session)
    progress$set(message="Creating chart",detail="Please wait a moment...")
    Missing <- c(sum(dqReport()[6:13,"Missing"]),sum(dqReport()[14:17,"Missing"]),
      sum(dqReport()[18:21,"Missing"]),sum(dqReport()[22:29,"Missing"]))/
      c(sum(dqReport()[6:13,"Applicable Records"]),sum(dqReport()[14:17,"Applicable Records"]),
      sum(dqReport()[18:21,"Applicable Records"]),sum(dqReport()[22:29,"Applicable Records"]))
      
    DKR <- c(sum(dqReport()[6:13,"DKR"]),sum(dqReport()[14:17,"DKR"]),
      sum(dqReport()[18:21,"DKR"]),sum(dqReport()[22:29,"DKR"]))/
      c(sum(dqReport()[6:13,"Applicable Records"]),sum(dqReport()[14:17,"Applicable Records"]),
      sum(dqReport()[18:21,"Applicable Records"]),sum(dqReport()[22:29,"Applicable Records"]))
  
    x <- data.frame(
      Element = seq(1:6),
      label = c("","","General","Program","Income/Benefits","Special Needs"),
      Missing = c(0,0,Missing),
      DKR = c(0,0,DKR),
      OK = c(0,0,1-Missing-DKR)
    )

    graphData <- melt(x, id.vars=1:2)

    print(elementsPlot <- ggplot(graphData, aes(x=Element, y=value,fill=variable)) + 
      geom_bar(width=.9,stat="identity") +
      scale_fill_manual(values=c("firebrick2","yellow","green2"),name="Status of Client Data") + 
      coord_polar(theta="y") +
      xlab("") + 
      ylab("") +
      geom_text(data=graphData, hjust=1.02, aes(x=Element, y=0,label=label)) +
      ggtitle("Data Health") +
      theme_minimal() +
      theme(
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_blank(),
        axis.text.y = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks = element_blank(),
        plot.title = element_text(size = 25)
      )
    )
    progress$close()
  })
    
  output$plot <- renderPlot({
    # Take a dependency on input$update by reading it. (Nothing is actually done with the value.)
    input$update
    #Reactivity is invalidated unless update button is pressed
    isolate(ifelse(input$reportLevel=="Program",return(elementsPlot()),return(progsPlot())))
  })
    
