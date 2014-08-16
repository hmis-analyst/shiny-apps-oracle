  # APP: HOUSING OUTCOMES
  # File: plots.r
  #------------------------------------
  # Global dependencies:
  #   None
  # Local dependencies:
  #   Exits_summary()
  #   Exits2()
  #   Returns()
  #   Exits()
  #   Outcome_predicted()
  #   DoD_reference()
  #   Outcome_actual()
  # Objects created:
  #   destBar_data()
  #   output$destBar
  #   returnsBar_data()
  #   output$returnsBar
  #   output$DoD_gauge
  #   output$performance
  #------------------------------------
  #------------------------------------
  
  #################################
  # PLOT
  #################################

  # Create a reactive plot based on Exits_summary()
  # Then make available for output into UI
  destBar_data <- reactive({
    # Take a dependency on input$update by reading it. (Nothing is actually done with the value.)
    # Transform Exits_summary into a format acceptable for plotting
    destBar_data <- data.frame(
      Destination = Exits_summary()[which(!(Exits_summary()$DESTINATION %in% c("Unknown","TOTAL"))),1],
      Value = Exits_summary()[which(!(Exits_summary()$DESTINATION %in% c("Unknown","TOTAL"))),2],
      color = c("firebrick2","darkorchid","royalblue2","green4"),
      cat_order = c("A","B","C","D")
    )
    if(input$mdkr==FALSE) {
      UNKNOWN <- data.frame( 
        Destination = "Unknown",
        Value = Exits_summary()[1,2],
        color = "gray45",
        cat_order = "E"
      )
      destBar_data <- rbind(UNKNOWN,destBar_data)
    }
    return(destBar_data)
  })
  output$destBar <- renderPlot({
    if(validSelect()==FALSE) return()
    print(
      # Begin plotting with ggplot()
      # Define variables
      barchart <- ggplot(data=destBar_data(), aes(x=Destination,y=Value,fill=cat_order)) +
        # Define as bar chart
        geom_bar(color="black",stat="identity") + 
        # Define bar colors
        scale_fill_manual(values=as.character(destBar_data()$color),name="Destination") +
        # Add text labels
        geom_text(aes(label = paste(Value," (",round(100*Value/sum(Value),digits=1),"%)",sep=""),
          size=10,vjust=-.5)) +
        # Remove legend
        theme(legend.position="none", axis.text.x = element_text(angle = 45, hjust = 1,size=16),axis.text.y = element_text(size=16),
          axis.title.x = element_text(size=18),axis.title.y = element_text(size=18),plot.title = element_text(size =18)) +
        # Define title
        ggtitle("Housing Outcomes") + 
        # Define axis labels
        xlab("Destination at Program Exit") + 
        ylab("Number of Clients Who Exited")
    )
  }) 
  
  #################################
  # PLOTS FOR RETURNS
  #################################
  
  returnsBar_data <- reactive({
    frame <- data.frame(
      label = c("0-3", "3-6", "6-9", "9-12", "12-15","15-18","18-21","21-24",">= 24", "None"),
      Return_Type = seq(1:10),
      Return_Type_Txt = c("A","B","C","D","E","F","G","H","I","J"),
      col = c("#67000D","#A50F15","#CB181D","#EF3B2C","#FB6A4A","#FC9272","#FCBBA1","#FEE0D2","#FFF5F0","#BAE4B3")
    )
    Exits_Summary <- ddply(Exits2()[which(Exits2()[,"Return_Type"]!=10),],"Return_Type",summarize,num=length(Return))
    returnsBar_data <- merge(frame[1:max(Exits_Summary$Return_Type),],Exits_Summary,by="Return_Type",all.x=TRUE)
    returnsBar_data <- returnsBar_data[order(returnsBar_data[,"Return_Type"]),]
    returnsBar_data[,"rate"] <- round(returnsBar_data[,"num"]/length(Exits2()[,1])*100,1)
    return(returnsBar_data)
  })
  
  output$returnsBar <- renderPlot({
    if(length(Exits2()[,1])==0) return()
    if(Returns()==0) return()
    print(barChart <- ggplot(data=returnsBar_data(),aes(x=Return_Type_Txt,y=rate,fill=Return_Type_Txt)) + 
      geom_bar(color="black",stat="identity") +
      scale_fill_manual(
        values=as.character(returnsBar_data()[,"col"])
      ) +
      theme(legend.position="none", axis.text.x = element_text(angle = 45, hjust = 1,size=18),axis.text.y = element_text(size=18),
        axis.title.x = element_text(size=18),axis.title.y = element_text(size=18),plot.title = element_text(size =18)) +
      scale_x_discrete(breaks=returnsBar_data()[,"Return_Type_Txt"], labels=returnsBar_data()[,"label"]) +
      ggtitle("The Swiftness of Return") +
      ylab("% of exits") +
      xlab("Months until return")
    )
  })
  
  #################################
  # PLOTS FOR PERFORMANCE
  #################################

  output$DoD_gauge <- renderGvis({
    isolate(if(validSelect()==FALSE | input$reportLevel!="Program") return())
    if(length(Exits()[,1])<10) return()
    if(Outcome_predicted()[1]==999) return() 
    DoD_r <- DoD_reference()[which(!is.na(DoD_reference()$decile)),]
    op <- Outcome_predicted()
    gauge_data <- data.frame(
      Label = "Difficulty", 
      Value = DoD_r$decile[which(abs(DoD_r$p-op)==min(abs(DoD_r$p-op)))]
    )
    gvisGauge(gauge_data,
      options=list(min=0, max=10, greenFrom=0, greenTo=2, yellowFrom=2, yellowTo=4,redFrom=4, redTo=10,width=225,height=225,majorTicks='["0","","","","","10"]')
    );
  })
  
  output$performance <- renderGvis({
    if(length(Exits()[,1])<10) return()
    if(Outcome_predicted()[1]==999) return()
    performance <- data.frame(
      label = c("Predicted","Actual"),
      value = c(round(Outcome_predicted()*100,1),round(Outcome_actual()*100,1))
    )
    gvisBarChart(
      data=performance,
      xvar="label",
      yvar="value",
      options=list(
        title="90-Day Placement Rate",
        titleTextStyle="{fontSize: 18}",
        hAxis="{
          title: '% of clients who were placed in permanent housing for at least 90 days', minValue: 0
        }",
        legend="{position: 'none'}",
        colors="['#336688']"
      )
    )
  })


  
  