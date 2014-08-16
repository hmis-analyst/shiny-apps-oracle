  # APP: HOUSING OUTCOMES
  # File: tables.r
  #------------------------------------
  # Global dependencies:
  #   input$programTypes
  #   "Variable descriptions.csv"
  # Local dependencies:
  #   Exits_summary()
  #   Exits_progs()
  #   Exits()
  #   Returns()
  #   MLE()
  # Objects created:
  #   output$destTable
  #   output$progsTable
  #   output$returnsTable
  #   output$predictors
  #------------------------------------
  #------------------------------------

  #################################
  # TABLES FOR DESTINATIONS
  #################################

  output$destTable <- renderDataTable({
    Exits_summary()
  },options=list(
      fnRowCallback = I('
        function(nRow, aData, iDisplayIndex, iDisplayIndexFull) {
          // Column alignment
          $("td:eq(1)", nRow).css("text-align", "left");
          $("td:eq(2)", nRow).css("text-align", "right");
        }
      '),
      bAutoWidth=FALSE,bFilter=0,bPaginate=0,bLengthChange=0,bSort=0,bInfo=0,
        aoColumns=list(list(bSearchable=FALSE),list(bSearchable=FALSE))
    )
  )
  
  output$progsTable <- renderDataTable({
    Exits_progs()
  },options=list(
      fnRowCallback = I('
        function(nRow, aData, iDisplayIndex, iDisplayIndexFull) {
          // Column alignment
          $("td:eq(3)", nRow).css("text-align", "right");
          $("td:eq(4)", nRow).css("text-align", "right");
          $("td:eq(5)", nRow).css("text-align", "right");
        }
      '),
      bAutoWidth=FALSE,bPaginate=0,bLengthChange=0,bInfo=0,iDisplayLength=500,
      if(input$mdkr==FALSE) {
        aoColumns=list(list(bSearchable=FALSE),list(bSearchable=FALSE),list(bSearchable=FALSE),
          list(bSearchable=FALSE),list(bSearchable=FALSE),list(bSearchable=FALSE))
      }
      else {
        aoColumns=list(list(bSearchable=FALSE),list(bSearchable=FALSE),list(bSearchable=FALSE),
          list(bSearchable=FALSE),list(bSearchable=FALSE))
      }
    )
  )  
  
  
  #################################
  # TABLES FOR RETURNS
  #################################
  #-------------------
  output$returnsTable <- renderDataTable({
    if(length(Exits2()[,1])==0) return()
    if(Returns()==0) return()
    returnsTable <- returnsBar_data()[,c("label","num","rate")]
    returnsTable[which(is.na(returnsTable$num)),c("num","rate")] <- 0
    for (i in 1:length(returnsTable[,1])) {
      returnsTable[i,"rate_cum"] <- sum(returnsTable[1:i,"rate"])
    }
    totalRow <- data.frame(
      label="TOTAL",
      num=sum(returnsTable$num),
      rate=sum(returnsTable$rate),
      rate_cum=NA
    )
    returnsTable <- rbind(returnsTable,totalRow)
    names(returnsTable) <- c("Months Until Return","Returns","% of Exits", "Cumulative % of Exits")
    return(returnsTable)
  },options=list(
      fnRowCallback = I('
        function(nRow, aData, iDisplayIndex, iDisplayIndexFull) {
          // Column alignment
          $("td:eq(0)", nRow).css("text-align", "left");
          $("td:eq(1)", nRow).css("text-align", "right");
          $("td:eq(2)", nRow).css("text-align", "right");
          $("td:eq(3)", nRow).css("text-align", "right");
        }
      '),
      bAutoWidth=FALSE,bPaginate=0,bLengthChange=0,bInfo=0,bSort=0,bFilter=0,
      aoColumns=list(list(bSearchable=FALSE),list(bSearchable=FALSE),list(bSearchable=FALSE),list(bSearchable=FALSE))
    )
  )

  #################################
  # TABLES FOR PERFORMANCE
  #################################
  output$predictors <- renderDataTable({
    if(validSelect()==FALSE) return()
    isolate(if(wrongProgType(input$programTypes)==TRUE | input$reportLevel!="Program") return())
    descriptions <- read.csv(paste(libPath2,"Variable descriptions.csv",sep=""),stringsAsFactors=FALSE)
    predictors1 <- merge(MLE()[which(MLE()$Estimate<0),],descriptions[,c("Category","Variable","Pos")],by.x="Parameter",by.y="Variable")
    predictors2 <- merge(MLE()[which(MLE()$Estimate>0),],descriptions[,c("Category","Variable","Neg")],by.x="Parameter",by.y="Variable")
    names(predictors1) <- c("Category","Variable","Effect","Description")
    names(predictors2) <- c("Category","Variable","Effect","Description")
    predictors3 <- rbind(predictors1,predictors2)
    predictors4 <- data.frame(Description = predictors3[with(predictors3,order(Category,Variable)),"Description"])
    names(predictors4) <- "Client characteristics that contribute to degree of difficulty"
    return(predictors4)
  },options=list(
    bAutoWidth=FALSE,bFilter=0,bPaginate=0,bLengthChange=0,bInfo=0,iDisplayLength=500,bSort=0,aoColumns=list(list(bSearchable=FALSE))
  ))
  

