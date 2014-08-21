  # APP: DATA CONSISTENCY
  # File: tables.r
  #------------------------------------
  # Global dependencies:
  #   input$printable
  # Local dependencies:
  #   summaryData()
  #   summaryData_2()
  #   progsData_2()
  #   clientsData()
  # Objects created:
  #   table_options_1()
  #   table_options_2
  #   table_options_3
  #   table_options_4
  #   output$summaryTable
  #   output$progsTable
  #   output$clientsTable
  #------------------------------------
  #------------------------------------
  
  table_options_1 <- reactive({
    list(
      fnRowCallback = I('
        function(nRow, aData, iDisplayIndex, iDisplayIndexFull) {
          // Column alignment
          $("td:eq(1)", nRow).css("text-align", "left");
          $("td:eq(2)", nRow).css("text-align", "right");
          $("td:eq(3)", nRow).css("text-align", "right");
          $("td:eq(4)", nRow).css("text-align", "right");
          // Make column 4 values bold
          $("td:eq(4)", nRow).css("font-weight", "bold");
          // Set conditional font colors for values in column 2
          if (parseFloat(aData[3]) > 0) {
            $("td:eq(3)", nRow).css("color", "red");
            $("td:eq(3)", nRow).css("font-weight", "bold");
          };
          // Set conditional font colors for values in column 4
          $("td:eq(4)", nRow).css("color", "green");
          if (parseFloat(aData[4]) > 0) {
            $("td:eq(4)", nRow).css("color", "orange");
          };
          if (parseFloat(aData[4]) >= 2.5) {
            $("td:eq(4)", nRow).css("color", "red");
          };
        }
      '),
      bAutoWidth=FALSE,bFilter=0,bPaginate=0,bLengthChange=0,bSort=0,bInfo=0,iDisplayLength=length(summaryData())+1,
      aoColumns=list(list(bSearchable=FALSE),list(bSearchable=FALSE),list(bSearchable=FALSE),
        list(bSearchable=FALSE),list(bSearchable=FALSE))
    )
  })
  
  table_options_2 <- list(
    fnRowCallback = I('
      function(nRow, aData, iDisplayIndex, iDisplayIndexFull) {
        // Column alignment
        $("td:eq(3)", nRow).css("text-align", "right");
        $("td:eq(4)", nRow).css("text-align", "right");
        $("td:eq(5)", nRow).css("text-align", "right");
        // Make column 5 values bold
        $("td:eq(5)", nRow).css("font-weight", "bold");
        // Set conditional font colors for values in column 5
        $("td:eq(5)", nRow).css("color", "green");
        if (parseFloat(aData[5]) > 0) {
          $("td:eq(5)", nRow).css("color", "orange");
        };
        if (parseFloat(aData[5]) >= 2.5) {
          $("td:eq(5)", nRow).css("color", "red");
        };
      }
    '),
    bAutoWidth=FALSE,bFilter=0,bPaginate=0,bLengthChange=0,bInfo=0,iDisplayLength=500,
    aoColumns=list(list(bSearchable=FALSE),list(bSearchable=FALSE),list(bSearchable=FALSE),
      list(bSearchable=FALSE),list(bSearchable=FALSE),list(bSearchable=FALSE))
  )
  
  table_options_3 <- list(
    fnRowCallback = I('
      function(nRow, aData, iDisplayIndex, iDisplayIndexFull) {
        // Column alignment
        $("td:eq(3)", nRow).css("text-align", "right");
        $("td:eq(4)", nRow).css("text-align", "right");
        $("td:eq(5)", nRow).css("text-align", "right");
        // Make column 5 values bold
        $("td:eq(5)", nRow).css("font-weight", "bold");
        // Set conditional font colors for values in column 5
        $("td:eq(5)", nRow).css("color", "green");
        if (parseFloat(aData[5]) > 0) {
          $("td:eq(5)", nRow).css("color", "orange");
        };
        if (parseFloat(aData[5]) >= 2.5) {
          $("td:eq(5)", nRow).css("color", "red");
        };
      }
    '),
    bAutoWidth=FALSE,
    aoColumns=list(list(bSearchable=FALSE),list(bSearchable=FALSE),list(bSearchable=FALSE),
      list(bSearchable=FALSE),list(bSearchable=FALSE),list(bSearchable=FALSE))
  )
  
  table_options_4 <- list(
    fnRowCallback = I('
      function(nRow, aData, iDisplayIndex, iDisplayIndexFull) {
        // Column alignment
        $("td:eq(0)", nRow).css("text-align", "left");
        $("td:eq(1)", nRow).css("text-align", "center");
        $("td:eq(2)", nRow).css("text-align", "center");
        $("td:eq(3)", nRow).css("text-align", "center");
        $("td:eq(4)", nRow).css("text-align", "center");
      }
    '),
    bAutoWidth=FALSE,bFilter=0,bPaginate=0,bLengthChange=0,bInfo=0,iDisplayLength=500,
    aoColumns=list(list(bSearchable=FALSE),list(bSearchable=FALSE),list(bSearchable=FALSE),
      list(bSearchable=FALSE),list(bSearchable=FALSE))
  )
  
  #################################
  # TABLES
  #################################
  
  output$summaryTable <- renderDataTable({
    summaryTable <- summaryData_2()
    names(summaryTable) <- c("ID","Data Element","Applicable Enrollment Records","Hits","Hits (%)")
    return(summaryTable)
  },
    options=table_options_1()
  )  
    
  
  output$progsTable <- renderDataTable({
    progsTable <- progsData_2()
    names(progsTable) <- c("Program Key", "Agency Name", "Program Name", "Enrollments", "Applicable Records", "Inconsistency Hits (%)")
    return(progsTable)
  },
    options=reactive({if(input$printable==TRUE) {table_options_2} else {table_options_3}})
  )
  
    
  output$clientsTable <- renderDataTable({
    clientsTable <- clientsData()
    names(clientsTable) <- c("Client Key",1:(length(clientsTable)-1))
    return(clientsTable)
  },
    options=table_options_4
  )
  
