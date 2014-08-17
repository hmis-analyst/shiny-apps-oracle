  # APP: DATA COMPLETENESS
  #------------------------------------
  #------------------------------------

  
  #################################
  # TABLES
  #################################
  
  dqReport <- reactive({
    if (progCount2()==0) return()
      dqReport <- data.frame(
      
        Data_Element=c("Total Clients","Total Adults","Total Unaccompanied Children","Total Leavers", "Long-Term Stayers",
          "First Name","Last Name","Social Security Number","Date of Birth","Race","Ethnicity","Gender",
          "Veteran Status","Disabling Condition","Residence Prior to Program Entry",
          "Zip Code of Last Permanent Address","Housing Status (at entry)","Income (at entry)",
          "Income (at exit)","Non-Cash Benefits (at entry)","Non-Cash Benefits (at exit)",
          "Physical Disablity (at entry)","Developmental Disability (at entry)",
          "Chronic Health Condition (at entry)","HIV/AIDS (at entry)","Mental Illness (at entry)",
          "Substance Abuse (at entry)","Domestic Violence (at entry)","Destination (at exit)","TOTAL"),
        
        Applicable_Records=c(sum(dataQuality()[["ENROLLS"]]),sum(dataQuality()[["ADULTS"]]),
          sum(dataQuality()[["UNACHILD"]]),sum(dataQuality()[["LEAVERS"]]),sum(dataQuality()[["LONGTERMSTAYERS"]]),rep(sum(dataQuality()[["ENROLLS"]]),7),
          sum(dataQuality()[["ADULTS"]]),rep(sum(dataQuality()[["ENROLLS"]]),5),sum(dataQuality()[["LEAVERS"]]),
          sum(dataQuality()[["ENROLLS"]]),sum(dataQuality()[["LEAVERS"]]),rep(sum(dataQuality()[["ENROLLS"]]),7),
          sum(dataQuality()[["LEAVERS"]]),sum(dataQuality()[["APP_REC"]])),
        
        Missing=c(NA,NA,NA,NA,NA,as.numeric(colSums(dataQuality()[grep("^NAME_F_M$",colnames(dataQuality())):
          grep("^DEST_M$",colnames(dataQuality()))])),sum(dataQuality()[["SUM_MISS"]])),
          
        DKR=c(NA,NA,NA,NA,NA,0,0,as.numeric(colSums(dataQuality()[grep("^SSN_DKR$",colnames(dataQuality())):
          grep("^DEST_DKR$",colnames(dataQuality()))])),sum(dataQuality()[["SUM_DKR"]]))
        
      )
    
      names(dqReport) <- c("Data Element","Applicable Records","Missing","DKR")
    
      dqReport[,"Missing + DKR (%)"] <- ifelse(is.na(dqReport[,"Missing"]) | is.na(dqReport[,"DKR"]),"",
        ifelse(dqReport[,"Applicable Records"]==0 & !is.na(dqReport[,"Missing"]) & !is.na(dqReport[,"DKR"]),"0",
        round((dqReport[,"Missing"]+dqReport[,"DKR"])/dqReport[,"Applicable Records"]*100,1)))
    
      return(dqReport)
  })
  
  dqReport_short <- reactive({
    if (progCount2()==0) return()   
      dqReport_short <- data.frame(
        Category = c("Unaccompanied Children","Long-Term Stayers","Universal","Program-Specific","TOTAL"),
        App_Rec = c(sum(dqReport()[3,2]),sum(dqReport()[5,2]),sum(dqReport()[6:13,2]),sum(dqReport()[14:29,2]),dqReport()[30,2]),
        M = c(NA,NA,sum(dqReport()[6:13,3]),sum(dqReport()[14:29,3]),dqReport()[30,3]),
        DKR = c(NA,NA,sum(dqReport()[6:13,4]),sum(dqReport()[14:29,4]),dqReport()[30,4]),
        MDKR = round(c(NA,NA,sum(dqReport()[6:13,3:4])/sum(dqReport()[6:13,2]),
          sum(dqReport()[14:29,3:4])/sum(dqReport()[14:29,2]),
          sum(dqReport()[30,3:4])/dqReport()[30,2])*100,1)
      )
      names(dqReport_short) <- c("Data Element Category", "Applicable Records","Missing","DKR", "Missing + DKR (%)")
      return(dqReport_short)
  })
  
  
  output$dqTable_short <- renderDataTable({
    dqReport_short()
  },
    options=list(
      fnRowCallback = I('
        function(nRow, aData, iDisplayIndex, iDisplayIndexFull) {
          // Column alignment
          $("td:eq(1)", nRow).css("text-align", "right");
          $("td:eq(2)", nRow).css("text-align", "right");
          $("td:eq(3)", nRow).css("text-align", "right");
          $("td:eq(4)", nRow).css("text-align", "right");
          // Make column 4 values bold
          $("td:eq(4)", nRow).css("font-weight", "bold");
          // Set conditional font colors for values in column 2
          if (parseFloat(aData[2]) > 0) {
            $("td:eq(2)", nRow).css("color", "red");
            $("td:eq(2)", nRow).css("font-weight", "bold");
          };
          // Set conditional font colors for values in column 4
          $("td:eq(4)", nRow).css("color", "green");
          if (parseFloat(aData[4]) >= 2.5) {
            $("td:eq(4)", nRow).css("color", "orange");
          };
          if (parseFloat(aData[4]) >= 5) {
            $("td:eq(4)", nRow).css("color", "red");
          };
        }
      '),
      bAutoWidth=FALSE,bFilter=0,bPaginate=0,bLengthChange=0,bSort=0,bInfo=0,iDisplayLength=29,
      aoColumns=list(list(bSearchable=FALSE),list(bSearchable=FALSE),list(bSearchable=FALSE),
        list(bSearchable=FALSE),list(bSearchable=FALSE))
    )
  )

  output$dqTable <- renderDataTable({
    dqReport()
  },
    options=list(
      fnRowCallback = I('
        function(nRow, aData, iDisplayIndex, iDisplayIndexFull) {
          // Column alignment
          $("td:eq(1)", nRow).css("text-align", "right");
          $("td:eq(2)", nRow).css("text-align", "right");
          $("td:eq(3)", nRow).css("text-align", "right");
          $("td:eq(4)", nRow).css("text-align", "right");
          // Make column 4 values bold
          $("td:eq(4)", nRow).css("font-weight", "bold");
          // Set conditional font colors for values in column 2
          if (parseFloat(aData[2]) > 0) {
            $("td:eq(2)", nRow).css("color", "red");
            $("td:eq(2)", nRow).css("font-weight", "bold");
          };
          // Set conditional font colors for values in column 4
          $("td:eq(4)", nRow).css("color", "green");
          if (parseFloat(aData[4]) >= 2.5) {
            $("td:eq(4)", nRow).css("color", "orange");
          };
          if (parseFloat(aData[4]) >= 5) {
            $("td:eq(4)", nRow).css("color", "red");
          };
        }
      '),
      bAutoWidth=FALSE,bFilter=0,bPaginate=0,bLengthChange=0,bSort=0,bInfo=0,iDisplayLength=30,
      aoColumns=list(list(bSearchable=FALSE),list(bSearchable=FALSE),list(bSearchable=FALSE),
        list(bSearchable=FALSE),list(bSearchable=FALSE))
    )
  )
  
  progsTable <- reactive({
    if (progCount2()==0) return()
    dqReportProgs <- dataQuality()[,c("PROGRAM_KEY","AGENCY_NAME","PROGRAM_NAME","CLIENTS","ENROLLS","APP_REC","PCT_MISSDKR")]
    dqReportProgs <- dqReportProgs[order(-dqReportProgs[,7]),]
    dqReportProgs[,7] <- round(dqReportProgs[,7]*100,1)
    names(dqReportProgs) <- c("Program Key","Agency Name","Program Name","Clients","Enrollments","Applicable Records","Missing + DKR (%)")
    return(dqReportProgs)
  })
  
  output$progsTable <- renderDataTable({
    progsTable()
  },
    options=list(
      fnRowCallback = I('
        function(nRow, aData, iDisplayIndex, iDisplayIndexFull) {
          // Column alignment
          $("td:eq(3)", nRow).css("text-align", "right");
          $("td:eq(4)", nRow).css("text-align", "right");
          $("td:eq(5)", nRow).css("text-align", "right");
          $("td:eq(6)", nRow).css("text-align", "right");
          // Make column 6 values bold
          $("td:eq(6)", nRow).css("font-weight", "bold");
          // Set conditional font colors for values in column 6
          $("td:eq(6)", nRow).css("color", "green");
          if (parseFloat(aData[6]) >= 2.5) {
            $("td:eq(6)", nRow).css("color", "orange");
          };
          if (parseFloat(aData[6]) >= 5) {
            $("td:eq(6)", nRow).css("color", "red");
          };
        }
      '),
      bAutoWidth=FALSE,bPaginate=0,bLengthChange=0,bInfo=0,iDisplayLength=500,
      aoColumns=list(list(bSearchable=FALSE),list(bSearchable=FALSE),list(bSearchable=FALSE),
        list(bSearchable=FALSE),list(bSearchable=FALSE),list(bSearchable=FALSE),list(bSearchable=FALSE))
    )
  )
  
  output$Violations <- renderDataTable({
    Violations()
  },
    options=list(
      fnRowCallback = I('
        function(nRow, aData, iDisplayIndex, iDisplayIndexFull) {
          // Column alignment
          $("td:eq(1)", nRow).css("text-align", "center");
          $("td:eq(2)", nRow).css("text-align", "center");
          $("td:eq(3)", nRow).css("text-align", "center");
          $("td:eq(4)", nRow).css("text-align", "center");
          $("td:eq(5)", nRow).css("text-align", "center");
          $("td:eq(6)", nRow).css("text-align", "center");
          $("td:eq(7)", nRow).css("text-align", "center");
          $("td:eq(8)", nRow).css("text-align", "center");
          $("td:eq(9)", nRow).css("text-align", "center");
          $("td:eq(10)", nRow).css("text-align", "center");
        }
      '),
      bAutoWidth=FALSE,bFilter=0,bPaginate=0,bLengthChange=0,bInfo=0,iDisplayLength=500,
      aoColumns=list(list(bSearchable=FALSE),list(bSearchable=FALSE),list(bSearchable=FALSE),
        list(bSearchable=FALSE),list(bSearchable=FALSE),list(bSearchable=FALSE),list(bSearchable=FALSE),
        list(bSearchable=FALSE),list(bSearchable=FALSE),list(bSearchable=FALSE),list(bSearchable=FALSE))
    )
  )
  
