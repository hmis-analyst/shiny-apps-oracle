  # APP: HOUSING OUTCOMES
  # File: mainQueries.r
  #------------------------------------
  # Global dependencies:
  #   input$programTypes
  #   input$reportLevel
  #   beginSelect()
  #   endSelect()
  #   finalSelect_Table()
  #   finalSelect_Key()
  #   programTypesSelect()
  #   "County classifications.csv"
  #   input$mdkr
  #   groupKeys()
  # Local dependencies:
  #   MLE()
  # Objects created:
  #   sqlVars()
  #   sqlJoins()
  #   Exits()
  #   Exits_summary()
  #   Exits_progs()
  #   Exits_access()
  #------------------------------------
  #------------------------------------

  #################################
  # MAIN QUERIES
  #################################
  
  sqlVars <- reactive({
    if(isolate(wrongProgType(input$programTypes))==TRUE | input$reportLevel!="Program") return(1)
    sqlVars1 <- read.csv(paste(libPath2,"Variable descriptions.csv",sep=""),stringsAsFactors=FALSE)
    sqlVars2 <- merge(sqlVars1[,c("Variable","SQL_calc")],data.frame(Parameter=MLE()[which(MLE()$Parameter!="Rural"),"Parameter"]),by.x="Variable",by.y="Parameter")
    sqlVars3 <- paste(sqlVars2$SQL_calc, collapse=", ")
    return(sqlVars3)
  })
  sqlJoins <- reactive({
    if(isolate(wrongProgType(input$programTypes))==TRUE | input$reportLevel!="Program") return()
    sqlJoins1 <- read.csv(paste(libPath2,"Variable joins.csv",sep=""),stringsAsFactors=FALSE)
    sqlJoins2 <- merge(sqlJoins1[,c("Variable","SQL_join")],data.frame(Parameter=MLE()[which(MLE()$Parameter!="Rural"),"Parameter"]),by.x="Variable",by.y="Parameter")
    sqlJoins3 <- unique(sqlJoins2$SQL_join)
    sqlJoins4 <- paste(sqlJoins3, collapse=" ")
    return(sqlJoins4)
  })
  
  Exits <- reactive({
    if(validSelect()==FALSE) return()
    # Take a dependency on input$update by reading it. (Nothing is actually done with the value.)
    input$update
    progress <- Progress$new(session)
    progress$set(message="Retrieving client enrollments",detail="Please wait a moment...")
    #Reactivity is invalidated unless update button is pressed
    isolate(
      # Query gender breakdown  based on user selections    
      Exits <- dbGetQuery(connection,paste("
        SELECT unique
          PE.Program_Enrollment_Key,
          PE.Client_Key, 
          PE.Household_Key,
          Program_Key,
          Agency_Name,
          Program_Name,
          Program_Type,
          Program_Type_Code,
          Program_Exit_Date,
          Destination_Code,
          DC.Description Destination,
          case when Destination_Code is null or Destination_Code in (8,9,17) then 'Unknown'
               when Destination_Code in (110, 16, 2, 1, 111, 121) then 'Homeless'
               when Destination_Code in (4, 5, 6, 7, 15, 106, 107, 109) then 'Institutional'
               when Destination_Code in (807, 806, 122, 14, 13, 12) then 'Temporary housing'
               when Destination_Code in (18, 120, 119, 117, 118, 116, 113, 23, 22, 21, 20, 19, 11, 10, 3) then 'Permanent housing'
            end Destination_Category, ",
          sqlVars()," 
        FROM (
          SELECT 
            Program_Enrollment_Key, 
            Household_Key, 
            Client_Key, 
            Program_Key, 
            Program_Exit_Date, 
            Program_Entry_Date, 
            Destination_Code,
            Entry_Cash_Key,
            Exit_Cash_Key, 
            Entry_Noncash_Key,
            Exit_Noncash_Key,
            Client_Key || '_' || Household_Key Household_Client_Key 
          FROM Program_Enrollment
        ) PE
        JOIN Program_Profile_Info PPI
          on PE.Program_Key = PPI.Program_Key
        JOIN Program_Community_Information PCI
          on PE.Program_Key = PCI.Program_Key
        JOIN Community_Group_Information CGI
          on PCI.Group_Key = CGI.Group_Key 
        LEFT JOIN Destination_Codes DC
          on PE.Destination_Code = DC.Code_Key ",
        sqlJoins()," 
        WHERE
          Program_Exit_Date >= to_date('",beginSelect(),"','yyyy-mm-dd') and
          Program_Exit_Date <= to_date('",endSelect(),"','yyyy-mm-dd') and ",
          finalSelect_Table(),input$reportLevel,"_Key=",finalSelect_Key(),programTypesSelect()       
      ,sep=""))
    )
    progress$close()
    if("Rural" %in% MLE()[,1]) {
      rural <- read.csv(paste(libPath2,"County classifications.csv",sep=""))
      Exits <- merge(Exits,rural[,c("AGENCY_NAME","PROGRAM_NAME","Rural")],by=c("AGENCY_NAME","PROGRAM_NAME"),all.x=TRUE)
    }
    if(isolate(input$reportLevel=="Program")) {
      MLE <- MLE()
      logit <- MLE[1,"Estimate"]+rowSums(as.data.frame(t(MLE[2:length(MLE[,1]),"Estimate"]*t(Exits[,as.character(MLE[2:length(MLE[,1]),"Parameter"])]))))
      Exits$perm_90day_likelihood <- exp(logit)/(exp(logit)+1)
    }
    if(input$mdkr==TRUE) {
      return(Exits[which(Exits$DESTINATION_CATEGORY!="Unknown"),])
    } else {
      return(Exits)
    }
  })
  
  Exits_summary <- reactive({
    if(validSelect()==FALSE) return()
    # Take a dependency on input$update by reading it. (Nothing is actually done with the value.)
    input$update
    progress <- Progress$new(session)
    progress$set(message="Retrieving data",detail="Please wait a moment...")
    #Reactivity is invalidated unless update button is pressed
    isolate(
      # Query gender breakdown  based on user selections    
      Exits_summary <- dbGetQuery(connection,paste("
        SELECT 
          count(unique case when Destination_Code is null or Destination_Code in (8,9,17) then Program_Enrollment_Key end) Unknown,
          count(unique case when Destination_Code in (110, 16, 2, 1, 111, 121) then Program_Enrollment_Key end) Homeless,
          count(unique case when Destination_Code in (4, 5, 6, 7, 15, 106, 107, 109) then Program_Enrollment_Key end) Institutional,
          count(unique case when Destination_Code in (807, 806, 122, 14, 13, 12) then Program_Enrollment_Key end) Temporary, 
          count(unique case when Destination_Code in (18, 120, 119, 117, 118, 116, 113, 23, 22, 21, 20, 19, 11, 10, 3) then Program_Enrollment_Key end) Permanent,
          count(unique Program_Enrollment_Key) Leavers
        FROM Program_Enrollment PE
        JOIN Program_Community_Information PCI
          on PE.Program_Key = PCI.Program_Key
        JOIN Community_Group_Information CGI
          on PCI.Group_Key = CGI.Group_Key
        JOIN Program_Profile_Info PPI
          on PE.Program_Key = PPI.Program_Key
        WHERE
          Program_Exit_Date >= to_date('",beginSelect(),"','yyyy-mm-dd') and
          Program_Exit_Date <= to_date('",endSelect(),"','yyyy-mm-dd') and ",
          finalSelect_Table(),input$reportLevel,"_Key=",finalSelect_Key(),programTypesSelect() 
      ,sep=""))
    )
    progress$close()
    Exits_summary_2 <- data.frame(
      DESTINATION = c("Unknown","Homeless","Institutional","Temporary","Permanent","TOTAL"),
      LEAVERS = c(Exits_summary[1,1],Exits_summary[1,2],Exits_summary[1,3],Exits_summary[1,4],Exits_summary[1,5],Exits_summary[1,6])
    )
    if(input$mdkr==FALSE) {
      return(Exits_summary_2)
    } 
    else {
      Exits_summary_2[6,2] <- Exits_summary_2[6,2] - Exits_summary_2[1,2]
      Exits_summary_2 <- Exits_summary_2[2:6,]
      return(Exits_summary_2)
    }
  })
  
  Exits_progs <- reactive({
    if(validSelect()==FALSE) return()
    # Take a dependency on input$update by reading it. (Nothing is actually done with the value.)
    input$update
    progress <- Progress$new(session)
    progress$set(message="Retrieving data",detail="Please wait a moment...")
    #Reactivity is invalidated unless update button is pressed
    isolate(
      # Query gender breakdown  based on user selections    
      Exits_progs <- dbGetQuery(connection,paste("
        SELECT 
          PPI.Program_Key,
          Agency_Name,
          Program_Name,
          count(unique Program_Enrollment_Key) Leavers,
          count(unique case when Destination_Code is null or Destination_Code in (8,9,17) then Program_Enrollment_Key end) Unknown,
          count(unique case when Destination_Code in (18, 120, 119, 117, 118, 116, 113, 23, 22, 21, 20, 19, 
            11, 10, 3) then Program_Enrollment_Key end) Permanent
        FROM Program_Enrollment PE
        JOIN Program_Community_Information PCI
          on PE.Program_Key = PCI.Program_Key
        JOIN Community_Group_Information CGI
          on PCI.Group_Key = CGI.Group_Key
        JOIN Program_Profile_Info PPI
          on PE.Program_Key = PPI.Program_Key
        WHERE
          Program_Exit_Date >= to_date('",beginSelect(),"','yyyy-mm-dd') and
          Program_Exit_Date <= to_date('",endSelect(),"','yyyy-mm-dd') and ",
          finalSelect_Table(),input$reportLevel,"_Key=",finalSelect_Key(),programTypesSelect(),"
        GROUP BY
          PPI.Program_Key,
          Agency_Name,
          Program_Name"
      ,sep=""))
    )
    progress$close()
    names(Exits_progs) <- c("PROGRAM KEY","AGENCY NAME","PROGRAM NAME","LEAVERS","UNKNOWN DESTINATIONS (%)","PERMANENT DESTINATIONS (%)")
    if(input$mdkr==FALSE) {
      Exits_progs[,5:6] <- round(Exits_progs[,5:6]/Exits_progs[,4]*100,1)
      return(Exits_progs)
    } 
    else {
      Exits_progs[,6] <- round(Exits_progs[,6]/(Exits_progs[,4]-Exits_progs[,5])*100,1)
      Exits_progs <- Exits_progs[,!(names(Exits_progs) %in% "UNKNOWN DESTINATIONS (%)")]
      return(Exits_progs)
    }
  })
  
  Exits_access <- reactive({
    # Take a dependency on input$update by reading it. (Nothing is actually done with the value.)
    input$update
    progress <- Progress$new(session)
    progress$set(message="Preparing client enrollments",detail="Please wait a moment...")
    #Reactivity is invalidated unless update button is pressed
    isolate(
      # Query gender breakdown  based on user selections    
      Exits_access <- dbGetQuery(connection,paste("
        SELECT unique
          PE.Program_Enrollment_Key
        FROM Program_Enrollment PE
        JOIN Program_Community_Information PCI
          on PE.Program_Key = PCI.Program_Key
        JOIN Community_Group_Information CGI
          on PCI.Group_Key = CGI.Group_Key
        JOIN Program_Profile_Info PPI
          on PE.Program_Key = PPI.Program_Key
        WHERE
          PCI.Group_Key in (",groupKeys(),") and
          Program_Exit_Date >= to_date('",beginSelect(),"','yyyy-mm-dd') and
          Program_Exit_Date <= to_date('",endSelect(),"','yyyy-mm-dd') and ",
          finalSelect_Table(),input$reportLevel,"_Key=",finalSelect_Key(),programTypesSelect()       
      ,sep=""))
    )
    progress$close()
    return(Exits_access)
  })
