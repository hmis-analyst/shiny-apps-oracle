  # APP: VETERANS
  #------------------------------------
  #------------------------------------

  #################################
  # MAIN QUERY
  #################################

  # Store query results into a reactive data frame
  queryResults <- reactive({
    input$update
    progress <- Progress$new(session)
    progress$set(message="Retrieving program data",detail="Please wait a moment...")
    # Query Veteran breakdown based on user selections
    isolate(
      queryResults <- dbGetQuery(connection,paste("
                                                  SELECT
                                                  count(unique case when Veteran_Status_Code = 0 then CI.Client_Key end) NonVeteran,
                                                  count(unique case when Veteran_Status_Code = 1 then CI.Client_Key end) Veteran
                                                  FROM Program_Enrollment PE
                                                  JOIN Client_Information CI
                                                  on PE.Client_Key = CI.Client_Key
                                                  JOIN Program_Community_Information PCI
                                                  on PE.Program_Key = PCI.Program_Key
                                                  JOIN Community_Group_Information CGI
                                                  on PCI.Group_Key = CGI.Group_Key
                                                  JOIN Program_Profile_Info PPI
                                                  on PE.Program_Key = PPI.Program_Key
                                                  WHERE
                                                  (Program_Exit_Date >= to_date('",beginSelect(),"','yyyy-mm-dd') or Program_Exit_Date is null) and
                                                  Program_Entry_Date <= to_date('",endSelect(),"','yyyy-mm-dd') and ",
                                                  finalSelect_Table(),input$reportLevel,"_Key=",finalSelect_Key()
                                                  ,sep="")
      )
    )
    progress$close()
    return(queryResults)
  })

