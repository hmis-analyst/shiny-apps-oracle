  # APP: AGE
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
    # Query age breakdown based on user selections
    isolate(
      queryResult <- dbGetQuery(connection,paste("
        select age_group, count(*) from (
          SELECT
            CASE
              when trunc( months_between(sysdate, DATE_OF_BIRTH) / 12 ) <= 4 then 1
              when trunc( months_between(sysdate, DATE_OF_BIRTH) / 12 ) <= 17 then 2
              when trunc( months_between(sysdate, DATE_OF_BIRTH) / 12 ) <= 24 then 3
              when trunc( months_between(sysdate, DATE_OF_BIRTH) / 12 ) <= 34 then 4
              when trunc( months_between(sysdate, DATE_OF_BIRTH) / 12 ) <= 44 then 5
              when trunc( months_between(sysdate, DATE_OF_BIRTH) / 12 ) <= 54 then 6
              when trunc( months_between(sysdate, DATE_OF_BIRTH) / 12 ) <= 64 then 7
              when trunc( months_between(sysdate, DATE_OF_BIRTH) / 12 ) <= 999 then 8
              else 0
              end age_group
                                               
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
            finalSelect_Table(),input$reportLevel,"_Key=",finalSelect_Key(),
            "and trunc( months_between(sysdate, DATE_OF_BIRTH) / 12 ) <= 112
        )
        group by age_group
        order by age_group"
      ,sep=""))
    )
  
  progress$close()
  # x <- queryResult[1,1]
  # y <- queryResult[1,2]
  return(queryResult)
})
