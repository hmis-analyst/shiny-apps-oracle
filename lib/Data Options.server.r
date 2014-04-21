# Store report level selection
  reportLevel <- reactive({
    # Take a dependency on input$update by reading it. (Nothing is actually done with the value.)
    input$update
    #Reactivity is invalidated unless update button is pressed
    isolate(input$reportLevel)
  })
  output$text1 <- renderText({ 
    # Take a dependency on input$update by reading it. (Nothing is actually done with the value.)
    input$update
    #Reactivity is invalidated unless update button is pressed
    isolate(ifelse(input$reportLevel=="Program","Clients","Programs"))
  })
  # Store date range selections
  beginSelect <- reactive({
    # Take a dependency on input$update by reading it. (Nothing is actually done with the value.)
    input$update
    #Reactivity is invalidated unless update button is pressed
    isolate(input$daterange[1])
  })
  endSelect <- reactive({
    # Take a dependency on input$update by reading it. (Nothing is actually done with the value.)
    input$update
    #Reactivity is invalidated unless update button is pressed
    isolate(input$daterange[2])
  })
  # Query program names into a reactive select list based on agency selection
  # Then make available for output into UI
  output$programChoices <- renderUI({
    selectInput("programSelect", "Program name",as.list(as.character(
      dbGetQuery(connection,paste("
        SELECT unique Program_Name
        FROM Program_Profile_Info PPI 
        WHERE Agency_Name = '",str_replace_all(input$agencySelect,"'","''"),"' 
        ORDER BY Program_Name"
      ,sep=""))[[1]]
    )))
  })
  progCount <- reactive({
    dbGetQuery(connection,paste("
      SELECT count(PPI.Program_Key)
      FROM Program_Profile_Info PPI 
      FULL JOIN Program_Community_Information PCI 
        on PPI.Program_Key = PCI.Program_Key 
      FULL JOIN Community_Group_Information CGI 
        on PCI.Group_Key = CGI.Group_Key 
      WHERE ", 
        if(input$reportLevel=="Group") {
          paste("CGI.Group_Name='",str_replace_all(input$groupSelect,"'","''"),"'",sep="")
        } 
        else {
          paste("PPI.Agency_Name='",str_replace_all(input$agencySelect,"'","''"),"'",sep="")
        },
        if(input$reportLevel=="Program") {
          paste(" and PPI.Program_Name='",str_replace_all(input$programSelect,"'","''"),"'",sep="")
        }
    ,sep=""))[[1]]
  })
  progCount2 <- reactive({
    input$update
    isolate(
      dbGetQuery(connection,paste("
        SELECT count(PPI.Program_Key)
        FROM Program_Profile_Info PPI 
        FULL JOIN Program_Community_Information PCI 
          on PPI.Program_Key = PCI.Program_Key 
        FULL JOIN Community_Group_Information CGI 
          on PCI.Group_Key = CGI.Group_Key 
        WHERE ", 
          if(input$reportLevel=="Group") {
            paste("CGI.Group_Name='",str_replace_all(input$groupSelect,"'","''"),"'",sep="")
          } 
          else {
            paste("PPI.Agency_Name='",str_replace_all(input$agencySelect,"'","''"),"'",sep="")
          },
          if(input$reportLevel=="Program") {
            paste(" and PPI.Program_Name='",str_replace_all(input$programSelect,"'","''"),"'",sep="")
          }
      ,sep=""))[[1]]
    )
  })
  # Store the name of the final group/agency/program selection
  finalSelect_Text <- reactive({
    # Take a dependency on input$update by reading it. (Nothing is actually done with the value.)
    input$update
    #Reactivity is invalidated unless update button is pressed
    isolate(
      if(input$reportLevel=="Group") {
        input$groupSelect
      } 
      else{
        if(input$reportLevel=="Agency") {
          input$agencySelect
        } 
        else {
          input$programSelect
        }
      }
    )
  })
  # Store the SQL table prefix that corresponds to the report level selection
  finalSelect_Table <- reactive({ifelse(input$reportLevel=="Group","CGI.","PPI.")})
  # Store the group/agency/program key that corresponds to the final selection
  finalSelect_Key <- reactive({
    dbGetQuery(connection,paste("
      SELECT unique ",finalSelect_Table(),input$reportLevel,"_Key
      FROM Program_Profile_Info PPI 
      LEFT JOIN Program_Community_Information PCI 
        on PPI.Program_Key = PCI.Program_Key 
      LEFT JOIN Community_Group_Information CGI
        on PCI.Group_Key = CGI.Group_Key 
      WHERE ",
        if(input$reportLevel=="Group") {
          paste("CGI.Group_Name='",str_replace_all(input$groupSelect,"'","''"),"'",sep="")
        } 
        else {
          paste("PPI.Agency_Name='",str_replace_all(input$agencySelect,"'","''"),"'",sep="")
        },
        if(input$reportLevel=="Program") {
          paste(" and PPI.Program_Name='",str_replace_all(input$programSelect,"'","''"),"'",sep="")
        }
    ,sep=""))[[1]]
  })
  programTypes_Reduced <- reactive({
    if(progCount()==0) return()
    as.character(dbGetQuery(connection,paste("
      SELECT unique Program_Type
      FROM Program_Profile_Info PPI
      LEFT JOIN Program_Community_Information PCI 
        on PPI.Program_Key = PCI.Program_Key 
      LEFT JOIN Community_Group_Information CGI 
        on PCI.Group_Key = CGI.Group_Key
      WHERE 
        Program_Type is not null and ",
        finalSelect_Table(),input$reportLevel,"_Key=",finalSelect_Key(),"
      ORDER BY Program_Type",
    sep=""))[[1]])
  })
  output$programTypes <- renderUI({
    if(progCount()==0) return(HTML(paste("<p>Program types</p><p><em>This",input$reportLevel,"has no programs")))
    checkboxGroupInput("programTypes","Program types",as.list(programTypes_Reduced()),
      selected=programTypes_Reduced())
  })
  programTypesSelect <- reactive({
    if(input$reportLevel=="Program") return()
    programTypesSelect <- " and Program_Type in ("
    for(i in 1:length(input$programTypes)) {
      programTypesSelect <- 
        paste(programTypesSelect,"'",ifelse(i>1,",'",""),input$programTypes[i],sep="")
    }
    programTypesSelect <- paste(programTypesSelect,"')",sep="")
  })