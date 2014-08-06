
  # Passkey
  groupKeys <- reactive({
    input$update
    isolate(source(paste(libPath1,"passkeys.r",sep=""),local=TRUE))
    if(length(groupKeys)!=0) {
      if(
        isolate(dbGetQuery(connection,paste("
          SELECT count(PPI.Program_Key)
          FROM Program_Profile_Info PPI
          JOIN Program_Community_Information PCI
            on PPI.Program_Key = PCI.Program_Key
          WHERE 
            PPI.Agency_Name='",str_replace_all(input$agencySelect,"'","''"),"' and
            PPI.Program_Name='",str_replace_all(input$programSelect,"'","''"),"' and 
            Group_Key in (",groupKeys,") "
        ,sep="")))==0
      ) {
        rm(groupKeys)
      } else {
        return(groupKeys)
      }
    } else {
      rm(groupKeys)
    }
  })

  # Store report level selection
  reportLevel <- reactive({
    # Take a dependency on input$update by reading it. (Nothing is actually done with the value.)
    input$update
    #Reactivity is invalidated unless update button is pressed
    isolate(input$reportLevel)
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
    selectInput(
      inputId = "programSelect", 
      label = "Program name",
      choices = as.list(c("[Select]",as.character(
        dbGetQuery(connection,paste("
          SELECT unique Program_Name
          FROM Program_Profile_Info PPI 
          WHERE Agency_Name = '",str_replace_all(input$agencySelect,"'","''"),"' 
          ORDER BY Program_Name"
        ,sep=""))[[1]]
      ))),
      selected = "[Select]"
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
  # Count of programs associated with the user's group/agency/program selection (reactive)
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
  # Determine the program types associated with the user's group/agency/program selection
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
  programTypes_Reduced_2 <- reactive({
    input$update
    if(progCount()==0) return()
    isolate(as.character(dbGetQuery(connection,paste("
      SELECT unique Program_Type
      FROM Program_Profile_Info PPI
      LEFT JOIN Program_Community_Information PCI 
        on PPI.Program_Key = PCI.Program_Key 
      LEFT JOIN Community_Group_Information CGI 
        on PCI.Group_Key = CGI.Group_Key
      WHERE 
        Program_Type is not null and ",
        finalSelect_Table(),input$reportLevel,"_Key=",finalSelect_Key_2(),"
      ORDER BY Program_Type",
    sep=""))[[1]]))
  })
  # Create program types UI using reduced program types list
  output$programTypes <- renderUI({
    if(progCount()==0) return(HTML(paste("<p>Program types</p><p><em>This",tolower(input$reportLevel),"has no",if(input$reportLevel=="Program") {"program type"} else {"programs"})))
    checkboxGroupInput("programTypes","Program types",as.list(programTypes_Reduced()),
      selected=as.list(programTypes_Reduced()))
  })
  # Create SQL code for program types condition (to be used in Main Query)
  programTypesSelect <- reactive({
    if(input$reportLevel=="Program") return()
    programTypesSelect <- " and Program_Type in ("
    for(i in 1:length(input$programTypes)) {
      programTypesSelect <- 
        paste(programTypesSelect,"'",ifelse(i>1,",'",""),input$programTypes[i],sep="")
    }
    programTypesSelect <- paste(programTypesSelect,"')",sep="")
  })
  # Establish conditions for determining valid user choices
  validSelect <- reactive({
    input$update
    isolate(
      if(
        length(input$groupSelect)==0 |
        length(input$agencySelect)==0 | 
        length(input$programSelect)==0
      ) return(FALSE)
    )
    isolate(
      if(
        (input$reportLevel=="Group" & input$groupSelect=="[Select]") |
        (input$reportLevel=="Agency" & input$agencySelect=="[Select]") | 
        (input$reportLevel=="Program" & input$programSelect=="[Select]")
      ) {return(FALSE)} else {return(TRUE)}
    )
  })

  finalSelect_Key_2 <- reactive({
    input$update
    isolate(dbGetQuery(connection,paste("
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
    ,sep=""))[[1]])
  })
  
  
