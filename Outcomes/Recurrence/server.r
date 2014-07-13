# TITLE: HOMELESSNESS RECURRENCE
#------------------------------------
#------------------------------------

# .libPaths("~/R/win-library/3.1")
# install.packages("RJDBC",repos="http://cran.rstudio.com/")
# install.packages("ggplot2",repos="http://cran.rstudio.com/")
# install.packages("reshape2",repos="http://cran.rstudio.com/")
# install.packages("stringr",repos="http://cran.rstudio.com/")
#-------------------
# Load relevant packages
library(shinyIncubator)
library(RJDBC)
library(ggplot2)
library(reshape2)
library(stringr)
library(plyr)
#-------------------
libPath1 <- "~/HMIS Data Analyst/lib/"
libPath2 <- "../../lib/"

# Establish JDBC connection using RJDBC
source(paste(libPath1,"conn-Ora-Georgia_Base.r",sep=""),local=TRUE)
#-------------------
# Create a function to modify date format
dateMod <- function(x) {paste(substr(x,6,7),"/",substr(x,9,10),"/",substr(x,1,4),sep="")}
#-------------------
# Define server logic required to query/graph HMIS gender data
shinyServer(function(input, output, session) {
  #################################
  # USER SELECTIONS
  #################################
  #-------------------
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
    selectInput("programSelect", "Program name",as.list(as.character(
      dbGetQuery(connection,paste("
        SELECT unique Program_Name
        FROM Program_Profile_Info PPI 
        WHERE Agency_Name = '",str_replace_all(input$agencySelect,"'","''"),"' and
          Program_Type_Code in (1,2,3,14)
        ORDER BY Program_Name"
      ,sep=""))[[1]]
    )))
  })
  # Store the name of the final group/agency/program selection
  finalSelect_Text <- reactive({
    # Take a dependency on input$update by reading it. (Nothing is actually done with the value.)
    input$update
    #Reactivity is invalidated unless update button is pressed
    isolate(
      if(input$reportLevel=="Group") {
        input$groupSelect
      } else{
        if(input$reportLevel=="Agency") {
          input$agencySelect
        } else {
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
        } else {
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
        } else {
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
          } else {
            paste("PPI.Agency_Name='",str_replace_all(input$agencySelect,"'","''"),"'",sep="")
          },
          if(input$reportLevel=="Program") {
            paste(" and PPI.Program_Name='",str_replace_all(input$programSelect,"'","''"),"'",sep="")
          }
      ,sep=""))[[1]]
    )
  })
  # Determine the program types associated with the user's group/agency/program selection
  # Recurrence is only relevant to housing programs, so at most the program list includes ES, TH, PSH, and RRH programs
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
        Program_Type_Code in (1,2,3,14) and ",
        finalSelect_Table(),input$reportLevel,"_Key=",finalSelect_Key(),"
      ORDER BY Program_Type",
    sep=""))[[1]])
  })
  # Create UI for program type list that displays error message if no programs exist
  output$programTypes <- renderUI({
    if(progCount()==0) return(HTML(paste("<p>Program types</p><p><em>This",input$reportLevel,"has no programs")))
    checkboxGroupInput("programTypes","Program types",as.list(programTypes_Reduced()),
      selected=programTypes_Reduced())
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
  #-------------------
  # Calculate recurrence window
  recWin <- reactive({
    Sys.Date()-2-input$daterange[2]
  })
  
  #-------------------
  #################################
  # RECURRENCE ALGORITHM
  #################################
  #-------------------
  # STEP 1: QUERYING THE INITIAL SAMPLE
  # Retrieve all clients who "left homelessness" during the report period
  # See "Rules for leaving homelessness" at https://github.com/hmis-analyst/shiny-apps-oracle/wiki/Homelessness-Recurrence
  Exits <- reactive({
    input$update
    progress <- Progress$new(session)
    progress$set(message="Retrieving program enrollments",detail="Please wait a moment...")
    isolate(
      Exits <- dbGetQuery(connection,paste("
        /* Query the basic information needed for each enrollment */
        SELECT unique
          1 counter,
          Program_Enrollment_Key,
          Client_Key, 
          Household_Key,
          Program_Key,
          Agency_Name,
          Program_Name,
          Program_Type,
          Program_Type_Code,
          Program_Exit_Date,
          Destination_Code,
          DC.Description Destination,
          case when Destination_Code in (120, 119, 117, 118, 116, 113, 23, 22, 21, 20, 19, 11, 10, 3) then 1 else 0 end Dest_Permanent,
          case when Destination_Code in (4, 5, 6, 7, 15, 106, 107, 109) then 1 else 0 end Dest_Institutional,
          case when Destination_Code in (807, 806, 122, 14, 13, 12) then 1 else 0 end Dest_Temporary,
          case when Destination_Code is null or Destination_Code in (8,9,17) then 1 else 0 end Dest_Unknown,
          Program_Exit_Date - Program_Entry_Date + 1 Length_of_Stay
        FROM Program_Enrollment PE
        JOIN Program_Community_Information PCI
          on PE.Program_Key = PCI.Program_Key
        JOIN Community_Group_Information CGI
          on PCI.Group_Key = CGI.Group_Key 
        JOIN Program_Profile_Info PPI
          on PE.Program_Key = PPI.Program_Key
        LEFT JOIN Destination_Codes DC
          on PE.Destination_Code = DC.Code_Key
        WHERE 
          /* Only include enrollments that have a program exit date within the report period */
          Program_Exit_Date >= to_date('",beginSelect(),"','yyyy-mm-dd') and
          Program_Entry_Date <= to_date('",endSelect(),"','yyyy-mm-dd') and
          /* Set conditions for a client leaving homelessness, according to program type */
          (
            /* Rule L.1.a */
            (
              Program_Type_Code in (1,2) and 
              Destination_Code in (3, 10, 11, 12, 13, 14, 19, 20, 21, 22, 23, 113, 116, 117, 118, 119, 120, 122, 806, 807)
            ) or 
            /* Rule L.1.b. */
            (
              Program_Type_Code in (3,14) and 
              (
                Program_Exit_Date - Program_Entry_Date + 1 >= 30 or 
                Destination_Code in (3, 10, 11, 12, 13, 14, 19, 20, 21, 22, 23, 113, 116, 117, 118, 119, 120, 122, 806, 807)
              )
            )
          ) and ",
          finalSelect_Table(),input$reportLevel,"_Key=",finalSelect_Key(),programTypesSelect()
      ,sep=""))
    )
    progress$close()
    return(Exits)
  })
  #-------------------
  # STEP 2: TESTING FOR CLIENT RECURRENCE
  # See "Rules for returning to homelessness" at https://github.com/hmis-analyst/shiny-apps-oracle/wiki/Homelessness-Recurrence
  Exits2 <- reactive({
    input$update
    progress <- Progress$new(session)
    Exits2 <- Exits()
    # Get number of observations in Exits, store in "n"
    n <- length(Exits2[,1])
    if(n==0 | is.na(Exits2[1,"CLIENT_KEY"])) return(Exits2)
    isolate(
      # Loop through every enrollment in Exits2 and determine, client by client, whether each came back into HMIS as homeless.
      # The result of this procedure is a new binary variable for recurrence (1 = did return, 0 = did not return).
      for (i in 1:n) {
        # Retrieve the observation's client key, store in "Client"
        Client <- Exits2[i,"CLIENT_KEY"]
        # Retrieve the date of enrollment termination, store in "Date"
        Date <- substr(Exits2[i,"PROGRAM_EXIT_DATE"],0,10)
        # Establishing that a client has returned to homelessness, Rule L.2.b
        # "[R]e-entry into the same program type within 30 days of program exit ... is not counted as a return to homelessness."
        # Only applies to clients leaving TH, PSH, or RRH programs.
        Rule_R2b <- data.frame(ProgLimit="",Wait30="")
        Rule_R2b[,1:2] <- 
          if(!is.na(Exits2[i,"DESTINATION_CODE"])) {
            # Destination code #20 is "Rental by client with other (non-VASH ongoing housing subsidy)"
            # Destination category 20 could refer to a rapid re-housing enrollment.
            if(Exits2[i,"DESTINATION_CODE"] == 20) {
              c("and not Program_Type_Code = 14","+30")
            } else {
              # Permanent supportive housing
              if(Exits2[i,"DESTINATION_CODE"] == 3) {
                c("and not Program_Type_Code = 3","+30")
              } else {c("","")}
            }
          } else {c("","")}
        # Queries the next homeless program entry date for the current client (if such an entry exists)
        # Also collects other information about the client's next enrollment.
        #-------------------
        Temp <- dbGetQuery(connection, paste("
          SELECT 
            /* Length of stay codes represent ranges of days. Convert these codes to the lower bounds of the ranges. 
               Then subtract from Next_Entry to obtain estimated Next_Homeless date. */
            min(Program_Entry_Date) -
              case  when Length_of_Stay_Code = 1 then 1
                    when Length_of_Stay_Code = 2 then 8
                    when Length_of_Stay_Code = 3 then 30
                    when Length_of_Stay_Code = 4 then 90
                    when Length_of_Stay_Code = 5 then 365
                when Length_of_Stay_Code is null or Length_of_Stay_Code in (8,9) then 0
              end Next_Homeless,
            min(Program_Entry_Date) Next_Entry, 
            PE.Client_Key,
            Agency_Name Next_Agency_Name,
            Program_Name Next_Program_Name,
            Program_Type Next_Program_Type,
            RTC.Description Next_Prior_Nights_Residence,
            LOSC.Description Next_PNR_Length_of_Stay,
            Length_of_Stay_Code Next_PNR_Length_of_Stay_Code
          FROM Program_Enrollment PE
          /* Join program information */
          JOIN Program_Profile_Info PPI
            on PE.Program_Key = PPI.Program_Key
          LEFT JOIN Client_Status_Information CSI
            on PE.Program_Enrollment_Key = CSI.Program_Enrollment_Key
          LEFT JOIN Residence_Type_Codes RTC
            on CSI.Prior_Nights_Residence_Code = RTC.Code_Key
          LEFT JOIN Length_of_Stay_Codes LOSC
            on CSI.Length_of_Stay_Code = LOSC.Code_Key
          WHERE 
            Collect_Stage = 1 and
            /* Only include enrollments that begin later than the original observation's program exit date */
            Program_Entry_Date > to_date('",Date,"','yyyy-mm-dd')",Rule_R2b$Wait30,"and
            /* Set conditions for returning to homelessness */
            (
              /* Rule R.1.a and Rule R.2.a */
              Program_Type_Code in (1,2,3,14) or
              /* Rule R.1.b and Rule R.2.b */
              Prior_Nights_Residence_Code in (1,16)
            ) and
            /* Only consider enrollments that share the same client key as the current observation */
            PE.Client_Key = ", Client, 
            Rule_R2b$ProgLimit,"
          GROUP BY PE.Client_Key, Agency_Name, Program_Name, Program_Type, RTC.Description,LOSC.Description, Length_of_Stay_Code
          ORDER BY Next_Homeless
        ")) 
        #-------------------
        # Establishing that a client has returned to homelessness, Rule R.2.d
        # "Recurrence takes place if a client exits to a homeless destination"
        # If this condition is met, Return_Date = Program_Exit_Date of the client's current enrollment
        Exits2[i,"Return_Date"] <- if(Exits2[i,"DESTINATION_CODE"] %in% c(1,2,16)) {
          Exits2[i,"PROGRAM_EXIT_DATE"]
        } else {
          # Otherwise, Return_Date = Next_Homeless
          if(length(Temp[1,"NEXT_HOMELESS"])==0) {NA}
          else {
            max(Temp[1,"NEXT_HOMELESS"],Exits2[i,"PROGRAM_EXIT_DATE"])
          }
        }
        # Create new Exits2 column called Next_Entry
        Exits2[i,"Next_Entry"] <- 
          if(length(Temp[1,"NEXT_ENTRY"])==0) {NA}
          else {Temp[1,"NEXT_ENTRY"]}
        # Create new Exits2 column to identify a recurrent enrollment. It is set to 0 if Return_Date is null; else, it is set to 1.
        Exits2[i,"Recurrence"] <- if(is.na(Exits2[i,"Return_Date"])) {0} else {1}
        # Create new Exits2 column to identify the Agency Name associated with the next homeless enrollment.
        Exits2[i,"Next_Agency_Name"] <- if(length(Temp[,1])==0) {NA} else {Temp[1,"NEXT_AGENCY_NAME"]}
        # Create new Exits2 column to identify the Program Name associated with the next homeless enrollment.
        Exits2[i,"Next_Program_Name"] <- if(length(Temp[,1])==0) {NA} else {Temp[1,"NEXT_PROGRAM_NAME"]}
        # Create new Exits2 column to identify the Program Type associated with the next homeless enrollment.
        Exits2[i,"Next_Program_Type"] <- if(length(Temp[,1])==0) {NA} else {Temp[1,"NEXT_PROGRAM_TYPE"]}
        # Create new Exits2 column to identify the Prior Nights Residence associated with the next homeless enrollment.
        Exits2[i,"Next_Prior_Nights_Residence"] <- if(length(Temp[,1])==0) {NA} else {Temp[1,"NEXT_PRIOR_NIGHTS_RESIDENCE"]}
        # Create new Exits2 column to identify the Prior Nights Residence Length of Stay associated with the next homeless enrollment.
        Exits2[i,"Next_PNR_Length_of_Stay"] <- if(length(Temp[,1])==0) {NA} else {Temp[1,"NEXT_PNR_LENGTH_OF_STAY"]}
        #-------------------
        # Set progress bar
        progress$set(message=paste("Testing clients for recurrence..."))
        progress$set(detail = paste(floor(i/n*100),"% complete",sep=""), value=i/n)
      }
    ) # END LOOP
    remove(Temp)
    #-------------------
    # Enrollment's termination date is converted to valid R date format
    Exits2[,"PROGRAM_EXIT_DATE"] <- as.Date(Exits2[,"PROGRAM_EXIT_DATE"])
    # Client's return date and next program entry are converted to valid R date format. (I'm not sure why it forces me to do it a different way.)
    Exits2[,"Return_Date"] <- as.Date(as.POSIXct(Exits2[,"Return_Date"], origin = "1970-01-01"))
    Exits2[,"Next_Entry"] <- as.Date(as.POSIXct(Exits2[,"Next_Entry"], origin = "1970-01-01"))
    # Calculates how many days it took for the client to return to homelessness after the enrollment's termination date
    Exits2[,"Days_Until_Return"] <- as.numeric(Exits2[,"Return_Date"] - Exits2[,"PROGRAM_EXIT_DATE"])
    #-------------------
    # Rule R.3
    isolate(Exits2[which(Exits2$Days_Until_Return>recWin()),"Recurrence"] <- 0)
    #-------------------
    # Transform Days_Until_Recurrence into a categorical variable called Recurrence_Type
    Exits2[,"Recurrence_Type"] <- 10
    if(length(Exits2[which(Exits2$Recurrence==1 & Exits2$Days_Until_Return<90),1])!=0) {
      Exits2[which(Exits2$Recurrence==1 & Exits2$Days_Until_Return<90),"Recurrence_Type"] <- 1
    }
    if(length(Exits2[which(Exits2$Recurrence==1 & Exits2$Days_Until_Return>=90 & Exits2[,"Days_Until_Return"]<180),1])!=0) {
      Exits2[which(Exits2$Recurrence==1 & Exits2$Days_Until_Return>=90 & Exits2[,"Days_Until_Return"]<180),"Recurrence_Type"] <- 2
    }
    if(length(Exits2[which(Exits2$Recurrence==1 & Exits2$Days_Until_Return>=180 & Exits2[,"Days_Until_Return"]<270),1])!=0) {
      Exits2[which(Exits2$Recurrence==1 & Exits2$Days_Until_Return>=180 & Exits2[,"Days_Until_Return"]<270),"Recurrence_Type"] <- 3
    }
    if(length(Exits2[which(Exits2$Recurrence==1 & Exits2$Days_Until_Return>=270 & Exits2[,"Days_Until_Return"]<365),1])!=0) {
      Exits2[which(Exits2$Recurrence==1 & Exits2$Days_Until_Return>=270 & Exits2[,"Days_Until_Return"]<365),"Recurrence_Type"] <- 4
    }
    if(length(Exits2[which(Exits2$Recurrence==1 & Exits2$Days_Until_Return>=365 & Exits2[,"Days_Until_Return"]<455),1])!=0) {
      Exits2[which(Exits2$Recurrence==1 & Exits2$Days_Until_Return>=365 & Exits2[,"Days_Until_Return"]<455),"Recurrence_Type"] <- 5
    }
    if(length(Exits2[which(Exits2$Recurrence==1 & Exits2$Days_Until_Return>=455 & Exits2[,"Days_Until_Return"]<545),1])!=0) {
      Exits2[which(Exits2$Recurrence==1 & Exits2$Days_Until_Return>=455 & Exits2[,"Days_Until_Return"]<545),"Recurrence_Type"] <- 6
    }
    if(length(Exits2[which(Exits2$Recurrence==1 & Exits2$Days_Until_Return>=545 & Exits2[,"Days_Until_Return"]<635),1])!=0) {
      Exits2[which(Exits2$Recurrence==1 & Exits2$Days_Until_Return>=545 & Exits2[,"Days_Until_Return"]<635),"Recurrence_Type"] <- 7
    }
    if(length(Exits2[which(Exits2$Recurrence==1 & Exits2$Days_Until_Return>=635 & Exits2[,"Days_Until_Return"]<730),1])!=0) {
      Exits2[which(Exits2$Recurrence==1 & Exits2$Days_Until_Return>=635 & Exits2[,"Days_Until_Return"]<730),"Recurrence_Type"] <- 8
    }
    if(length(Exits2[which(Exits2$Recurrence==1 & Exits2$Days_Until_Return>=730),1])!=0) {
      Exits2[which(Exits2$Recurrence==1 & Exits2$Days_Until_Return>=730),"Recurrence_Type"] <- 9
    }
    #-------------------
    progress$close()
    return(Exits2)
  })
  #-------------------
  # Rules L.1.b, L.2.b, and L.3.b
  # Exit3 removes enrollments with a "false exit" from homelessness
  # See "Rules for leaving homelessness" at https://github.com/hmis-analyst/shiny-apps-oracle/wiki/Homelessness-Recurrence
  Exits3 <- reactive({
    Exits2()[
      which(
        # Rule #1b
        ( 
          Exits2()[,"PROGRAM_TYPE_CODE"] %in% c(1,2) &
          Exits2()[,"DEST_PERMANENT"]==1 & 
          Exits2()[,"Days_Until_Return"]>=30
        ) | 
        # Rule #1c
        (
          Exits2()[,"PROGRAM_TYPE_CODE"] %in% c(1,2) &
          (
            Exits2()[,"DEST_TEMPORARY"]==1 | 
            Exits2()[,"DEST_INSTITUTIONAL"]==1
          ) & 
          Exits2()[,"Days_Until_Return"]>=90
        ) |
        # Rule #3
        (
          Exits2()[,"PROGRAM_TYPE_CODE"] %in% c(3,14) &
          Exits2()[,"LENGTH_OF_STAY"] + Exits2()[,"Days_Until_Return"] >= 30
        ) |
        # Rule #4
        Exits2()[,"Recurrence"]==0
      )
    ,]
  })
  #################################
  # FINAL CALCULATIONS
  #################################
  #-------------------
  # Recurrence rate calculation
  output$recRate <- renderText({paste("Recurrence Rate: ",round(mean(Exits3()[,"Recurrence"])*100,1),"%",sep="")})
  #-------------------
  # Count the number of false exits, if any. Display to UI.
  output$exitNum <- renderText({paste("A total of",length(Exits3()[,1]),"exits from homelessness were assessed.",
    if(length(Exits2()[,1])-length(Exits3()[,1])!=0) {paste("This EXCLUDES",length(Exits2()[,1])-length(Exits3()[,1]),"false exits.")})})
  #-------------------
  # Count the number of instances of recurrence
  Recurrence <- reactive({sum(Exits3()[,"Recurrence"])})
  #-------------------
  #################################
  # TABLES
  #################################
  #-------------------
  enrollTable <- reactive({
    enrollTable <- Exits3()[,!(names(Exits3()) %in% c("COUNTER","DESTINATION_CODE","DEST_TEMPORARY","DEST_INSTITUTIONAL","DEST_PERMANENT","DEST_UNKNOWN"))]
  }
  )
  #-------------------
  output$barData <- renderDataTable({
    if(Recurrence()==0) return()
    barData <- barData()[,c("label","num","rate")]
    barData[which(is.na(barData$num)),c("num","rate")] <- 0
    totalRow <- data.frame(label="TOTAL",num=sum(barData$num),rate=sum(barData$rate))
    barData <- rbind(barData,totalRow)
    names(barData) <- c("Months Until Recurrence","Instances of Recurrence","% of Exits")
    return(barData)
  },options=list(
      fnRowCallback = I('
        function(nRow, aData, iDisplayIndex, iDisplayIndexFull) {
          // Column alignment
          $("td:eq(0)", nRow).css("text-align", "left");
          $("td:eq(1)", nRow).css("text-align", "right");
          $("td:eq(2)", nRow).css("text-align", "right");
        }
      '),
      bAutoWidth=FALSE,bPaginate=0,bLengthChange=0,bInfo=0,bSort=0,bFilter=0,
      aoColumns=list(list(bSearchable=FALSE),list(bSearchable=FALSE),list(bSearchable=FALSE))
    )
  )
  #-------------------
  progsTable <- reactive({
    progsTable <- 
      ddply(
        Exits3(),
        c("PROGRAM_KEY","AGENCY_NAME","PROGRAM_NAME","PROGRAM_TYPE"),
        summarize,
        numTot=length(Recurrence),
        numRec=sum(Recurrence),
        rate=round(mean(Recurrence)*100,1)
      )
    names(progsTable) <- c("PROGRAM KEY","AGENCY NAME","PROGRAM NAME","PROGRAM TYPE","EXITS FROM HOMELESSNESS","INSTANCES OF RECURRENCE","RECURRENCE %")
    return(progsTable)
  })
  output$progsTable <- renderDataTable({
    progsTable()
  },options=list(
      fnRowCallback = I('
        function(nRow, aData, iDisplayIndex, iDisplayIndexFull) {
          // Column alignment
          $("td:eq(0)", nRow).css("text-align", "left");
          $("td:eq(1)", nRow).css("text-align", "left");
          $("td:eq(2)", nRow).css("text-align", "left");
          $("td:eq(3)", nRow).css("text-align", "left");
          $("td:eq(4)", nRow).css("text-align", "right");
          $("td:eq(5)", nRow).css("text-align", "right");
          $("td:eq(6)", nRow).css("text-align", "right");
        }
      '),
      aoColumns=list(
        list(bSearchable=FALSE),list(bSearchable=FALSE),list(bSearchable=FALSE),list(bSearchable=TRUE),list(bSearchable=FALSE),list(bSearchable=FALSE),list(bSearchable=FALSE)
      )
    )
  )  
  output$progsTable2 <- renderDataTable({
    progsTable()
  },options=list(
      fnRowCallback = I('
        function(nRow, aData, iDisplayIndex, iDisplayIndexFull) {
          // Column alignment
          $("td:eq(0)", nRow).css("text-align", "left");
          $("td:eq(1)", nRow).css("text-align", "left");
          $("td:eq(2)", nRow).css("text-align", "left");
          $("td:eq(3)", nRow).css("text-align", "left");
          $("td:eq(4)", nRow).css("text-align", "right");
          $("td:eq(5)", nRow).css("text-align", "right");
          $("td:eq(6)", nRow).css("text-align", "right");
        }
      '),
      bAutoWidth=FALSE,bFilter=0,bPaginate=0,bLengthChange=0,bSort=0,bInfo=0,iDisplayLength=500,
      aoColumns=list(
        list(bSearchable=FALSE),list(bSearchable=FALSE),list(bSearchable=FALSE),list(bSearchable=FALSE),list(bSearchable=FALSE),list(bSearchable=FALSE),list(bSearchable=FALSE)
      )
    )
  )  
  
  
  #################################
  # PLOT
  #################################
  
  barData <- reactive({
    frame <- data.frame(
      label = c("0-3", "3-6", "6-9", "9-12", "12-15","15-18","18-21","21-24",">= 24", "None"),
      Recurrence_Type = seq(1:10),
      Recurrence_Type_Txt = c("A","B","C","D","E","F","G","H","I","J"),
      col = c("#67000D","#A50F15","#CB181D","#EF3B2C","#FB6A4A","#FC9272","#FCBBA1","#FEE0D2","#FFF5F0","#BAE4B3")
    )
    Exits_Summary <- ddply(Exits3()[which(Exits3()[,"Recurrence_Type"]!=10),],"Recurrence_Type",summarize,num=length(Recurrence))
    barData <- merge(frame[1:max(Exits_Summary$Recurrence_Type),],Exits_Summary,by="Recurrence_Type",all.x=TRUE)
    barData <- barData[order(barData[,"Recurrence_Type"]),]
    barData[,"rate"] <- round(barData[,"num"]/length(Exits3()[,1])*100,1)
    return(barData)
  })
  
  output$barChart <- renderPlot({
    if(Recurrence()==0) return()
    print(barChart <- ggplot(data=barData(),aes(x=Recurrence_Type_Txt,y=rate,fill=Recurrence_Type_Txt)) + 
      geom_bar(color="black",stat="identity") +
      scale_fill_manual(
        values=as.character(barData()[,"col"])
      ) +
      theme(legend.position="none", axis.text.x = element_text(angle = 45, hjust = 1,size=18),axis.text.y = element_text(size=18),
        axis.title.x = element_text(size=18),axis.title.y = element_text(size=18),plot.title = element_text(size =18)) +
      scale_x_discrete(breaks=barData()[,"Recurrence_Type_Txt"], labels=barData()[,"label"]) +
      ggtitle("The Swiftness of Recurrence") +
      ylab("% of exits") +
      xlab("Months until recurrence")
  )})
  
  #################################
  # MAIN PANEL UI
  #################################
  
  output$mainPanel <- renderUI({
    # Take a dependency on input$update by reading it. (Nothing is actually done with the value.)
    input$update
    if (input$printable==TRUE) {
      #Reactivity is invalidated unless update button is pressed
      isolate(div(
        h2("OUTCOMES REPORT: RECURRENCE",align="center"), br(),
        (if(input$reportLevel=="Group") 
        {h3(input$groupSelect,align="center")}),
        (if(input$reportLevel %in% c("Agency","Program")) 
        {h3(input$agencySelect,align="center")}),
        (if(input$reportLevel=="Program") 
        {h3(input$programSelect,align="center")}),
        br(),
        h4(paste("Report Period:  ",dateMod(beginSelect()),"-",dateMod(endSelect())),align="center"),
        h3(class="break","Summary",align="center"),
        fluidRow(
          column(5,
            dataTableOutput("barData")
          ),
          column(6,offset=1,
            div(plotOutput("barChart"),align="center")
          )
        ),
        if(input$reportLevel!="Program") {
          div(
            h3(class="break","Programs",align="center"),
            dataTableOutput("progsTable2")
          )
        }
      ))
    } else {
      isolate(div(
        if(input$reportLevel!="Program") {div(
          h4("Main Panel",align="center"),
          tabsetPanel(
            tabPanel("Summary",
              div(downloadButton("downloadEnrolls","Download Enrollments"),align="right"),
              h4(textOutput("recRate")),
              HTML(
                paste("<p>Based on your <i>end date</i> selection, the 
                  <a href='https://github.com/hmis-analyst/shiny-apps-oracle/wiki/Homelessness-Recurrence#terms-to-remember' target='_blank'>recurrence window</a> 
                  was<strong>",recWin(),"days</strong></p>"
                )
              ),
              p(textOutput("exitNum")),
              br(),
              fluidRow(
                column(5,
                  dataTableOutput("barData")
                ),
                column(6,offset=1,
                  plotOutput("barChart")
                )
              )
            ),
            tabPanel("Programs",
              div(
                div(downloadButton("downloadProgs","Download Programs"),align="right"),
                br(),
                dataTableOutput("progsTable")
              )
            )
          )
        )} else{div(
          h4("Main Panel",align="center"),
          tabsetPanel(
            tabPanel("Summary",
              div(downloadButton("downloadEnrolls","Download Enrollments"),align="right"),
              h4(textOutput("recRate")),
              HTML(
                paste("<p>Based on your <i>end date</i> selection, the 
                  <a href='https://github.com/hmis-analyst/shiny-apps-oracle/wiki/Homelessness-Recurrence#terms-to-remember' target='_blank'>recurrence window</a> 
                  was<strong>",recWin(),"days</strong></p>"
                )
              ),
              p(textOutput("exitNum")),
              br(),
              fluidRow(
                column(5,
                  dataTableOutput("barData")
                ),
                column(6,offset=1,
                  plotOutput("barChart")
                )
              )
            )
          )
        )}
      ))
    } 
  })
  
  
  #################################
  # DOWNLOADS
  #################################
  
  output$downloadEnrolls <- downloadHandler(
    filename = function() {
      paste('enrolls-', Sys.Date(), '.csv', sep='')
    },
    content = function(file) {
      write.csv(enrollTable(), file,na="",row.names=FALSE)
    }
  )
  
  output$downloadProgs <- downloadHandler(
    filename = function() {
      paste('progs-', Sys.Date(), '.csv', sep='')
    },
    content = function(file) {
      write.csv(progsTable(), file,na="",row.names=FALSE)
    }
  )
})  
