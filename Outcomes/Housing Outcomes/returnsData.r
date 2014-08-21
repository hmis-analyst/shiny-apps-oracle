  # APP: HOUSING OUTCOMES
  # File: returnsAlgorithm.r
  #------------------------------------
  # Global dependencies:
  #   input$daterange
  # Local dependencies:
  #   Exits()
  #   MLE()
  # Objects created:
  #   returnWin()
  #   Exits2()
  #   output$returnRate
  #   output$exitNum
  #   Returns()
  #   Exits3()
  #   Exits4()
  #------------------------------------
  #------------------------------------

  # Calculate return window
  returnWin <- reactive({
    max(as.numeric(Sys.Date()-2-input$daterange[2]),0)
  })

  #-------------------
  # STEP 2: TESTING FOR CLIENT RETURN
  # See "Rules for returning to homelessness" at https://github.com/hmis-analyst/shiny-apps-oracle/wiki/Homeless-Returns
  Exits2 <- reactive({
    input$update
    Exits2 <- Exits()[which(Exits()$DESTINATION_CATEGORY=="Permanent housing" & Exits()$PROGRAM_TYPE_CODE %in% c(1,2,3,13,14)),]
    # Get number of observations in Exits, store in "n"
    if(length(Exits2)==0) return()
    n <- length(Exits2[,1])
    if(is.na(Exits2[1,"CLIENT_KEY"])) return(Exits2)
    progress <- Progress$new(session)
    isolate(
      # Loop through every enrollment in Exits2 and determine, client by client, whether each came back into HMIS as homeless.
      # The result of this procedure is a new binary variable for homeless return (1 = did return, 0 = did not return).
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
            /* Rule R.1.a and Rule R.2.a */
            Program_Type_Code in (1,2,3,14) and
            /* Only consider enrollments that share the same client key as the current observation */
            PE.Client_Key = ", Client, 
            Rule_R2b$ProgLimit,"
          GROUP BY PE.Client_Key, Agency_Name, Program_Name, Program_Type, RTC.Description,LOSC.Description, Length_of_Stay_Code
          ORDER BY Next_Homeless
        ")) 
        #-------------------
        # Establishing that a client has returned to homelessness, Rule R.2.d
        # "Client returns to homelessness if a client exits to a homeless destination"
        # If this condition is met, Return_Date = Program_Exit_Date of the client's current enrollment
        Exits2[i,"Return_Date"] <- 
          if(length(Temp[1,"NEXT_HOMELESS"])==0) {
            NA
          } else {
            max(Temp[1,"NEXT_HOMELESS"],Exits2[i,"PROGRAM_EXIT_DATE"])
          }
        # Create new Exits2 column called Next_Entry
        Exits2[i,"Next_Entry"] <- 
          if(length(Temp[1,"NEXT_ENTRY"])==0) {
            NA
          } else {
            Temp[1,"NEXT_ENTRY"]
          }
        # Create new Exits2 column to identify a return. It is set to 0 if Return_Date is null; else, it is set to 1.
        Exits2[i,"Return"] <- if(is.na(Exits2[i,"Return_Date"])) {0} else {1}
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
        progress$set(message=paste("Testing clients for returns..."))
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
    isolate(Exits2[which(Exits2$Days_Until_Return>returnWin()),"Return"] <- 0)
    #-------------------
    # Transform Days_Until_Return into a categorical variable called Return_Type
    Exits2[,"Return_Type"] <- 10
    if(length(Exits2[which(Exits2$Return==1 & Exits2$Days_Until_Return<90),1])!=0) {
      Exits2[which(Exits2$Return==1 & Exits2$Days_Until_Return<90),"Return_Type"] <- 1
    }
    if(length(Exits2[which(Exits2$Return==1 & Exits2$Days_Until_Return>=90 & Exits2[,"Days_Until_Return"]<180),1])!=0) {
      Exits2[which(Exits2$Return==1 & Exits2$Days_Until_Return>=90 & Exits2[,"Days_Until_Return"]<180),"Return_Type"] <- 2
    }
    if(length(Exits2[which(Exits2$Return==1 & Exits2$Days_Until_Return>=180 & Exits2[,"Days_Until_Return"]<270),1])!=0) {
      Exits2[which(Exits2$Return==1 & Exits2$Days_Until_Return>=180 & Exits2[,"Days_Until_Return"]<270),"Return_Type"] <- 3
    }
    if(length(Exits2[which(Exits2$Return==1 & Exits2$Days_Until_Return>=270 & Exits2[,"Days_Until_Return"]<365),1])!=0) {
      Exits2[which(Exits2$Return==1 & Exits2$Days_Until_Return>=270 & Exits2[,"Days_Until_Return"]<365),"Return_Type"] <- 4
    }
    if(length(Exits2[which(Exits2$Return==1 & Exits2$Days_Until_Return>=365 & Exits2[,"Days_Until_Return"]<455),1])!=0) {
      Exits2[which(Exits2$Return==1 & Exits2$Days_Until_Return>=365 & Exits2[,"Days_Until_Return"]<455),"Return_Type"] <- 5
    }
    if(length(Exits2[which(Exits2$Return==1 & Exits2$Days_Until_Return>=455 & Exits2[,"Days_Until_Return"]<545),1])!=0) {
      Exits2[which(Exits2$Return==1 & Exits2$Days_Until_Return>=455 & Exits2[,"Days_Until_Return"]<545),"Return_Type"] <- 6
    }
    if(length(Exits2[which(Exits2$Return==1 & Exits2$Days_Until_Return>=545 & Exits2[,"Days_Until_Return"]<635),1])!=0) {
      Exits2[which(Exits2$Return==1 & Exits2$Days_Until_Return>=545 & Exits2[,"Days_Until_Return"]<635),"Return_Type"] <- 7
    }
    if(length(Exits2[which(Exits2$Return==1 & Exits2$Days_Until_Return>=635 & Exits2[,"Days_Until_Return"]<730),1])!=0) {
      Exits2[which(Exits2$Return==1 & Exits2$Days_Until_Return>=635 & Exits2[,"Days_Until_Return"]<730),"Return_Type"] <- 8
    }
    if(length(Exits2[which(Exits2$Return==1 & Exits2$Days_Until_Return>=730),1])!=0) {
      Exits2[which(Exits2$Return==1 & Exits2$Days_Until_Return>=730),"Return_Type"] <- 9
    }
    progress$close()
    return(Exits2)
  })

  #################################
  # FINAL CALCULATIONS
  #################################
  #-------------------
  # Return rate calculation
  output$returnRate <- renderText({
    if(length(Exits2()[,1])==0) return()
    paste("Return Rate: ",round(mean(Exits2()[,"Return"])*100,1),"%",sep="")
  })
  #-------------------
  # Display to UI the number of permanent destinations assessed
  output$exitNum <- renderText({
    paste("A total of",length(Exits2()[,1]),"permanent destinations were assessed.")
  })
  #-------------------
  # Count the number of homeless returns
  Returns <- reactive({
    if(length(Exits2()[,1])==0) return()
    sum(Exits2()[,"Return"])
  })
  # Count the number of stable permanent destinations
  Exits3 <- reactive({
    if(length(Exits2()[,1])==0) return()
    Exits3 <- merge(
      x = Exits(),
      y = Exits2()[,c(grep("PROGRAM_ENROLLMENT_KEY",colnames(Exits2())),grep("Return_Date",colnames(Exits2())):grep("Return_Type",colnames(Exits2())))],
      by="PROGRAM_ENROLLMENT_KEY",
      all.x=TRUE
    )
    Exits3$Dest_Perm_90 <- ifelse(Exits3$DESTINATION_CATEGORY=="Permanent housing" & Exits3$Return_Type!=1, 1, 0)
    return(Exits3)
  })
  Exits4 <- reactive({
    Exits3()[which(!is.na(rowSums(Exits3()[,as.character(MLE()[2:length(MLE()[,1]),"Parameter"])]))),]
  })
