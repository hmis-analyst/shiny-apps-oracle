  # APP: DATA CONSISTENCY
  # File: data.r
  #------------------------------------
  # Global dependencies:
  #   beginSelect()
  #   endSelect()
  #   finalSelect_Table()
  #   input$reportLevel
  #   finalSelect_Key()
  #   programTypesSelect()
  #   groupKeys()
  # Local dependencies:
  #   [None]
  # Objects created:
  #   joins
  #   summaryData()
  #   summaryData_2()
  #   progsData()
  #   progsData_2()
  #   clientsData()
  #------------------------------------
  #------------------------------------
  
  
  joins <- "
          JOIN Program_Profile_Info PPI
          on PE.Program_Key = PPI.Program_Key
        JOIN Client_Information CI
          on PE.Client_Key = CI.Client_Key
        LEFT JOIN (
          SELECT *
          FROM Client_Status_Information
          WHERE 
            Collect_Stage = 1
          ) CSI_N
          on PE.Program_Enrollment_Key = CSI_N.Program_Enrollment_Key
        LEFT JOIN (
          SELECT *
          FROM Client_Status_Information
          WHERE 
            Collect_Stage = 3
          ) CSI_X
          on PE.Program_Enrollment_Key = CSI_X.Program_Enrollment_Key
        LEFT JOIN (
          SELECT *
          FROM Client_Special_Needs_Info
          WHERE
            Mental_Illness is not null and
            Collect_Stage = 1
          ) CSNI_N
          on PE.Program_Enrollment_Key = CSNI_N.Program_Enrollment_Key
        LEFT JOIN (
          SELECT *
          FROM Client_Special_Needs_Info
          WHERE
            Mental_Illness is not null and
            Collect_Stage = 3
          ) CSNI_X
          on PE.Program_Enrollment_Key = CSNI_X.Program_Enrollment_Key
        /*
        Creates a table called HCI_1 that summarizes household information and links it to program enrollments.
        Contents: new household (hh) variables.
          Num_Fam: Number of hh members
          Num_Child: Number of children in hh
          Num_Adult: Number of adults in hh
          Num_Teen_M: Number of teenage males in hh
          Num_Teen_F: Number of teenage females in hh
          Head_Num: Number of head of hh
          Head_Age: The age of the oldest head of hh
          Fem_Head: Number of female head of hh
          Oldest_Age: Age of the oldest hh member
          Oldest_Fem_Age: Age of the oldest female hh member
          Oldest_DoB: Oldest hh member's date of birth
        Each of these variables can be called in the main SELECT clause that the JOIN is attached to.
        */
        LEFT JOIN (
          SELECT 
            Program_Enrollment_Key,
            count(Client_Key) Num_Fam, 
            count(Child) Num_Child, 
            count(Adult) Num_Adult, 
            count(Teen_M) Num_Teen_M,
            count(Teen_F) Num_Teen_F,
            count(Head) Head_Num,
            max(Head_Age) Head_Age,
            count(Fem_Head) Fem_Head,
            max(Age) Oldest_Age,
            max(Oldest_Fem_Age) Oldest_Fem_Age,
            min(Date_of_Birth) Oldest_DoB
          FROM (
            SELECT 
              unique Program_Enrollment_Key, 
              HCI.Client_Key, 
              case when (Program_Entry_Date - Date_of_Birth)/365.25 < 18 then 1 end Child, 
              case when (Program_Entry_Date - Date_of_Birth)/365.25 >= 18 then 1 end Adult, 
              case when Relationship_Code = 3 then (Program_Entry_Date - Date_of_Birth)/365.25 end Head_Age,
              case when Relationship_Code = 3 then 1 end Head,
              case when Relationship_Code = 3 and Gender_Code = 2 then 1 end Fem_Head,
              case when Gender_Code = 2 then (Program_Entry_Date - Date_of_Birth)/365.25 else 0 end Oldest_Fem_Age,
              case when (Program_Entry_Date - Date_of_Birth)/365.25 >= 13 and (Program_Entry_Date - Date_of_Birth)/365.25 < 20 and Gender_Code = 1 
                then 1 end Teen_M,
              case when (Program_Entry_Date - Date_of_Birth)/365.25 >= 13 and (Program_Entry_Date - Date_of_Birth)/365.25 < 20 and Gender_Code = 2 
                then 1 end Teen_F,
              (Program_Entry_Date - Date_of_Birth)/365.25 Age,
              Date_of_Birth
            FROM Household_Client_Info HCI
            JOIN Client_Information CI 
              on HCI.Client_Key = CI.Client_Key
            JOIN Program_Enrollment PE
              on HCI.Household_Key = PE.Household_Key
          )
          GROUP BY Program_Enrollment_Key
        ) HCI_1
          on PE.Program_Enrollment_Key = HCI_1.Program_Enrollment_Key
        /*
        Creates a second household table with custom client-household key to link up to the Program_Enrollments table.
        Makes it possible to query the relationship code for a client during a particular enrollment.
        */
        LEFT JOIN (
          SELECT 
            Client_Key, 
            Household_Key, 
            Client_Key || '_' || Household_Key Household_Client_Key, 
            Relationship_Code 
          FROM Household_Client_Info
        ) HCI_2
          on PE.Household_Client_Key = HCI_2.Household_Client_Key
        LEFT JOIN Client_Cash_Income CCI_N
          on PE.Entry_Cash_Key = CCI_N.Cash_GK
        LEFT JOIN Client_Cash_Income CCI_X
          on PE.Exit_Cash_Key = CCI_X.Cash_GK
        LEFT JOIN Client_Noncash_Benefits CNB_N
          on PE.Entry_Noncash_Key = CNB_N.Noncash_GK
        LEFT JOIN Client_Noncash_Benefits CNB_X
          on PE.Exit_Noncash_Key = CNB_X.Noncash_GK
        /* */
        /* Create subquery establishing each client's date of first homeless occurrence in HMIS. Join to original enrollment table */
        /* */ 
        LEFT JOIN (
          SELECT 
            PE.Client_Key, 
            min(Program_Entry_Date) First_Homeless 
          FROM Program_Enrollment PE 
          JOIN Program_Profile_Info PPI 
            on PE.Program_Key = PPI.Program_Key 
          LEFT JOIN Client_Status_Information CSI 
            on PE.Program_Enrollment_Key = CSI.Program_Enrollment_Key 
          WHERE 
            (Prior_Nights_Residence_Code in (1,16) or Program_Type_Code in (1,2,3,14))
            and CSI.Collect_Stage=1
          GROUP BY PE.Client_Key
        ) PE_2
          on PE.Client_Key = PE_2.Client_Key 
        JOIN Program_Community_Information PCI
          on PE.Program_Key = PCI.Program_Key
        JOIN Community_Group_Information CGI
          on PCI.Group_Key = CGI.Group_Key 
        LEFT JOIN Destination_Codes DC
          on PE.Destination_Code = DC.Code_Key
    "
  
  summaryData <- reactive({
    # Take a dependency on input$update by reading it. (Nothing is actually done with the value.)
    input$update
    progress <- Progress$new(session)
    progress$set(message="Retrieving summary data",detail="Please wait a moment...")
    #Reactivity is invalidated unless update button is pressed
    isolate(
      summaryData <- dbGetQuery(connection,paste("
        SELECT 
          count(unique PE.Program_Enrollment_Key) Enrollments,
          /*
          /* DATA INCONSISTENCIES */
            /* Missing head of household (applicable enrollments: all) */
              count(unique case when Head_Num = 0 then PE.Program_Enrollment_Key end) HH_Head_missing,
            /* More than one head of household (applicable enrollments: all) */
              count(unique case when Head_Num > 1 then PE.Program_Enrollment_Key end) HH_Head_dup,
            /* Veteran income for non-veteran (applicable enrollments: all) */
              count(unique case when (CCI_N.VetPension = 1 or CCI_X.VetPension = 1 or CCI_N.VetDisPmt = 1 or CCI_X.VetDisPmt = 1) and CI.Veteran_Status_Code = 0 
                then PE.Program_Enrollment_Key end) VetIncome_Nonvet,
            /* Veteran benefits for non-veteran (applicable enrollments: all) */
              count(unique case when (CNB_N.VAMS = 1 or CNB_X.VAMS = 1) and CI.Veteran_Status_Code = 0 then PE.Program_Enrollment_Key end) VetBenefits_Nonvet
        FROM (
          SELECT 
            PE.*,
            Client_Key || '_' || Household_Key Household_Client_Key 
          FROM Program_Enrollment PE
        ) PE
        ",joins,"
        WHERE
          (Program_Exit_Date >= to_date('",beginSelect(),"','yyyy-mm-dd') or Program_Exit_Date is null) and
          Program_Entry_Date <= to_date('",endSelect(),"','yyyy-mm-dd') and ",
          finalSelect_Table(),input$reportLevel,"_Key=",finalSelect_Key(),programTypesSelect()
      ,sep=""))
    )
    progress$close()  
    return(summaryData)
  })
  
  summaryData_2 <- reactive({
    attach(summaryData())
    summaryData_2 <- data.frame(
      ID = c(NA, 1:(length(summaryData())-1)),
      Data_Element = c("All enrollments","Missing head of household","More than one head of household",
        "Non-veteran receiving veteran income","Non-veteran receiving veteran benefits"),
      Applicable_Records = c(ENROLLMENTS, ENROLLMENTS, ENROLLMENTS, ENROLLMENTS, ENROLLMENTS),
      Hits = c(NA, HH_HEAD_MISSING, HH_HEAD_DUP, VETINCOME_NONVET, VETBENEFITS_NONVET)
    )
    detach(summaryData())
    grandSummaryRow <- data.frame(
      ID = NA,
      Data_Element = "TOTAL",
      Applicable_Records = sum(summaryData_2[2:length(summaryData_2),"Applicable_Records"]),
      Hits = sum(summaryData_2[2:length(summaryData_2),"Hits"])
    )
    # Tack summary row to end of summaryReport
    summaryData_2b <- rbind(summaryData_2, grandSummaryRow)
    # Create column for hit percentage
    summaryData_2b$Hit_Pct <- ifelse(is.na(summaryData_2b$Hits),"",round(summaryData_2b$Hits/summaryData_2b$Applicable_Records*100,1))
    return(summaryData_2b)
  })
  
  
  progsData <- reactive({
    # Take a dependency on input$update by reading it. (Nothing is actually done with the value.)
    input$update
    progress <- Progress$new(session)
    progress$set(message="Retrieving program data",detail="Please wait a moment...")
    #Reactivity is invalidated unless update button is pressed
    isolate(
      progsData <- dbGetQuery(connection,paste("
        SELECT unique
          PE.Program_Key,
          Agency_Name,
          Program_Name,
          count(unique PE.Program_Enrollment_Key) Enrollments,
          count(unique PE.Program_Enrollment_Key)*4 Applicable_Records,
          /*
          /* COUNT OF TOTAL DATA INCONSISTENCY HITS */
            /* Missing head of household (applicable records: households) */
              count(unique case when Head_Num = 0 then PE.Household_Key end) +
            /* More than one head of household (applicable records: households) */
              count(unique case when Head_Num > 1 then PE.Household_Key end) +
            /* Veteran income for non-veteran (applicable records: program enrollments) */
              count(unique case when (CCI_N.VetPension = 1 or CCI_X.VetPension = 1 or CCI_N.VetDisPmt = 1 or CCI_X.VetDisPmt = 1) and CI.Veteran_Status_Code = 0 
                then PE.Program_Enrollment_Key end) +
            /* Veteran benefits for non-veteran (applicable records: program enrollments) */
              count(unique case when (CNB_N.VAMS = 1 or CNB_X.VAMS = 1) and CI.Veteran_Status_Code = 0 then PE.Program_Enrollment_Key end) Hits
        FROM (
          SELECT 
            PE.*,
            Client_Key || '_' || Household_Key Household_Client_Key 
          FROM Program_Enrollment PE
        ) PE
        ",joins,"
        WHERE
          (Program_Exit_Date >= to_date('",beginSelect(),"','yyyy-mm-dd') or Program_Exit_Date is null) and
          Program_Entry_Date <= to_date('",endSelect(),"','yyyy-mm-dd') and ",
          finalSelect_Table(),input$reportLevel,"_Key=",finalSelect_Key(),programTypesSelect(),"  
        GROUP BY PE.Program_Key, Agency_Name, Program_Name"
      ,sep=""))
    )
    progress$close()  
    return(progsData)
  })
  
  progsData_2 <- reactive({
    progsData_2 <- progsData()
    progsData_2$Hit_Pct <- round(progsData_2$HITS/progsData_2$APPLICABLE_RECORDS*100,1)
    progsData_2 <- progsData_2[,!(names(progsData_2)=="HITS")]
    return(progsData_2)
  })
    
    
  #################################
  # CLIENT-LEVEL DATA QUALITY
  #################################
  
  clientsData <- reactive({
    if (is.null(groupKeys())) return()
    # Take a dependency on input$update by reading it. (Nothing is actually done with the value.)
    input$update
    progress <- Progress$new(session)
    progress$set(message="Retrieving client data",detail="Please wait a moment...")
    #Reactivity is invalidated unless update button is pressed
    clientsData <- 
      isolate(dbGetQuery(connection,paste("
        SELECT 
          Client_Key, 
          HH_Head_missing, 
          HH_Head_dup, 
          VetIncome_Nonvet, 
          VetBenefits_Nonvet
        FROM (
          SELECT unique 
            Client_Key, 
            case when count(HH_Head_missing)>0 then 'x' end HH_Head_missing, 
            case when count(HH_Head_dup)>0 then 'x' end HH_Head_dup,
            case when count(VetIncome_Nonvet)>0 then 'x' end VetIncome_Nonvet,
            case when count(VetBenefits_Nonvet)>0 then 'x' end VetBenefits_Nonvet,
            count(HH_Head_missing) + count(HH_Head_dup) + count(VetIncome_Nonvet) + count(VetBenefits_Nonvet) Hits
          FROM (
            SELECT unique
              PE.Program_Enrollment_Key,
              CI.Client_Key,
              case when Name_First = 'MISSING' then CI.Client_Key end FN_M,
              /* DATA INCONSISTENCIES */
                /* Missing head of household (applicable enrollments: all) */
                  case when Head_Num = 0 then CI.Client_Key end HH_Head_missing,
                /* More than one head of household (applicable enrollments: all) */
                  case when Head_Num > 1 then CI.Client_Key end HH_Head_dup,
                /* Veteran income for non-veteran (applicable enrollments: all) */
                  case when (CCI_N.VetPension = 1 or CCI_X.VetPension = 1 or CCI_N.VetDisPmt = 1 or CCI_X.VetDisPmt = 1) and CI.Veteran_Status_Code = 0 
                    then CI.Client_Key end VetIncome_Nonvet,
                /* Veteran benefits for non-veteran (applicable enrollments: all) */
                  case when (CNB_N.VAMS = 1 or CNB_X.VAMS = 1) and CI.Veteran_Status_Code = 0 then CI.Client_Key end VetBenefits_Nonvet
            FROM (
              SELECT 
                PE.*,
                Client_Key || '_' || Household_Key Household_Client_Key 
              FROM Program_Enrollment PE
            ) PE
            ",joins,"
            WHERE 
              PCI.Group_Key in (",groupKeys(),") and
              (Program_Exit_Date >= to_date('",beginSelect(),"','yyyy-mm-dd') or 
                Program_Exit_Date is null) and
              Program_Entry_Date <= to_date('",endSelect(),"','yyyy-mm-dd') and
              PPI.Program_Key=",finalSelect_Key()," 
          )
          GROUP BY Client_Key
          ORDER BY Hits Desc
        ) 
        WHERE 
          Hits > 0
      ",sep="")))
    progress$close()
    return(clientsData)
  })
  
