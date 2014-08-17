  # APP: DATA COMPLETENESS
  #------------------------------------
  #------------------------------------
  
  #################################
  # APR FORMAT QUERY
  #################################
  
  APR_records <- reactive({
    input$update
    isolate(
      if(input$APR==TRUE) {
        return(" and num=1")
      }
    )
  })
  
  dqQuery <- reactive({
    if (progCount2()==0) return()
    # Take a dependency on input$update by reading it. (Nothing is actually done with the value.)
    input$update
    progress <- Progress$new(session)
    progress$set(message="Retrieving program data",detail="Please wait a moment...")
    #Reactivity is invalidated unless update button is pressed
    isolate(
      dqQuery <- dbGetQuery(connection,paste("
        SELECT unique PE.Program_Key,
          Agency_Name,
          Program_Name,
          count(unique PE.Program_Enrollment_Key) Enrolls,
          count(unique PE.Client_Key) Clients,
          count(unique case when (Program_Entry_Date - Date_of_Birth)/365.25 >= 18 
            then PE.Program_Enrollment_Key end) Adults,
          count(unique case when HH_Members=1 and (Program_Entry_Date - Date_of_Birth)/365.25 < 18 
            then PE.Program_Enrollment_Key end) UnaChild,
          count(unique case when Program_Exit_Date <= to_date('",endSelect(),"','yyyy-mm-dd') 
            then PE.Program_Enrollment_Key end) Leavers,
          count(unique case when (
              (Program_Type_Code = 1 and to_date('",endSelect(),"','yyyy-mm-dd') - Program_Entry_Date > 180) or 
              (Program_Type_Code = 2 and to_date('",endSelect(),"','yyyy-mm-dd') - Program_Entry_Date > 720)
            ) and 
            Program_Exit_Date is null then PE.Program_Enrollment_Key end) LongtermStayers, 
          count(unique case when Name_First = 'MISSING' then PE.Program_Enrollment_Key end) Name_F_M,
          count(unique case when Name_Last = 'MISSING' then PE.Program_Enrollment_Key end) Name_L_M,
          count(unique case when ID_Type is null then PE.Program_Enrollment_Key end) SSN_M,
          count(unique case when Date_of_Birth is null and not DOB_Type in (8,9)
            then PE.Program_Enrollment_Key end) DOB_M,
          count(unique case when Race_Code is null then PE.Program_Enrollment_Key end) Race_M,
          count(unique case when Ethnicity_Code is null then PE.Program_Enrollment_Key end) Ethn_M,
          count(unique case when Gender_Code is null then PE.Program_Enrollment_Key end) Gender_M,
          count(unique case when Veteran_Status_Code is null then PE.Program_Enrollment_Key end) Vet_M,
          count(unique case when Disabling_Condition is null then PE.Program_Enrollment_Key end) Disab_M,
          count(unique case when Prior_Nights_Residence_Code is null 
            then PE.Program_Enrollment_Key end) PNR_M,
          count(unique case when Zip_Quality_Code is null then PE.Program_Enrollment_Key end) Zip_M,
          count(unique case when Housing_Status_Code is null 
            then PE.Program_Enrollment_Key end) HStatus_M,
          count(unique case when CCI_N.Verified_Answer is null 
            then PE.Program_Enrollment_Key end) Inc_N_M,
          count(unique case when CCI_X.Verified_Answer is null and 
            Program_Exit_Date <= to_date('",endSelect(),"','yyyy-mm-dd') 
            then PE.Program_Enrollment_Key end) Inc_X_M,
          count(unique case when CNB_N.Verified_Answer is null 
            then PE.Program_Enrollment_Key end) Ben_N_M,
          count(unique case when CNB_X.Verified_Answer is null and 
            Program_Exit_Date <= to_date('",endSelect(),"','yyyy-mm-dd') 
            then PE.Program_Enrollment_Key end) Ben_X_M,
          count(unique case when Physical_Disability is null then PE.Program_Enrollment_Key end) Disab_P_M,
          count(unique case when Developmental_Disability is null then PE.Program_Enrollment_Key end) Disab_D_M,
          count(unique case when Chronic_Health_Condition is null then PE.Program_Enrollment_Key end) Disab_C_M,
          count(unique case when HIV_AIDS is null then PE.Program_Enrollment_Key end) Disab_H_M,
          count(unique case when Mental_Illness is null then PE.Program_Enrollment_Key end) Disab_M_M,
          count(unique case when Substance_Abuse is null then PE.Program_Enrollment_Key end) Disab_S_M,
          count(unique case when Dom_Vio_Survivor is null then PE.Program_Enrollment_Key end) Disab_DV_M,
          count(unique case when Program_Exit_Date <= to_date('",endSelect(),"','yyyy-mm-dd') and 
            Destination_Code is null then PE.Program_Enrollment_Key end) Dest_M,
          count(unique case when ID_Type in (2,3,4,8,9) then PE.Program_Enrollment_Key end) SSN_DKR,
          count(unique case when DOB_Type in (8,9)then PE.Program_Enrollment_Key end) DOB_DKR,
          count(unique case when Race_Code in (15,16) then PE.Program_Enrollment_Key end) Race_DKR,
          count(unique case when Ethnicity_Code in (8,9) then PE.Program_Enrollment_Key end) Ethn_DKR,
          count(unique case when Gender_Code in (8,9) then PE.Program_Enrollment_Key end) Gender_DKR,
          count(unique case when Veteran_Status_Code in (8,9) then PE.Program_Enrollment_Key end) Vet_DKR,
          count(unique case when Disabling_Condition in (8,9) then PE.Program_Enrollment_Key end) Disab_DKR,
          count(unique case when Prior_Nights_Residence_Code in (8,9) 
            then PE.Program_Enrollment_Key end) PNR_DKR,
          count(unique case when Zip_Quality_Code in (8,9) then PE.Program_Enrollment_Key end) Zip_DKR,
          count(unique case when Housing_Status_Code in (8,9) 
            then PE.Program_Enrollment_Key end) HStatus_DKR,
          count(unique case when CCI_N.Verified_Answer in (8,9) 
            then PE.Program_Enrollment_Key end) Inc_N_DKR,
          count(unique case when CCI_X.Verified_Answer in (8,9) and 
            Program_Exit_Date <= to_date('",endSelect(),"','yyyy-mm-dd') 
            then PE.Program_Enrollment_Key end) Inc_X_DKR,
          count(unique case when CNB_N.Verified_Answer in (8,9) 
            then PE.Program_Enrollment_Key end) Ben_N_DKR,
          count(unique case when CNB_X.Verified_Answer in (8,9) and 
            Program_Exit_Date <= to_date('",endSelect(),"','yyyy-mm-dd') 
            then PE.Program_Enrollment_Key end) Ben_X_DKR,
          count(unique case when Physical_Disability in (8,9) 
            then PE.Program_Enrollment_Key end) Disab_P_DKR,
          count(unique case when Developmental_Disability in (8,9) 
            then PE.Program_Enrollment_Key end) Disab_D_DKR,
          count(unique case when Chronic_Health_Condition in (8,9) 
            then PE.Program_Enrollment_Key end) Disab_C_DKR,
          count(unique case when HIV_AIDS in (8,9) then PE.Program_Enrollment_Key end) Disab_H_DKR,
          count(unique case when Mental_Illness in (8,9) then PE.Program_Enrollment_Key end) Disab_M_DKR,
          count(unique case when Substance_Abuse in (8,9) then PE.Program_Enrollment_Key end) Disab_S_DKR,
          count(unique case when Dom_Vio_Survivor in (8,9) then PE.Program_Enrollment_Key end) Disab_DV_DKR,
          count(unique case when Program_Exit_Date <= to_date('",endSelect(),"','yyyy-mm-dd') and 
            Destination_Code in (8,9) then PE.Program_Enrollment_Key end) Dest_DKR         
        FROM (
          SELECT
            PE.*,
            row_number() over(partition by Client_Key order by Program_Entry_Date desc) num
          FROM Program_Enrollment PE
          JOIN Program_Profile_Info PPI
            on PE.Program_Key = PPI.Program_Key
          FULL JOIN Program_Community_Information PCI
            on PE.Program_Key = PCI.Program_Key
          FULL JOIN Community_Group_Information CGI
            on PCI.Group_Key = CGI.Group_Key
          WHERE
            (Program_Exit_Date >= to_date('",beginSelect(),"','yyyy-mm-dd') or Program_Exit_Date is null) and
            Program_Entry_Date <= to_date('",endSelect(),"','yyyy-mm-dd') and ",
            finalSelect_Table(),input$reportLevel,"_Key=",finalSelect_Key(),programTypesSelect(),"
        ) PE
        JOIN Client_Information CI
          on PE.Client_Key = CI.Client_Key
        JOIN Program_Profile_Info PPI
          on PE.Program_Key = PPI.Program_Key
        FULL JOIN Program_Community_Information PCI
          on PE.Program_Key = PCI.Program_Key
        FULL JOIN Community_Group_Information CGI
          on PCI.Group_Key = CGI.Group_Key
        LEFT JOIN (
          SELECT *
          FROM Client_Status_Information
          WHERE Collect_Stage=1
        ) CSI
          on PE.Program_Enrollment_Key = CSI.Program_Enrollment_Key
        LEFT JOIN Client_Cash_Income CCI_N
          on PE.Entry_Cash_Key = CCI_N.Cash_GK
        LEFT JOIN Client_Cash_Income CCI_X
          on PE.Exit_Cash_Key = CCI_X.Cash_GK
        LEFT JOIN Client_Noncash_Benefits CNB_N
          on PE.Entry_Noncash_Key = CNB_N.Noncash_GK
        LEFT JOIN Client_Noncash_Benefits CNB_X
          on PE.Exit_Noncash_Key = CNB_X.Noncash_GK
        LEFT JOIN (
          SELECT 
            Household_Key,
            count(Household_Key) HH_Members
          FROM Household_Client_Info HCI 
          GROUP BY Household_Key
        ) HCI
          on PE.Household_Key = HCI.Household_Key
        LEFT JOIN (
          SELECT *
          FROM Client_Special_Needs_Info 
          WHERE Collect_Stage=1 and Group_Key is not null
        ) CSNI
          on PE.Program_Enrollment_Key = CSNI.Program_Enrollment_Key
        WHERE
          (Program_Exit_Date >= to_date('",beginSelect(),"','yyyy-mm-dd') or Program_Exit_Date is null) and
          Program_Entry_Date <= to_date('",endSelect(),"','yyyy-mm-dd') and ",
          finalSelect_Table(),input$reportLevel,"_Key=",finalSelect_Key(),programTypesSelect(),APR_records(),"  
        GROUP BY PE.Program_Key, Agency_Name, Program_Name
      ",sep=""))
    )
    progress$close()    
    return(dqQuery)
  })
  
  # Summary
  dataQuality <- reactive({
    dataQuality <- dqQuery()
    if(sum(grepl("ERROR",dataQuality))==0) {
      dataQuality[,"APP_REC"] <- dataQuality$ENROLLS*20+dataQuality$ADULTS+
        dataQuality$LEAVERS*3
      dataQuality[,"SUM_MISS"] <- rowSums(
        dataQuality[,grep("^NAME_F_M$",colnames(dataQuality)):grep("^DEST_M$",colnames(dataQuality))])
      dataQuality[,"PCT_MISS"] <- rowSums(
        dataQuality[,grep("^NAME_F_M$",colnames(dataQuality)):grep("^DEST_M$",colnames(dataQuality))])/
        dataQuality[,"APP_REC"]
      dataQuality[,"SUM_DKR"] <- rowSums(
        dataQuality[,grep("^SSN_DKR$",colnames(dataQuality)):grep("^DEST_DKR$",colnames(dataQuality))])
      dataQuality[,"PCT_DKR"] <- rowSums(
        dataQuality[,grep("^SSN_DKR$",colnames(dataQuality)):grep("^DEST_DKR$",colnames(dataQuality))])/
        dataQuality[,"APP_REC"]
      dataQuality[,"SUM_MISSDKR"] <- dataQuality[,"SUM_MISS"]+dataQuality[,"SUM_DKR"]
      dataQuality[,"PCT_MISSDKR"] <- dataQuality[,"PCT_MISS"]+dataQuality[,"PCT_DKR"]
      dataQuality[,"PLOT_COL"] <- 
        ifelse(dataQuality$PCT_MISSDKR >= .05,
          "red",
          ifelse(
            dataQuality$PCT_MISSDKR >= .025,
            "darkorange",
            "green3"
          )
        )
    }   
    return(dataQuality)
  })
    
  #################################
  # CLIENT-LEVEL DATA QUALITY
  #################################
  
  Violations <- reactive({
    if (progCount2()==0 | input$reportLevel != "Program" | is.null(groupKeys())) return()
    # Take a dependency on input$update by reading it. (Nothing is actually done with the value.)
    input$update
    progress <- Progress$new(session)
    progress$set(message="Retrieving client data",detail="Please wait a moment...")
    #Reactivity is invalidated unless update button is pressed
    
    Violations <- 
      isolate(dbGetQuery(connection,paste("
        SELECT 
          Client_Key, 
          GI_M, 
          PI_M, 
          IB_M, 
          SN_M, 
          GI_DKR, 
          PI_DKR, 
          IB_DKR, 
          SN_DKR, 
          Unachild,
          LongtermStayers, 
          Violations 
        FROM (
          SELECT unique 
            Client_Key, 
            case when count(FN_M)>0 or count(LN_M)>0 or count(ID_M)>0 or count(DOB_M)>0 or
              count(Race_M)>0 or count(Ethn_M)>0 or count(Gender_M)>0 or count(Vet_M)>0 
              then 'x' end GI_M, 
            case when count(Disab_M)>0 or count(PNR_M)>0 or count(Zip_M)>0 or count(HS_M)>0 or 
              count(Dest_M)>0 then 'x' end PI_M, 
            case when count(CCI_N_M)>0 or count(CCI_X_M)>0 or count(CNB_N_M)>0 or count(CNB_X_M)>0 
              then 'x' end IB_M, 
            case when count(PD_M)>0 or count(DD_M)>0 or count(CHC_M)>0 or count(HIV_M)>0 or 
              count(MH_M)>0 or count(SA_M)>0 or count(DV_M)>0 then 'x' end SN_M,
            case when count(ID_DKR)>0 or count(DOB_DKR)>0 or count(Race_DKR)>0 or count(Ethn_DKR)>0 or
              count(Gender_DKR)>0 or count(Vet_DKR)>0 then 'x' end GI_DKR, 
            case when count(Disab_DKR)>0 or count(PNR_DKR)>0 or count(Zip_DKR)>0 or count(HS_DKR)>0 or 
              count(Dest_DKR)>0 then 'x' end PI_DKR, 
            case when count(CCI_N_DKR)>0 or count(CCI_X_DKR)>0 or count(CNB_N_DKR)>0 or 
              count(CNB_X_DKR)>0 then 'x' end IB_DKR, 
            case when count(PD_DKR)>0 or count(DD_DKR)>0 or count(CHC_DKR)>0 or count(HIV_DKR)>0 or 
              count(MH_DKR)>0 or count(SA_DKR)>0 or count(DV_DKR)>0 then 'x' end SN_DKR,
            case when count(UnaChild)>0 then 'x' end UnaChild, 
            case when count(LongtermStayers)>0 then 'x' end LongtermStayers, 
            count(unique FN_M) + count(unique LN_M) + count(unique ID_M) + count(unique DOB_M) + 
              count(unique Race_M) + count(unique Ethn_M) + count(unique Gender_M) + 
              count(unique Vet_M) + count(unique Disab_M) + count(unique PNR_M) + count(unique Zip_M) +
              count(unique HS_M) + count(unique Dest_M) + count(unique CCI_N_M) + 
              count(unique CCI_X_M) + count(unique CNB_N_M) + count(unique CNB_X_M) + 
              count(unique PD_M) + count(unique DD_M) + count(unique CHC_M) + count(unique HIV_M) +
              count(unique MH_M) + count(unique SA_M) + count(unique DV_M) + count(unique ID_DKR) + 
              count(unique DOB_DKR) + count(unique Race_DKR) + count(unique Ethn_DKR) + 
              count(unique Gender_DKR) + count(unique Vet_DKR) + count(unique Disab_DKR) +
              count(unique PNR_DKR) + count(unique Zip_DKR) + count(unique HS_DKR) + 
              count(unique Dest_DKR) + count(unique CCI_N_DKR) + count(unique CCI_X_DKR) + 
              count(unique CNB_N_DKR) + count(unique CNB_X_DKR) + count(unique PD_DKR) + 
              count(unique DD_DKR) + count(unique CHC_DKR) + count(unique HIV_DKR) + 
              count(unique MH_DKR) + count(unique SA_DKR) + count(unique DV_DKR) + 
              count(unique UnaChild) +
              count(unique LongtermStayers) Violations
          FROM (
            SELECT unique
              PE.Program_Enrollment_Key,
              CI.Client_Key,
              case when Name_First = 'MISSING' then CI.Client_Key end FN_M,
              case when Name_Last = 'MISSING' then CI.Client_Key end LN_M,
              case when not ID_Type in (1,8,9) then CI.Client_Key end ID_M,
              case when Date_of_Birth is null and not DOB_Type in (8,9) then CI.Client_Key end DOB_M,
              case when Race_Code is null then CI.Client_Key end Race_M,
              case when Ethnicity_Code is null then CI.Client_Key end Ethn_M,
              case when Gender_Code is null then CI.Client_Key end Gender_M,
              case when Veteran_Status_Code is null then CI.Client_Key end Vet_M,
              case when Disabling_Condition is null then PE.Program_Enrollment_Key end Disab_M,
              case when Prior_Nights_Residence_Code is null then PE.Program_ENrollment_Key end PNR_M,
              case when Zipcode_Last_Perm_Address is null and not Zip_Quality_Code in (8,9) 
                then PE.Program_Enrollment_Key end Zip_M,
              case when Housing_Status_Code is null then PE.Program_Enrollment_Key end HS_M,
              case when CCI_N.Verified_Answer is null then PE.Program_Enrollment_Key end CCI_N_M,
              case when CCI_X.Verified_Answer is null and Program_Exit_Date is not null 
                then PE.Program_Enrollment_Key end CCI_X_M,
              case when CNB_N.Verified_Answer is null then PE.Program_Enrollment_Key end CNB_N_M,
              case when CNB_X.Verified_Answer is null and Program_Exit_Date is not null 
                then PE.Program_Enrollment_Key end CNB_X_M,
              case when Physical_Disability is null then Program_Enrollment_Key end PD_M,
              case when Developmental_Disability is null then PE.Program_Enrollment_Key end DD_M,
              case when Chronic_Health_Condition is null then PE.Program_Enrollment_Key end CHC_M,
              case when HIV_AIDS is null then PE.Program_Enrollment_Key end HIV_M,
              case when Mental_Illness is null then PE.Program_Enrollment_Key end MH_M,
              case when Substance_Abuse is null then PE.Program_Enrollment_Key end SA_M,
              case when Dom_Vio_Survivor is null then PE.Program_Enrollment_Key end DV_M,
              case when Destination_Code is null and Program_Exit_Date is not null 
                then PE.Program_Enrollment_Key end Dest_M,
              case when ID_Type in (8,9) then CI.Client_Key end ID_DKR,
              case when DOB_Type in (8,9) then CI.Client_Key end DOB_DKR,
              case when Race_Code in (15,16) then CI.Client_Key end Race_DKR,
              case when Ethnicity_Code in (8,9) then CI.Client_Key end Ethn_DKR,
              case when Gender_Code in (8,9) then CI.Client_Key end Gender_DKR,
              case when Veteran_Status_Code in (8,9) then CI.Client_Key end Vet_DKR,
              case when Disabling_Condition in (8,9) then PE.Program_Enrollment_Key end Disab_DKR,
              case when Prior_Nights_Residence_Code in (8,9) then PE.Program_ENrollment_Key 
                end PNR_DKR,
              case when Zip_Quality_Code in (8,9) then PE.Program_Enrollment_Key end Zip_DKR,
              case when Housing_Status_Code in (8,9) then PE.Program_Enrollment_Key end HS_DKR,
              case when CCI_N.Verified_Answer in (8,9) then PE.Program_Enrollment_Key end CCI_N_DKR,
              case when CCI_X.Verified_Answer in (8,9) then PE.Program_Enrollment_Key end CCI_X_DKR,
              case when CNB_N.Verified_Answer in (8,9) then PE.Program_Enrollment_Key end CNB_N_DKR,
              case when CNB_X.Verified_Answer in (8,9) then PE.Program_Enrollment_Key end CNB_X_DKR,
              case when Physical_Disability in (8,9) then PE.Program_Enrollment_Key end PD_DKR,
              case when Developmental_Disability in (8,9) then PE.Program_Enrollment_Key end DD_DKR,
              case when Chronic_Health_Condition in (8,9) then PE.Program_Enrollment_Key end CHC_DKR,
              case when HIV_AIDS in (8,9) then PE.Program_Enrollment_Key end HIV_DKR,
              case when Mental_Illness in (8,9) then PE.Program_Enrollment_Key end MH_DKR,
              case when Substance_Abuse in (8,9) then PE.Program_Enrollment_Key end SA_DKR,
              case when Dom_Vio_Survivor in (8,9) then PE.Program_Enrollment_Key end DV_DKR,
              case when Destination_Code in (8,9) and Program_Exit_Date is not null 
                then PE.Program_Enrollment_Key end Dest_DKR,
              case when HH_Members=1 and (Program_Entry_Date - Date_of_Birth)/365.25 < 18 
                then PE.Program_Enrollment_Key end UnaChild,
              case when ((Program_Type_Code = 1 and to_date('",endSelect(),"','yyyy-mm-dd') - 
                Program_Entry_Date > 180) or (Program_Type_Code = 2 and 
                to_date('",endSelect(),"','yyyy-mm-dd') - Program_Entry_Date > 720)) and 
                Program_Exit_Date is null then PE.Program_Enrollment_Key end LongtermStayers
            FROM (
              SELECT
                PE.*,
                row_number() over(partition by Client_Key order by Program_Entry_Date desc) num
              FROM Program_Enrollment PE
              JOIN Program_Profile_Info PPI
                on PE.Program_Key = PPI.Program_Key
              FULL JOIN Program_Community_Information PCI
                on PE.Program_Key = PCI.Program_Key
              FULL JOIN Community_Group_Information CGI
                on PCI.Group_Key = CGI.Group_Key
              WHERE
                (Program_Exit_Date >= to_date('",beginSelect(),"','yyyy-mm-dd') or Program_Exit_Date is null) and
                Program_Entry_Date <= to_date('",endSelect(),"','yyyy-mm-dd') and ",
                finalSelect_Table(),input$reportLevel,"_Key=",finalSelect_Key(),programTypesSelect(),"
            ) PE
            RIGHT JOIN Program_Profile_Info PPI
              on PE.Program_Key = PPI.Program_Key
            LEFT JOIN Program_Community_Information PCI
              on PPI.Program_Key = PCI.Program_Key
            JOIN Client_Information CI
              on PE.Client_Key = CI.Client_Key
            LEFT JOIN (
              SELECT * 
              FROM Client_Status_Information
              WHERE Collect_Stage=1
            ) CSI
              on PE.Program_Enrollment_Key = CSI.Program_Enrollment_Key
            LEFT JOIN Client_Cash_Income CCI_N
              on PE.Entry_Cash_Key = CCI_N.Cash_GK
            LEFT JOIN Client_Cash_Income CCI_X
              on PE.Exit_Cash_Key = CCI_X.Cash_GK
            LEFT JOIN Client_Noncash_Benefits CNB_N
              on PE.Entry_Noncash_Key = CNB_N.Noncash_GK
            LEFT JOIN Client_Noncash_Benefits CNB_X
              on PE.Exit_Noncash_Key = CNB_X.Noncash_GK
            LEFT JOIN (
              SELECT *
              FROM Client_Special_Needs_Info 
              WHERE Collect_Stage=1 and Group_Key is not null
            ) CSNI
              on PE.Program_Enrollment_Key = CSNI.Program_Enrollment_Key
            LEFT JOIN (
              SELECT 
                Household_Key,
                count(Household_Key) HH_Members
              FROM Household_Client_Info HCI 
              GROUP BY Household_Key
            ) HCI
              on PE.Household_Key = HCI.Household_Key
            WHERE 
              PCI.Group_Key in (",groupKeys(),") and
              (Program_Exit_Date >= to_date('",beginSelect(),"','yyyy-mm-dd') or 
                Program_Exit_Date is null) and
              Program_Entry_Date <= to_date('",endSelect(),"','yyyy-mm-dd') and
              PPI.Program_Key=",finalSelect_Key(),APR_records()," 
          )
          GROUP BY Client_Key
          ORDER BY Violations Desc
        ) 
        WHERE 
          Violations > 0
      ",sep="")))
    
    names(Violations) <- c("Client Key","Missing General Info","Missing Program Info",
      "Missing Income / Benefits","Missing Special Needs","DKR General Info","DKR Program Info",
      "DKR Income / Benefits","DKR Special Needs","Unac. Child","Long-Term Stayer")
    progress$close()
    return(Violations[,1:11])
  })
  
