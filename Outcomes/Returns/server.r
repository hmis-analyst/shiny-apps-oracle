# TITLE: HOMELESS RETURNS
#------------------------------------
#------------------------------------

# .libPaths("~/R/win-library/3.1")
# install.packages("RJDBC",repos="http://cran.rstudio.com/")
# install.packages("ggplot2",repos="http://cran.rstudio.com/")
# install.packages("reshape2",repos="http://cran.rstudio.com/")
# install.packages("stringr",repos="http://cran.rstudio.com/")
# devtools::install_github("shiny-gridster", "wch")
#-------------------
# Load relevant packages
library(shinyIncubator)
library(RJDBC)
library(ggplot2)
library(reshape2)
library(stringr)
library(shinyGridster)
library(plyr)
#-------------------
libPath1 <- "~/HMIS Data Analyst/lib/"
libPath2 <- "../../lib/"
libPath3 <- "lib/"
l3 <- FALSE

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
  # Homeless returns are only relevant to housing programs, so at most the program list includes ES, TH, PSH, and RRH programs
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
  # Calculate return window
  returnWin <- reactive({
    Sys.Date()-2-input$daterange[2]
  })
  
  #-------------------
  #################################
  # RETURNS ALGORITHM
  #################################
  #-------------------
  # STEP 1: QUERYING THE INITIAL SAMPLE
  # Retrieve all clients who "left homelessness" during the report period
  # See "Rules for leaving homelessness" at https://github.com/hmis-analyst/shiny-apps-oracle/wiki/Homeless-Returns
  Exits <- reactive({
    input$update
    progress <- Progress$new(session)
    progress$set(message="Retrieving program enrollments",detail="Please wait a moment...")
    isolate(
      Exits <- dbGetQuery(connection,paste("
        /* Query the basic information needed for each enrollment */
        SELECT unique
          1 counter,
          PE.Program_Enrollment_Key,
          CI.Client_Key, 
          PE.Household_Key,
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
          Program_Exit_Date - Program_Entry_Date + 1 Length_of_Stay,
          /*
          /* GENERAL CHARACTERISTICS */
            /* Demographic info */
              case when Race_Code in (5,6,7,9,10,11,12,13,14) then 1 else (case when Race_Code = 8 then 0 end) end Dem_Race_Nonwhite,
              case when Race_Code = 6 then 1 else (case when Race_Code in (5,7,8,9,10,11,12,13,14) then 0 end) end Dem_Race_Black,
              case when Ethnicity_Code = 105 then 1 else (case when Ethnicity_Code = 104 then 0 end) end Dem_Ethnicity_Latino,
              case when Gender_Code = 1 then 1 else (case when Gender_Code in (2,3,4,5) then 0 end) end Dem_Gender_Male,
              case when Veteran_Status_Code in (1,0) then Veteran_Status_Code end Dem_Vet,
              Program_Entry_Date - Date_of_Birth Age_at_Entry,
              case when (Program_Entry_Date - Date_of_Birth)/365.25 < 18 then 1
                when (Program_Entry_Date - Date_of_Birth)/365.25 >= 18 then 0
                end Age_Under18_Entry,
              case when (Program_Entry_Date - Date_of_Birth)/365.25 between 18 and 24.99 then 1
                when (Program_Entry_Date - Date_of_Birth)/365.25 < 18 or (Program_Entry_Date - Date_of_Birth)/365.25 >= 25 then 0
                end Age_18to24_Entry,
              case when (Program_Entry_Date - Date_of_Birth)/365.25 >= 25 then 1
                when (Program_Entry_Date - Date_of_Birth)/365.25 < 25 then 0
                end Age_25andOlder_Entry,
              case when (Program_Exit_Date - Date_of_Birth)/365.25 < 18 then 1
                when (Program_Exit_Date - Date_of_Birth)/365.25 >= 18 then 0
                end Age_Under18_Exit,
              case when (Program_Exit_Date - Date_of_Birth)/365.25 between 18 and 24.99 then 1
                when (Program_Exit_Date - Date_of_Birth)/365.25 < 18 or (Program_Exit_Date - Date_of_Birth)/365.25 >= 25 then 0
                end Age_18to24_Exit,
              case when (Program_Exit_Date - Date_of_Birth)/365.25 >= 25 then 1
                when (Program_Exit_Date - Date_of_Birth)/365.25 < 25 then 0
                end Age_25andOlder_Exit,
            /* Household info */
              case when (Head_Num = 1 and Relationship_Code = 3) or (Head_Num = 0 and Date_of_Birth = Oldest_DoB) or 
                (Head_Num > 1 and Relationship_Code = 3 and Date_of_Birth = Oldest_DoB) then 1 else 0 end HH_Head,
              case when Num_Fam = 1 then 1 else 0 end HH_Unaccompanied,
              Num_Fam HH_Members, 
              case when Num_Child > 0 then 1 else 0 end HH_Has_Children,
              case when Num_Adult > 1 then 1 else 0 end HH_Not_Only_Adult,
              case when Num_Teen_M > 0 then 1 else 0 end HH_Has_Teenage_Male,
              case when Num_Teen_F > 0 then 1 else 0 end HH_Has_Teenage_Female,
              case when Num_Teen_M + Num_Teen_F > 0 then 1 else 0 end HH_Has_Teenager,
          /*
          /* ENTRY-SPECIFIC CHARACTERISTICS */
            /* Client status information */
              case when CSI_N.Prior_Nights_Residence_Code in (1,16) then 1
                else (
                  case when not (CSI_N.Prior_Nights_Residence_Code is null or CSI_N.Prior_Nights_Residence_Code in (8,9)) then 0 end
                ) end CS_PNR_Homeless,
              case when not PE_2.First_Homeless = PE.Program_Entry_Date then 1 else 0 end CS_Prior_Homeless_Enroll,
            /* Special needs */
              case when CSI_N.Disabling_Condition in (1,0) then CSI_N.Disabling_Condition end SN_DisabCond_Entry,
              case when CSNI_N.Physical_Disability in (1,0) then CSNI_N.Physical_Disability end SN_PhysDis_Entry,
              case when CSNI_N.Developmental_Disability in (1,0) then CSNI_N.Developmental_Disability end SN_DevelDis_Entry,
              case when CSNI_N.Chronic_Health_Condition in (1,0) then CSNI_N.Chronic_Health_Condition end SN_ChronHealth_Entry,
              case when CSNI_N.HIV_AIDS in (1,0) then CSNI_N.HIV_AIDS end SN_HIV_Entry,
              case when CSNI_N.Mental_Illness in (1,0) then CSNI_N.Mental_Illness end SN_MentIll_Entry,
              case when CSNI_N.Substance_Abuse in (1,0) then CSNI_N.Substance_Abuse end SN_SubstAbuse_Entry,
              case when CSNI_N.Dom_Vio_Survivor in (1,0) then CSNI_N.Dom_Vio_Survivor end SN_DV_Entry,
            /* Income */
              case when CCI_N.Verified_Answer in (1,0) then CCI_N.Verified_Answer end Inc_Any_Entry,
              case
                when CCI_N.EarnedIncome + CCI_N.UnempIns + CCI_N.SSI + CCI_N.SSDI + CCI_N.VetDisPmt + CCI_N.PrivDisIns + CCI_N.WorkersComp + CCI_N.TANF +
                  CCI_N.GA + CCI_N.SS + CCI_N.VetPension + CCI_N.JobPension + CCI_N.ChildSupport + CCI_N.Alimony + CCI_N.OtherSource = 1 then 1
                when CCI_N.EarnedIncome + CCI_N.UnempIns + CCI_N.SSI + CCI_N.SSDI + CCI_N.VetDisPmt + CCI_N.PrivDisIns + CCI_N.WorkersComp + CCI_N.TANF +
                  CCI_N.GA + CCI_N.SS + CCI_N.VetPension + CCI_N.JobPension + CCI_N.ChildSupport + CCI_N.Alimony + CCI_N.OtherSource != 1
                  or CCI_N.Verified_Answer = 0 then 0
                end Inc_One_Entry,
              case
                when CCI_N.EarnedIncome + CCI_N.UnempIns + CCI_N.SSI + CCI_N.SSDI + CCI_N.VetDisPmt + CCI_N.PrivDisIns + CCI_N.WorkersComp + CCI_N.TANF +
                  CCI_N.GA + CCI_N.SS + CCI_N.VetPension + CCI_N.JobPension + CCI_N.ChildSupport + CCI_N.Alimony + CCI_N.OtherSource > 1 then 1
                when CCI_N.EarnedIncome + CCI_N.UnempIns + CCI_N.SSI + CCI_N.SSDI + CCI_N.VetDisPmt + CCI_N.PrivDisIns + CCI_N.WorkersComp + CCI_N.TANF +
                  CCI_N.GA + CCI_N.SS + CCI_N.VetPension + CCI_N.JobPension + CCI_N.ChildSupport + CCI_N.Alimony + CCI_N.OtherSource <= 1
                  or CCI_N.Verified_Answer = 0 then 0
                end Inc_Multiple_Entry,
              case when CCI_N.Verified_Answer = 0 then 0 else CCI_N.EarnedIncome end Inc_Earned_Entry,
              case when CCI_N.Verified_Answer = 0 then 0 else CCI_N.UnempIns end Inc_UnempIns_Entry,
              case when CCI_N.Verified_Answer = 0 then 0 else CCI_N.SSI end Inc_SSI_Entry,
              case when CCI_N.Verified_Answer = 0 then 0 else CCI_N.SSDI end Inc_SSDI_Entry,
              case when CCI_N.Verified_Answer = 0 then 0 else CCI_N.VetDisPmt end Inc_VetDisPmt_Entry,
              case when CCI_N.Verified_Answer = 0 then 0 else CCI_N.PrivDisIns end Inc_PrivDisIns_Entry,
              case when CCI_N.Verified_Answer = 0 then 0 else CCI_N.WorkersComp end Inc_WorkersComp_Entry,
              case when CCI_N.Verified_Answer = 0 then 0 else CCI_N.TANF end Inc_TANF_Entry,
              case when CCI_N.Verified_Answer = 0 then 0 else CCI_N.GA end Inc_GA_Entry,
              case when CCI_N.Verified_Answer = 0 then 0 else CCI_N.SS end Inc_SS_Entry,
              case when CCI_N.Verified_Answer = 0 then 0 else CCI_N.VetPension end Inc_VetPension_Entry,
              case when CCI_N.Verified_Answer = 0 then 0 else CCI_N.JobPension end Inc_JobPension_Entry,
              case when CCI_N.Verified_Answer = 0 then 0 else CCI_N.ChildSupport end Inc_ChildSupport_Entry,
              case when CCI_N.Verified_Answer = 0 then 0 else CCI_N.Alimony end Inc_Alimony_Entry,
              case when CCI_N.Verified_Answer = 0 then 0 else CCI_N.OtherSource end Inc_OtherSource_Entry,
            /* Benefits */
              case when CNB_N.Verified_Answer in (1,0) then CNB_N.Verified_Answer end Ben_Any_Entry,
              case 
                when CNB_N.SNAP + CNB_N.Medicaid + CNB_N.Medicare + CNB_N.SCHIP + CNB_N.SSNP + CNB_N.VAMS + CNB_N.TANFCC + CNB_N.TANFTRAN + CNB_N.TANFOther + 
                  CNB_N.OngoingRentAssist + CNB_N.Other + CNB_N.RentTemp = 1 then 1 
                when CNB_N.SNAP + CNB_N.Medicaid + CNB_N.Medicare + CNB_N.SCHIP + CNB_N.SSNP + CNB_N.VAMS + CNB_N.TANFCC + CNB_N.TANFTRAN + CNB_N.TANFOther + 
                  CNB_N.OngoingRentAssist + CNB_N.Other + CNB_N.RentTemp != 1 or CNB_N.Verified_Answer = 0 then 0 
                end Ben_One_Entry,
              case 
                when CNB_N.SNAP + CNB_N.Medicaid + CNB_N.Medicare + CNB_N.SCHIP + CNB_N.SSNP + CNB_N.VAMS + CNB_N.TANFCC + CNB_N.TANFTRAN + CNB_N.TANFOther + 
                  CNB_N.OngoingRentAssist + CNB_N.Other + CNB_N.RentTemp > 1 then 1 
                when CNB_N.SNAP + CNB_N.Medicaid + CNB_N.Medicare + CNB_N.SCHIP + CNB_N.SSNP + CNB_N.VAMS + CNB_N.TANFCC + CNB_N.TANFTRAN + CNB_N.TANFOther + 
                  CNB_N.OngoingRentAssist + CNB_N.Other + CNB_N.RentTemp <= 1 or CNB_N.Verified_Answer = 0 then 0 
                end Ben_Multiple_Entry,
              case when CNB_N.Verified_Answer = 0 then 0 else CNB_N.SNAP end Ben_SNAP_Entry,
              case when CNB_N.Verified_Answer = 0 then 0 else CNB_N.Medicaid end Ben_Medicaid_Entry,
              case when CNB_N.Verified_Answer = 0 then 0 else CNB_N.Medicare end Ben_Medicare_Entry,
              case when CNB_N.Verified_Answer = 0 then 0 else CNB_N.SCHIP end Ben_SCHIP_Entry,
              case when CNB_N.Verified_Answer = 0 then 0 else CNB_N.SSNP end Ben_SSNP_Entry,
              case when CNB_N.Verified_Answer = 0 then 0 else CNB_N.VAMS end Ben_VAMS_Entry,
              case when CNB_N.Verified_Answer = 0 then 0 else CNB_N.TANFCC end Ben_TANFCC_Entry,
              case when CNB_N.Verified_Answer = 0 then 0 else CNB_N.TANFTRAN end Ben_TANFTRAN_Entry,
              case when CNB_N.Verified_Answer = 0 then 0 else CNB_N.TANFOther end Ben_TANFOther_Entry,
              case when CNB_N.Verified_Answer = 0 then 0 else CNB_N.OngoingRentAssist end Ben_RentAssist_Entry,
              case when CNB_N.Verified_Answer = 0 then 0 else CNB_N.Other end Ben_Other_Entry,
              case when CNB_N.Verified_Answer = 0 then 0 else CNB_N.RentTemp end Ben_RentTemp_Entry,
          /*
          /* EXIT-SPECIFIC CHARACTERISTICS */
            /* Special needs */
              case when CSI_N.Disabling_Condition in (1,0) then CSI_N.Disabling_Condition end SN_DisabCond_Exit,
              case when CSNI_N.Physical_Disability in (1,0) then CSNI_N.Physical_Disability end SN_PhysDis_Exit,
              case when CSNI_N.Developmental_Disability in (1,0) then CSNI_N.Developmental_Disability end SN_DevelDis_Exit,
              case when CSNI_N.Chronic_Health_Condition in (1,0) then CSNI_N.Chronic_Health_Condition end SN_ChronHealth_Exit,
              case when CSNI_N.HIV_AIDS in (1,0) then CSNI_N.HIV_AIDS end SN_HIV_Exit,
              case when CSNI_N.Mental_Illness in (1,0) then CSNI_N.Mental_Illness end SN_MentIll_Exit,
              case when CSNI_N.Substance_Abuse in (1,0) then CSNI_N.Substance_Abuse end SN_SubstAbuse_Exit,
              case when CSNI_N.Dom_Vio_Survivor in (1,0) then CSNI_N.Dom_Vio_Survivor end SN_DV_Exit,
            /* Income and benefits*/
              case when CCI_X.Verified_Answer in (1,0) then CCI_X.Verified_Answer end Any_Income_Exit,
              case 
                when CCI_X.EarnedIncome_Mthly_Amt - CCI_N.EarnedIncome_Mthly_Amt > 0 or 
                  ((CCI_N.Verified_Answer = 0 or CCI_N.EarnedIncome = 0 or CCI_N.EarnedIncome_Mthly_Amt = 0) and CCI_X.EarnedIncome = 1) then 1
                else (
                  case 
                    when CCI_X.Verified_Answer = 0 and CCI_N.Verified_Answer = 0 then 0
                    when CCI_X.Verified_Answer = 0 and CCI_N.EarnedIncome = 0 then 0
                    when CCI_X.Verified_Answer = 0 and CCI_N.EarnedIncome = 1 and CCI_N.EarnedIncome_Mthly_Amt is not null then 0
                    when CCI_N.Verified_Answer = 0 and CCI_X.EarnedIncome = 0 then 0
                    when CCI_N.Verified_Answer = 0 and CCI_X.EarnedIncome = 1 and CCI_X.EarnedIncome_Mthly_Amt is not null then 0
                    when CCI_X.EarnedIncome = 0 and CCI_N.EarnedIncome = 0 then 0
                    when CCI_X.EarnedIncome = 0 and CCI_N.EarnedIncome = 1 and CCI_N.EarnedIncome_Mthly_Amt is not null then 0
                    when CCI_N.EarnedIncome = 0 and CCI_X.EarnedIncome = 0 then 0
                    when CCI_N.EarnedIncome = 0 and CCI_X.EarnedIncome = 1 and CCI_X.EarnedIncome_Mthly_Amt is not null then 0
                    when CCI_X.EarnedIncome = 1 and CCI_X.EarnedIncome_Mthly_Amt is not null and CCI_N.EarnedIncome = 1 and 
                      CCI_N.EarnedIncome_Mthly_Amt is not null then 0
                  end
                ) end Inc_EarnedIncome_Increase,
              case 
                when CCI_X.EarnedIncome_Mthly_Amt - CCI_N.EarnedIncome_Mthly_Amt < 0 or 
                  ((CCI_X.Verified_Answer = 0 or CCI_X.EarnedIncome = 0 or CCI_X.EarnedIncome_Mthly_Amt = 0) and CCI_N.EarnedIncome = 1) then 1
                else (
                  case 
                    when CCI_X.Verified_Answer = 0 and CCI_N.Verified_Answer = 0 then 0
                    when CCI_X.Verified_Answer = 0 and CCI_N.EarnedIncome = 0 then 0
                    when CCI_X.Verified_Answer = 0 and CCI_N.EarnedIncome = 1 and CCI_N.EarnedIncome_Mthly_Amt is not null then 0
                    when CCI_N.Verified_Answer = 0 and CCI_X.EarnedIncome = 0 then 0
                    when CCI_N.Verified_Answer = 0 and CCI_X.EarnedIncome = 1 and CCI_X.EarnedIncome_Mthly_Amt is not null then 0
                    when CCI_X.EarnedIncome = 0 and CCI_N.EarnedIncome = 0 then 0
                    when CCI_X.EarnedIncome = 0 and CCI_N.EarnedIncome = 1 and CCI_N.EarnedIncome_Mthly_Amt is not null then 0
                    when CCI_N.EarnedIncome = 0 and CCI_X.EarnedIncome = 0 then 0
                    when CCI_N.EarnedIncome = 0 and CCI_X.EarnedIncome = 1 and CCI_X.EarnedIncome_Mthly_Amt is not null then 0
                    when CCI_X.EarnedIncome = 1 and CCI_X.EarnedIncome_Mthly_Amt is not null and CCI_N.EarnedIncome = 1 and 
                      CCI_N.EarnedIncome_Mthly_Amt is not null then 0
                  end
                ) end Inc_EarnedIncome_Decrease,
              case 
                when (CCI_X.EarnedIncome_Mthly_Amt - CCI_N.EarnedIncome_Mthly_Amt = 0 and CCI_X.EarnedIncome_Mthly_Amt > 0) then 1
                else (
                  case 
                    when CCI_X.Verified_Answer = 0 and CCI_N.Verified_Answer = 0 then 0
                    when CCI_X.Verified_Answer = 0 and CCI_N.EarnedIncome = 0 then 0
                    when CCI_X.Verified_Answer = 0 and CCI_N.EarnedIncome = 1 and CCI_N.EarnedIncome_Mthly_Amt is not null then 0
                    when CCI_N.Verified_Answer = 0 and CCI_X.EarnedIncome = 0 then 0
                    when CCI_N.Verified_Answer = 0 and CCI_X.EarnedIncome = 1 and CCI_X.EarnedIncome_Mthly_Amt is not null then 0
                    when CCI_X.EarnedIncome = 0 and CCI_N.EarnedIncome = 0 then 0
                    when CCI_X.EarnedIncome = 0 and CCI_N.EarnedIncome = 1 and CCI_N.EarnedIncome_Mthly_Amt is not null then 0
                    when CCI_N.EarnedIncome = 0 and CCI_X.EarnedIncome = 0 then 0
                    when CCI_N.EarnedIncome = 0 and CCI_X.EarnedIncome = 1 and CCI_X.EarnedIncome_Mthly_Amt is not null then 0
                    when CCI_X.EarnedIncome = 1 and CCI_X.EarnedIncome_Mthly_Amt is not null and CCI_N.EarnedIncome = 1 and 
                      CCI_N.EarnedIncome_Mthly_Amt is not null then 0
                  end
                ) end Inc_EarnedIncome_Same,
              case 
                when (CCI_N.Verified_Answer = 0 and CCI_X.Verified_Answer = 0) or 
                  (CCI_X.Verified_Answer = 0 and (CCI_N.EarnedIncome = 0 or CCI_N.EarnedIncome_Mthly_Amt = 0)) or
                  (CCI_N.Verified_Answer = 0 and (CCI_X.EarnedIncome = 0 or CCI_X.EarnedIncome_Mthly_Amt = 0)) or 
                  ((CCI_N.EarnedIncome = 0 or CCI_N.EarnedIncome_Mthly_Amt = 0) and (CCI_X.EarnedIncome = 0 or CCI_X.EarnedIncome_Mthly_Amt = 0)) then 1
                else (
                  case 
                    when CCI_X.Verified_Answer = 0 and CCI_N.Verified_Answer = 0 then 0
                    when CCI_X.Verified_Answer = 0 and CCI_N.EarnedIncome = 0 then 0
                    when CCI_X.Verified_Answer = 0 and CCI_N.EarnedIncome = 1 and CCI_N.EarnedIncome_Mthly_Amt is not null then 0
                    when CCI_N.Verified_Answer = 0 and CCI_X.EarnedIncome = 0 then 0
                    when CCI_N.Verified_Answer = 0 and CCI_X.EarnedIncome = 1 and CCI_X.EarnedIncome_Mthly_Amt is not null then 0
                    when CCI_X.EarnedIncome = 0 and CCI_N.EarnedIncome = 0 then 0
                    when CCI_X.EarnedIncome = 0 and CCI_N.EarnedIncome = 1 and CCI_N.EarnedIncome_Mthly_Amt is not null then 0
                    when CCI_N.EarnedIncome = 0 and CCI_X.EarnedIncome = 0 then 0
                    when CCI_N.EarnedIncome = 0 and CCI_X.EarnedIncome = 1 and CCI_X.EarnedIncome_Mthly_Amt is not null then 0
                    when CCI_X.EarnedIncome = 1 and CCI_X.EarnedIncome_Mthly_Amt is not null and CCI_N.EarnedIncome = 1 and 
                      CCI_N.EarnedIncome_Mthly_Amt is not null then 0
                  end
                ) end Inc_EarnedIncome_BothNo,
              case
                when CCI_X.EarnedIncome + CCI_X.UnempIns + CCI_X.SSI + CCI_X.SSDI + CCI_X.VetDisPmt + CCI_X.PrivDisIns + CCI_X.WorkersComp + CCI_X.TANF +
                  CCI_X.GA + CCI_X.SS + CCI_X.VetPension + CCI_X.JobPension + CCI_X.ChildSupport + CCI_X.Alimony + CCI_X.OtherSource = 1 then 1
                when CCI_X.EarnedIncome + CCI_X.UnempIns + CCI_X.SSI + CCI_X.SSDI + CCI_X.VetDisPmt + CCI_X.PrivDisIns + CCI_X.WorkersComp + CCI_X.TANF +
                  CCI_X.GA + CCI_X.SS + CCI_X.VetPension + CCI_X.JobPension + CCI_X.ChildSupport + CCI_X.Alimony + CCI_X.OtherSource != 1
                  or CCI_X.Verified_Answer = 0 then 0
                end Inc_One_Exit,
              case
                when CCI_X.EarnedIncome + CCI_X.UnempIns + CCI_X.SSI + CCI_X.SSDI + CCI_X.VetDisPmt + CCI_X.PrivDisIns + CCI_X.WorkersComp + CCI_X.TANF +
                  CCI_X.GA + CCI_X.SS + CCI_X.VetPension + CCI_X.JobPension + CCI_X.ChildSupport + CCI_X.Alimony + CCI_X.OtherSource > 1 then 1
                when CCI_X.EarnedIncome + CCI_X.UnempIns + CCI_X.SSI + CCI_X.SSDI + CCI_X.VetDisPmt + CCI_X.PrivDisIns + CCI_X.WorkersComp + CCI_X.TANF +
                  CCI_X.GA + CCI_X.SS + CCI_X.VetPension + CCI_X.JobPension + CCI_X.ChildSupport + CCI_X.Alimony + CCI_X.OtherSource <= 1
                  or CCI_X.Verified_Answer = 0 then 0
                end Inc_Multiple_Exit,
              case when CCI_X.Verified_Answer = 0 then 0 else CCI_X.EarnedIncome end Inc_Earned_Exit,
              case when CCI_X.Verified_Answer = 0 then 0 else CCI_X.UnempIns end Inc_UnempIns_Exit,
              case when CCI_X.Verified_Answer = 0 then 0 else CCI_X.SSI end Inc_SSI_Exit,
              case when CCI_X.Verified_Answer = 0 then 0 else CCI_X.SSDI end Inc_SSDI_Exit,
              case when CCI_X.Verified_Answer = 0 then 0 else CCI_X.VetDisPmt end Inc_VetDisPmt_Exit,
              case when CCI_X.Verified_Answer = 0 then 0 else CCI_X.PrivDisIns end Inc_PrivDisIns_Exit,
              case when CCI_X.Verified_Answer = 0 then 0 else CCI_X.WorkersComp end Inc_WorkersComp_Exit,
              case when CCI_X.Verified_Answer = 0 then 0 else CCI_X.TANF end Inc_TANF_Exit,
              case when CCI_X.Verified_Answer = 0 then 0 else CCI_X.GA end Inc_GA_Exit,
              case when CCI_X.Verified_Answer = 0 then 0 else CCI_X.SS end Inc_SS_Exit,
              case when CCI_X.Verified_Answer = 0 then 0 else CCI_X.VetPension end Inc_VetPension_Exit,
              case when CCI_X.Verified_Answer = 0 then 0 else CCI_X.JobPension end Inc_JobPension_Exit,
              case when CCI_X.Verified_Answer = 0 then 0 else CCI_X.ChildSupport end Inc_ChildSupport_Exit,
              case when CCI_X.Verified_Answer = 0 then 0 else CCI_X.Alimony end Inc_Alimony_Exit,
              case when CCI_X.Verified_Answer = 0 then 0 else CCI_X.OtherSource end Inc_OtherSource_Exit,
            /* Benefits */
              case when CNB_X.Verified_Answer in (1,0) then CNB_X.Verified_Answer end Ben_Any_Exit,
              case 
                when CNB_X.SNAP + CNB_X.Medicaid + CNB_X.Medicare + CNB_X.SCHIP + CNB_X.SSNP + CNB_X.VAMS + CNB_X.TANFCC + CNB_X.TANFTRAN + CNB_X.TANFOther + 
                  CNB_X.OngoingRentAssist + CNB_X.Other + CNB_X.RentTemp = 1 then 1 
                when CNB_X.SNAP + CNB_X.Medicaid + CNB_X.Medicare + CNB_X.SCHIP + CNB_X.SSNP + CNB_X.VAMS + CNB_X.TANFCC + CNB_X.TANFTRAN + CNB_X.TANFOther + 
                  CNB_X.OngoingRentAssist + CNB_X.Other + CNB_X.RentTemp != 1 or CNB_X.Verified_Answer = 0 then 0 
                end Ben_One_Exit,
              case 
                when CNB_X.SNAP + CNB_X.Medicaid + CNB_X.Medicare + CNB_X.SCHIP + CNB_X.SSNP + CNB_X.VAMS + CNB_X.TANFCC + CNB_X.TANFTRAN + CNB_X.TANFOther + 
                  CNB_X.OngoingRentAssist + CNB_X.Other + CNB_X.RentTemp > 1 then 1 
                when CNB_X.SNAP + CNB_X.Medicaid + CNB_X.Medicare + CNB_X.SCHIP + CNB_X.SSNP + CNB_X.VAMS + CNB_X.TANFCC + CNB_X.TANFTRAN + CNB_X.TANFOther + 
                  CNB_X.OngoingRentAssist + CNB_X.Other + CNB_X.RentTemp <= 1 or CNB_X.Verified_Answer = 0 then 0 
                end Ben_Multiple_Exit,
              case when CNB_X.Verified_Answer = 0 then 0 else CNB_X.SNAP end Ben_SNAP_Exit,
              case when CNB_X.Verified_Answer = 0 then 0 else CNB_X.Medicaid end Ben_Medicaid_Exit,
              case when CNB_X.Verified_Answer = 0 then 0 else CNB_X.Medicare end Ben_Medicare_Exit,
              case when CNB_X.Verified_Answer = 0 then 0 else CNB_X.SCHIP end Ben_SCHIP_Exit,
              case when CNB_X.Verified_Answer = 0 then 0 else CNB_X.SSNP end Ben_SSNP_Exit,
              case when CNB_X.Verified_Answer = 0 then 0 else CNB_X.VAMS end Ben_VAMS_Exit,
              case when CNB_X.Verified_Answer = 0 then 0 else CNB_X.TANFCC end Ben_TANFCC_Exit,
              case when CNB_X.Verified_Answer = 0 then 0 else CNB_X.TANFTRAN end Ben_TANFTRAN_Exit,
              case when CNB_X.Verified_Answer = 0 then 0 else CNB_X.TANFOther end Ben_TANFOther_Exit,
              case when CNB_X.Verified_Answer = 0 then 0 else CNB_X.OngoingRentAssist end Ben_RentAssist_Exit,
              case when CNB_X.Verified_Answer = 0 then 0 else CNB_X.Other end Ben_Other_Exit,
              case when CNB_X.Verified_Answer = 0 then 0 else CNB_X.RentTemp end Ben_RentTemp_Exit
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
        WHERE 
          /* Only include enrollments that have a program exit date within the report period */
          Program_Exit_Date >= to_date('",beginSelect(),"','yyyy-mm-dd') and
          Program_Entry_Date <= to_date('",endSelect(),"','yyyy-mm-dd') and
          Program_Type_Code in (1,2,3,14) and 
          Destination_Code in (18,120,119,117,118,116,113,23,22,21,20,19,11,10,3) and ",
          finalSelect_Table(),input$reportLevel,"_Key=",finalSelect_Key(),programTypesSelect()
      ,sep=""))
    )
    progress$close()
    return(Exits)
  })
  #-------------------
  # STEP 2: TESTING FOR CLIENT RETURN
  # See "Rules for returning to homelessness" at https://github.com/hmis-analyst/shiny-apps-oracle/wiki/Homeless-Returns
  Exits2 <- reactive({
    input$update
    progress <- Progress$new(session)
    Exits2 <- Exits()
    # Get number of observations in Exits, store in "n"
    n <- length(Exits2[,1])
    if(n==0 | is.na(Exits2[1,"CLIENT_KEY"])) return(Exits2)
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
    #-------------------
    progress$close()
    return(Exits2)
  })

  #################################
  # FINAL CALCULATIONS
  #################################
  #-------------------
  # Return rate calculation
  output$returnRate <- renderText({paste("Return Rate: ",round(mean(Exits2()[,"Return"])*100,1),"%",sep="")})
  #-------------------
  # Display to UI the number of permanent destinations assessed
  output$exitNum <- renderText({paste("A total of",length(Exits2()[,1]),"permanent destinations were assessed.")})
  #-------------------
  # Count the number of homeless returns
  Return <- reactive({sum(Exits2()[,"Return"])})
  #-------------------
  ProgScore_prep <- reactive({
    input$update
    if(length(Exits2()[which(Exits2()$HH_HEAD==1),1])<10) {return("There was insufficient data to process this request. In order for a 
      score to be calculated, at least 10 households must have exited during the report period. Please extend the report period or choose another program.")}
    if(length(grep("Returns/lib",getwd()))==0) {
      setwd(libPath3)
    }
    isolate(prog_type <- switch(programTypes_Reduced(),
      "ES" = "ES",
      "TH" = "TH",
      "PSH" = "PSH",
      "RapidReHousing" = "RRH"
    ))
    if(prog_type %in% c("PSH","RRH")) {return}
    q <- Exits2()
    q$DEM_RACE_OTHERMINORITY <- NA
    q$DEM_RACE_OTHERMINORITY[which(q$DEM_RACE_NONWHITE==1 & q$DEM_RACE_BLACK==0)] <- 1
    q$DEM_RACE_OTHERMINORITY[which(is.na(q$DEM_RACE_OTHERMINORITY) & !is.na(q$DEM_RACE_NONWHITE))] <- 0
    q$INC_CUSTOM_OTHER <- NA
    q$INC_CUSTOM_OTHER[which(q$INC_ANY_ENTRY==1 & q$INC_EARNED_ENTRY==0)] <- 1
    q$INC_CUSTOM_OTHER[which(is.na(q$INC_CUSTOM_OTHER) & !is.na(q$INC_ANY_ENTRY))] <- 0
    q$BEN_CUSTOM_OTHER <- NA
    q$BEN_CUSTOM_OTHER[which(q$BEN_ANY_ENTRY==1 & q$BEN_SNAP_ENTRY==0)] <- 1
    q$BEN_CUSTOM_OTHER[which(is.na(q$BEN_CUSTOM_OTHER) & !is.na(q$BEN_ANY_ENTRY))] <- 0
    MLE <- read.csv(paste("MLE",prog_type,"Apr13-Mar14_Rec.csv",sep="_"))
    logit <- MLE[1,"Estimate"]+rowSums(MLE[2:length(MLE[,1]),"Estimate"]*q[,as.character(MLE[2:length(MLE[,1]),"Parameter"])])
    program_data <- data.frame(
      PROGRAM_KEY = q$PROGRAM_KEY,
      likelihood = exp(logit)/(exp(logit)+1),
      Returns = ifelse(is.na(q$O_Return),0,q$Return)
    )
    program_data_valid <- program_data[which(!is.na(program_data$likelihood)),]
    program_data_p <- ddply(program_data_valid,"PROGRAM_KEY",summarize,p=mean(likelihood))
    program_data_agg <- ddply(program_data,"PROGRAM_KEY",summarize,returns=sum(Returns),n=length(PROGRAM_KEY))
    p <- merge(program_data_p,program_data_agg,by="PROGRAM_KEY",all.x=TRUE,all.y=TRUE)
    setwd("../")
    return(p)
  })
  ProgScore <- reactive({
    if(length(ProgScore_prep())==1) return()
    p <- ProgScore_prep()
    phi <- chisq.test(x=c(p$returns,p$n-p$returns),p=c(p$p,1-p$p))$statistic[[1]]/p$n
    phi2 <- ifelse(p$returns/p$n < p$p, phi*-1, phi)
    phi3 <- ifelse(phi2 < -1, -100, ifelse(phi2 > 1, 100, phi2*100))
    phi4 <- phi3/2+50
    return(phi4)
  })
  
  output$status <- reactive({
    if(length(ProgScore_prep())==1) return(ProgScore_prep())
      list(text=paste(round(ProgScore(),0),"/100",sep=""))
  })
  
  output$significance <- renderText({
    if(length(ProgScore_prep())==1) return()
    p <- ProgScore_prep()
    p_value <- binom.test(x=p$returns,n=p$n,p=p$p)$p.value
    if(p_value<.05) {
      if(p$returns/p$n<p$p) {
        return("(WORSE than the Georgia average)")
      }
      else {
        return("(BETTER than the Georgia average)")
      }
    }
    else {
      return("(not statistically different from the Georgia average)")
    }
  })
  #################################
  # TABLES
  #################################
  #-------------------
  enrollTable <- reactive({
    enrollTable <- Exits2()[,!(names(Exits2()) %in% c("COUNTER","DESTINATION_CODE","DEST_TEMPORARY","DEST_INSTITUTIONAL","DEST_PERMANENT","DEST_UNKNOWN"))]
  }
  )
  #-------------------
  output$barData <- renderDataTable({
    if(Return()==0) return()
    barData <- barData()[,c("label","num","rate")]
    barData[which(is.na(barData$num)),c("num","rate")] <- 0
    for (i in 1:length(barData[,1])) {
      barData[i,"rate_cum"] <- sum(barData[1:i,"rate"])
    }
    totalRow <- data.frame(
      label="TOTAL",
      num=sum(barData$num),
      rate=sum(barData$rate),
      rate_cum=NA
    )
    barData <- rbind(barData,totalRow)
    names(barData) <- c("Months Until Return","Returns","% of Exits", "Cumulative % of Exits")
    return(barData)
  },options=list(
      fnRowCallback = I('
        function(nRow, aData, iDisplayIndex, iDisplayIndexFull) {
          // Column alignment
          $("td:eq(0)", nRow).css("text-align", "left");
          $("td:eq(1)", nRow).css("text-align", "right");
          $("td:eq(2)", nRow).css("text-align", "right");
          $("td:eq(2)", nRow).css("text-align", "right");
        }
      '),
      bAutoWidth=FALSE,bPaginate=0,bLengthChange=0,bInfo=0,bSort=0,bFilter=0,
      aoColumns=list(list(bSearchable=FALSE),list(bSearchable=FALSE),list(bSearchable=FALSE),list(bSearchable=FALSE))
    )
  )
  #-------------------
  progsTable <- reactive({
    progsTable <- 
      ddply(
        Exits2(),
        c("PROGRAM_KEY","AGENCY_NAME","PROGRAM_NAME","PROGRAM_TYPE"),
        summarize,
        numTot=length(Return),
        numRec=sum(Return),
        rate=round(mean(Return)*100,1)
      )
    names(progsTable) <- c("PROGRAM KEY","AGENCY NAME","PROGRAM NAME","PROGRAM TYPE","EXITS FROM HOMELESSNESS","RETURNS","RETURN %")
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
      Return_Type = seq(1:10),
      Return_Type_Txt = c("A","B","C","D","E","F","G","H","I","J"),
      col = c("#67000D","#A50F15","#CB181D","#EF3B2C","#FB6A4A","#FC9272","#FCBBA1","#FEE0D2","#FFF5F0","#BAE4B3")
    )
    Exits_Summary <- ddply(Exits2()[which(Exits2()[,"Return_Type"]!=10),],"Return_Type",summarize,num=length(Return))
    barData <- merge(frame[1:max(Exits_Summary$Return_Type),],Exits_Summary,by="Return_Type",all.x=TRUE)
    barData <- barData[order(barData[,"Return_Type"]),]
    barData[,"rate"] <- round(barData[,"num"]/length(Exits2()[,1])*100,1)
    return(barData)
  })
  
  output$barChart <- renderPlot({
    if(Return()==0) return()
    print(barChart <- ggplot(data=barData(),aes(x=Return_Type_Txt,y=rate,fill=Return_Type_Txt)) + 
      geom_bar(color="black",stat="identity") +
      scale_fill_manual(
        values=as.character(barData()[,"col"])
      ) +
      theme(legend.position="none", axis.text.x = element_text(angle = 45, hjust = 1,size=18),axis.text.y = element_text(size=18),
        axis.title.x = element_text(size=18),axis.title.y = element_text(size=18),plot.title = element_text(size =18)) +
      scale_x_discrete(breaks=barData()[,"Return_Type_Txt"], labels=barData()[,"label"]) +
      ggtitle("The Swiftness of Return") +
      ylab("% of exits") +
      xlab("Months until return")
  )})
  
  output$live_gauge <- reactive({
    if(length(ProgScore_prep())==1) return()
    if(length(grep("Destinations/lib",getwd()))==0) {
      setwd(libPath3)
    }
    isolate(prog_type <- switch(programTypes_Reduced(),
      "ES" = "ES",
      "TH" = "TH",
      "PSH" = "PSH",
      "RapidReHousing" = "RRH"
    ))
    if(prog_type %in% c("RRH","PSH")) {return}
    program_data <- read.csv(paste("Prog data",prog_type,"Apr13-Mar14.csv",sep="_"))
    x <- program_data[which(!is.na(program_data$decile)),]
    p <- ProgScore_prep()
    DoD <- x$decile[which(abs(x$p-p$p)==min(abs(x$p-p$p)))]
    setwd("../")
    return(DoD)
  })
  
  outcomePlot <- reactive({
    input$update 
    if(length(ProgScore_prep())==1) return()
    if(length(grep("Destinations/lib",getwd()))==0) {
      setwd(libPath3)
    }
    isolate(prog_type <- switch(programTypes_Reduced(),
      "ES" = "ES",
      "TH" = "TH",
      "PSH" = "PSH",
      "RapidReHousing" = "RRH"
    ))
    if(prog_type %in% c("RRH","PSH")) {return}
    program_data <- read.csv(paste("Prog data",prog_type,"Apr13-Mar14.csv",sep="_"))
    program_data2 <- NULL
    program_data2 <- program_data[which(program_data$n>10 & !is.na(program_data$p) & is.na(program_data$exclude)),]
    program_data2$avg <- mean(program_data2$adjscore)
    program_data2$progscore <- ProgScore()
    setwd("../")
    isolate(
      ggplot(program_data2,aes(x=adjscore)) +
        geom_histogram() + 
        geom_vline(aes(xintercept=avg,color="Georgia average"),show_guide=TRUE) +
        geom_vline(aes(xintercept=progscore,color="Your program")) +
        scale_colour_brewer(name = "Scores",palette="Set1") + 
        theme(legend.text=element_text(size=14),legend.title=element_text(size=14),axis.text.x = element_text(size=14),axis.text.y = element_text(size=14),
          axis.title.x = element_text(size=14),axis.title.y = element_text(size=14),plot.title = element_text(size =18)) +
        xlab(paste("Adjusted outcome scores for ",prog_type," programs in SFY 2014",sep="")) + 
        ylab("Number of programs") +
        ggtitle("How your program compares\nwith the Georgia average")
    )
  })
  output$outcomePlot <- renderPlot({
    print(outcomePlot())
  })
  
  #################################
  # MAIN PANEL UI
  #################################
  
  output$mainPanel <- renderUI({
    # Take a dependency on input$update by reading it. (Nothing is actually done with the value.)
    input$update
    if (input$printable==TRUE) {
      #Reactivity is invalidated unless update button is pressed
      isolate(div(
        h2("OUTCOMES REPORT: RETURNS",align="center"), br(),
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
              h4(textOutput("returnRate")),
              HTML(
                paste("<p>Based on your <i>end date</i> selection, the 
                  <a href='https://github.com/hmis-analyst/shiny-apps-oracle/wiki/Homeless-Returns#terms-to-remember' target='_blank'>return window</a> 
                  was<strong>",returnWin(),"days</strong></p>"
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
              h4(textOutput("returnRate")),
              HTML(
                paste("<p>Based on your <i>end date</i> selection, the 
                  <a href='https://github.com/hmis-analyst/shiny-apps-oracle/wiki/Homeless-Returns#terms-to-remember' target='_blank'>return window</a> 
                  was<strong>",returnWin(),"days</strong></p>"
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
#             tabPanel("Performance",
#               if(length(ProgScore_prep())==1) {
#                 div(
#                   p(paste("There was insufficient data to process this request. In order for a score to be calculated, at least 10 households must have exited to a permanent destination during the report period. Please extend the report period or choose another program.")),
#                   br()
#                 )
#               },
#               fluidRow(
#                 column(4,
#                   wellPanel(
#                     gridster(width = 250, height = 250,
#                       gridsterItem(col = 2, row = 2, sizex = 1, sizey = 1,
#                         tags$div(class = 'grid_title', 'Adjusted Outcome Score'),
#                         statusOutput('status')
#                       )
#                     ),
#                     br(),
#                     div(align="center",textOutput("significance"))
#                   )
#                 ),
#                 column(8,
#                   h4("What is the Adjusted Outcome Score?"),
#                   p("A 2014 analysis of Georgia HMIS data found that, out of over 100 characteristics, there are a handful that strongly predict whether heads of household will exit to permanent housing destinations. This means that a ",strong("PREDICTED permanent destination rate")," can be calculated for your program -- based on (a) the types of households that exited your program during the report period and (b) your program type. Once this is done, your predicted outcome is compared with your ",strong("ACTUAL permanent destination rate.")," The final result is an adjusted score that is based on your program's actual performance, but takes into account the difficulty of your clientele. In other words, you are not penalized for serving more-difficult households.")
#                 )
#               ),
#               br(),
#               br(),
#               plotOutput("outcomePlot")
#             )
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
