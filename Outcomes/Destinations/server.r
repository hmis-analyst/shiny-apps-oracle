# .libPaths("~/R/win-library/3.0")
# install.packages("RJDBC",repos="http://cran.rstudio.com/")
# install.packages("ggplot2",repos="http://cran.rstudio.com/")
# install.packages("reshape2",repos="http://cran.rstudio.com/")
# install.packages("stringr",repos="http://cran.rstudio.com/")

# Load shiny, RODBC, and ggplot2 packages
library(shinyIncubator)
library(RJDBC)
library(ggplot2)
library(reshape2)
library(stringr)
library(shinyGridster)
library(plyr)

libPath1 <- "~/HMIS Data Analyst/lib/"
libPath2 <- "../../lib/"
libPath3 <- "lib/"
l3 <- FALSE

# Establish JDBC connection using RJDBC
source(paste(libPath1,"conn-Ora-Georgia_Base.r",sep=""),local=TRUE)

# Create a function to modify date format
dateMod <- function(x) {paste(substr(x,6,7),"/",substr(x,9,10),"/",substr(x,1,4),sep="")}

# Define server logic required to query/graph HMIS gender data
shinyServer(function(input, output, session) {
  #################################
  # USER SELECTIONS
  #################################
  
  
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
  # Import Data Options server code
  source(paste(libPath2,"DataOptions-Ora.server.r",sep=""), local=TRUE)
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
  #################################
  # MAIN QUERIES
  #################################
  
  qResults_Enrolls <- reactive({
    if(progCount2()==0) return()
    # Take a dependency on input$update by reading it. (Nothing is actually done with the value.)
    input$update
    progress <- Progress$new(session)
    progress$set(message="Calculating outcome score",detail="Please wait a moment...")
    #Reactivity is invalidated unless update button is pressed
    isolate(
      # Query gender breakdown  based on user selections    
      qResults_Enrolls <- dbGetQuery(connection,paste("
        SELECT unique
          PE.Program_Enrollment_Key,
          PPI.Program_Key,
          Agency_Name,
          Program_Name,
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
              case when CNB_X.Verified_Answer = 0 then 0 else CNB_X.RentTemp end Ben_RentTemp_Exit,  
          /*
          /* OUTCOMES */
          case when Destination_Code in (3,10,11,23,22,21,20,19) then 1 else (case when not Destination_Code in (8,9) then 0 end) end o_destType_P,
          DC.Description Destination,
          case when Destination_Code is null or Destination_Code in (8,9,17) then 'Unknown'
               when Destination_Code in (110, 16, 2, 1, 111, 121) then 'Homeless'
               when Destination_Code in (4, 5, 6, 7, 15, 106, 107, 109) then 'Institutional'
               when Destination_Code in (807, 806, 122, 14, 13, 12) then 'Temporary housing'
               when Destination_Code in (18, 120, 119, 117, 118, 116, 113, 23, 22, 21, 20, 19, 11, 10, 3) then 'Permanent housing'
            end Destination_Category
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
          Program_Exit_Date >= to_date('",beginSelect(),"','yyyy-mm-dd') and
          Program_Exit_Date <= to_date('",endSelect(),"','yyyy-mm-dd') and ",
          finalSelect_Table(),input$reportLevel,"_Key=",finalSelect_Key(),programTypesSelect()       
      ,sep=""))
    )
    progress$close()
    return(qResults_Enrolls)
  })
  
  qResults_short <- reactive({
    if(progCount2()==0) return()
    # Take a dependency on input$update by reading it. (Nothing is actually done with the value.)
    input$update
    progress <- Progress$new(session)
    progress$set(message="Retrieving data",detail="Please wait a moment...")
    #Reactivity is invalidated unless update button is pressed
    isolate(
      # Query gender breakdown  based on user selections    
      qResults_short <- dbGetQuery(connection,paste("
        SELECT 
          count(unique case when Destination_Code is null or Destination_Code in (8,9,17) then Program_Enrollment_Key end) Unknown,
          count(unique case when Destination_Code in (110, 16, 2, 1, 111, 121) then Program_Enrollment_Key end) Homeless,
          count(unique case when Destination_Code in (4, 5, 6, 7, 15, 106, 107, 109) then Program_Enrollment_Key end) Institutional,
          count(unique case when Destination_Code in (807, 806, 122, 14, 13, 12) then Program_Enrollment_Key end) Temporary, 
          count(unique case when Destination_Code in (18, 120, 119, 117, 118, 116, 113, 23, 22, 21, 20, 19, 
            11, 10, 3) then Program_Enrollment_Key end) Permanent,
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
        Program_Entry_Date <= to_date('",endSelect(),"','yyyy-mm-dd') and ",
        finalSelect_Table(),input$reportLevel,"_Key=",finalSelect_Key(),programTypesSelect() 
      ,sep=""))
    )
    progress$close()
    qResults_short_2 <- data.frame(
      DESTINATION = c("Unknown","Homeless","Institutional","Temporary","Permanent","TOTAL"),
      LEAVERS = c(qResults_short[1,1],qResults_short[1,2],qResults_short[1,3],qResults_short[1,4],qResults_short[1,5],qResults_short[1,6])
    )
    if(input$mdkr==FALSE) {
      return(qResults_short_2)
    } 
    else {
      qResults_short_2[6,2] <- qResults_short_2[6,2] - qResults_short_2[1,2]
      qResults_short_2 <- qResults_short_2[2:6,]
      return(qResults_short_2)
    }
  })
  
  qResults_progs <- reactive({
    if(progCount2()==0) return()
    # Take a dependency on input$update by reading it. (Nothing is actually done with the value.)
    input$update
    progress <- Progress$new(session)
    progress$set(message="Retrieving data",detail="Please wait a moment...")
    #Reactivity is invalidated unless update button is pressed
    isolate(
      # Query gender breakdown  based on user selections    
      qResults_progs <- dbGetQuery(connection,paste("
        
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
        Program_Entry_Date <= to_date('",endSelect(),"','yyyy-mm-dd') and ",
        finalSelect_Table(),input$reportLevel,"_Key=",finalSelect_Key(),programTypesSelect(),"
        GROUP BY
          PPI.Program_Key,
          Agency_Name,
          Program_Name"
      ,sep=""))
    )
    progress$close()
    names(qResults_progs) <- c("PROGRAM KEY","AGENCY NAME","PROGRAM NAME","LEAVERS","UNKNOWN DESTINATIONS (%)","PERMANENT DESTINATIONS (%)")
    if(input$mdkr==FALSE) {
      qResults_progs[,5:6] <- round(qResults_progs[,5:6]/qResults_progs[,4]*100,1)
      return(qResults_progs)
    } 
    else {
      qResults_progs[,6] <- round(qResults_progs[,6]/(qResults_progs[,4]-qResults_progs[,5])*100,1)
      qResults_progs <- qResults_progs[,!(names(qResults_progs) %in% "UNKNOWN DESTINATIONS (%)")]
      return(qResults_progs)
    }
  })
  
  ProgScore_prep <- reactive({
    input$update
    if(length(qResults_Enrolls()[,1])<=10) {return("There was insufficient data to process this request. Please extend the report period or choose another program.")}
    if(length(grep("Destinations/lib",getwd()))==0) {
      setwd(libPath3)
    }
    isolate(prog_type <- switch(programTypes_Reduced_2(),
      "ES" = "ES",
      "TH" = "TH",
      "PSH" = "PSH",
      "RapidReHousing" = "RRH"
    ))
    q <- qResults_Enrolls()
    q$DEM_RACE_OTHERMINORITY <- NA
    q$DEM_RACE_OTHERMINORITY[which(q$DEM_RACE_NONWHITE==1 & q$DEM_RACE_BLACK==0)] <- 1
    q$DEM_RACE_OTHERMINORITY[which(is.na(q$DEM_RACE_OTHERMINORITY) & !is.na(q$DEM_RACE_NONWHITE))] <- 0
    q$INC_CUSTOM_OTHER <- NA
    q$INC_CUSTOM_OTHER[which(q$INC_ANY_ENTRY==1 & q$INC_EARNED_ENTRY==0)] <- 1
    q$INC_CUSTOM_OTHER[which(is.na(q$INC_CUSTOM_OTHER) & !is.na(q$INC_ANY_ENTRY))] <- 0
    q$BEN_CUSTOM_OTHER <- NA
    q$BEN_CUSTOM_OTHER[which(q$BEN_ANY_ENTRY==1 & q$BEN_SNAP_ENTRY==0)] <- 1
    q$BEN_CUSTOM_OTHER[which(is.na(q$BEN_CUSTOM_OTHER) & !is.na(q$BEN_ANY_ENTRY))] <- 0
    MLE <- read.csv(paste("MLE",prog_type,"SFY2014.csv",sep="_"))
    logit <- MLE[1,"Estimate"]+rowSums(MLE[2:length(MLE[,1]),"Estimate"]*q[,as.character(MLE[2:length(MLE[,1]),"Parameter"])])
    program_data <- data.frame(
      PROGRAM_KEY = q$PROGRAM_KEY,
      likelihood = exp(logit)/(exp(logit)+1),
      O_DESTTYPE_P = ifelse(is.na(q$O_DESTTYPE_P),0,q$O_DESTTYPE_P)
    )
    program_data_valid <- program_data[which(!is.na(program_data$likelihood)),]
    program_data_p <- ddply(program_data_valid,"PROGRAM_KEY",summarize,p=mean(likelihood))
    program_data_agg <- ddply(program_data,"PROGRAM_KEY",summarize,dest_perm=sum(O_DESTTYPE_P),n=length(PROGRAM_KEY))
    p <- merge(program_data_p,program_data_agg,by="PROGRAM_KEY",all.x=TRUE,all.y=TRUE)
    return(p)
  })
  ProgScore <- reactive({
    if(length(ProgScore_prep())==1) return()
    p <- ProgScore_prep()
    phi <- chisq.test(x=c(p$dest_perm,p$n-p$dest_perm),p=c(p$p,1-p$p))$statistic[[1]]/p$n
    phi2 <- ifelse(p$dest_perm/p$n < p$p, phi*-1, phi)
    phi3 <- ifelse(phi2 < -1, -100, ifelse(phi2 > 1, 100, phi2*100))
    phi4 <- phi3/2+50
    setwd("../")
    return(phi4)
  })
  
  output$status <- reactive({
    if(length(ProgScore_prep())==1) return(ProgScore_prep())
      list(text=paste(round(ProgScore(),0),"/100",sep=""))
  })
  
  output$significance <- renderText({
    if(length(ProgScore_prep())==1) return()
    p <- ProgScore_prep()
    p_value <- binom.test(x=p$dest_perm,n=p$n,p=p$p)$p.value
    if(p_value<.05) {
      if(p$dest_perm/p$n<p$p) {
        return("(significantly WORSE than the Georgia average)")
      }
      else {
        return("(significantly BETTER than the Georgia average)")
      }
    }
    else {
      return("(not significantly different from the Georgia average)")
    }
  })
  
  #################################
  # TABLES
  #################################
  
  output$summaryTable <- renderDataTable({
    qResults_short()
  },options=list(
      fnRowCallback = I('
        function(nRow, aData, iDisplayIndex, iDisplayIndexFull) {
          // Column alignment
          $("td:eq(1)", nRow).css("text-align", "left");
          $("td:eq(2)", nRow).css("text-align", "right");
        }
      '),
      bAutoWidth=FALSE,bFilter=0,bPaginate=0,bLengthChange=0,bSort=0,bInfo=0,iDisplayLength=29,
        aoColumns=list(list(bSearchable=FALSE),list(bSearchable=FALSE))
    )
  )
  
  output$progsTable <- renderDataTable({
    qResults_progs()
  },options=list(
      fnRowCallback = I('
        function(nRow, aData, iDisplayIndex, iDisplayIndexFull) {
          // Column alignment
          $("td:eq(3)", nRow).css("text-align", "right");
          $("td:eq(4)", nRow).css("text-align", "right");
          $("td:eq(5)", nRow).css("text-align", "right");
        }
      '),
      bAutoWidth=FALSE,bPaginate=0,bLengthChange=0,bInfo=0,iDisplayLength=500,
      if(input$mdkr==FALSE) {
        aoColumns=list(list(bSearchable=FALSE),list(bSearchable=FALSE),list(bSearchable=FALSE),
          list(bSearchable=FALSE),list(bSearchable=FALSE),list(bSearchable=FALSE))
      }
      else {
        aoColumns=list(list(bSearchable=FALSE),list(bSearchable=FALSE),list(bSearchable=FALSE),
          list(bSearchable=FALSE),list(bSearchable=FALSE))
      }
    )
  )  
  
  
  #################################
  # PLOT
  #################################

  # Create a reactive plot based on qResults_short()
  # Then make available for output into UI
  graphdata <- reactive({
    if(progCount2()==0) return()
    # Take a dependency on input$update by reading it. (Nothing is actually done with the value.)
    # Transform qResults_short into a format acceptable for plotting
    graphdata <- qResults_short()[which(!(qResults_short()$DESTINATION %in% c("Unknown","TOTAL"))),]
    graphdata$color <- c("firebrick2","darkorchid","royalblue2","green4")
    if(input$mdkr==FALSE) {
      UNKNOWN <- data.frame( 
        DESTINATION = "Unknown",
        LEAVERS = qResults_short()[1,2],
        color = "gray45"
      )
      graphdata <- rbind(UNKNOWN,graphdata)
    }
    names(graphdata) <- c("Destination","Value","color")
    return(graphdata)
  })
  destPlot <- reactive({
    input$update 
    graphdata()
    #Graph reactivity is invalidated unless update button is pressed   
    progress <- Progress$new(session)
    progress$set(message="Creating chart",detail="Please wait a moment...")
    isolate(
      print(
        # Begin plotting with ggplot()
        # Define variables
        ggplot(data=graphdata(), aes(x=factor(Destination,levels=Destination),y=Value)) +
          # Define as bar chart
          geom_bar(fill=graphdata()$color,color="black",stat="identity") + 
          # Define bar coloers
          scale_fill_manual(values=graphdata()$color,name="Destination") +
          # Add text labels
          geom_text(aes(label = paste(Value," (",round(100*Value/sum(Value),digits=1),"%)",sep=""),
            size=10,vjust=-.5)) +
          # Remove legend
          theme(legend.position="none") +
          # Define title
            ggtitle(paste("Housing Outcomes for ", 
              if(input$reportLevel!="Program") {
                "Programs in "
              }
              else {
                paste(input$agencySelect,": ",sep="")
              },
              finalSelect_Text(),"\nReport Period: ",
              substr(beginSelect(),6,7),"/",
              substr(beginSelect(),9,10),"/",
              substr(beginSelect(),1,4)," - ",
              substr(endSelect(),6,7),"/",
              substr(endSelect(),9,10),"/",
              substr(endSelect(),1,4),sep="")) + 
          # Define axis labels
            xlab("Destination at Program Exit") + 
            ylab("Number of Clients Who Exited")
      )
    )
    progress$close()
  }) 
  output$destPlot <- renderPlot({
    destPlot()  
  })
  
  output$live_gauge <- reactive({
    if(length(ProgScore_prep())==1) return()
    if(length(grep("Destinations/lib",getwd()))==0) {
      setwd(libPath3)
    }
    isolate(prog_type <- switch(programTypes_Reduced_2(),
      "ES" = "ES",
      "TH" = "TH",
      "PSH" = "PSH",
      "RapidReHousing" = "RRH"
    ))
    program_data <- read.csv(paste(prog_type," program data 2013.csv",sep=""))
    x <- program_data[which(!is.na(program_data$decile)),]
    p <- ProgScore_prep()
    DoD <- x$decile[which(abs(x$p-p$p)==min(abs(x$p-p$p)))]
    return(DoD)
  })
  
  outcomePlot <- reactive({
    input$update 
    if(length(ProgScore_prep())==1) return()
    if(length(grep("Destinations/lib",getwd()))==0) {
      setwd(libPath3)
    }
    isolate(prog_type <- switch(programTypes_Reduced_2(),
      "ES" = "ES",
      "TH" = "TH",
      "PSH" = "PSH",
      "RapidReHousing" = "RRH"
    ))
    program_data <- read.csv(paste(prog_type," program data 2013.csv",sep=""))
    program_data2 <- NULL
    program_data2 <- program_data[which(program_data$n>10 & !is.na(program_data$p) & is.na(program_data$exclude)),]
    program_data2$avg <- mean(program_data2$adjscore)
    program_data2$progscore <- ProgScore()
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
        h2("OUTCOMES REPORT",align="center"), br(),
        (if(input$reportLevel=="Group") 
        {h3(input$groupSelect,align="center")}),
        (if(input$reportLevel %in% c("Agency","Program")) 
        {h3(input$agencySelect,align="center")}),
        (if(input$reportLevel=="Program") 
        {h3(input$programSelect,align="center")}),
        br(),
        h4(paste("Report Period:  ",dateMod(beginSelect()),"-",dateMod(endSelect())),align="center"),
        div(align="center",plotOutput("destPlot"))
      ))
    }
    else {
      isolate(div(
        h4("Main Panel",align="center"),
        if(input$reportLevel!="Program") {
          tabsetPanel(
            tabPanel("Summary",
              div(downloadButton("downloadEnrolls","Download Enrollments"),align="right"),
              dataTableOutput("summaryTable")
            ),
            tabPanel("Programs",
              div(div(downloadButton("downloadProgs","Download Programs"),align="right"),br(),dataTableOutput("progsTable"))
            ),
            tabPanel("Plot",
              plotOutput("destPlot")
            )
          )
        }
        else {
          tabsetPanel(
            tabPanel("Summary",
              div(downloadButton("downloadEnrolls","Download Enrollments"),align="right"),
              fluidRow(
                column(6,
                  gridster(width = 250, height = 250,
                    gridsterItem(col = 1, row = 2, sizex = 1, sizey = 1,
                      if(length(ProgScore_prep())>1) {justgageOutput("live_gauge", width=250, height=200)}
                    )
                  )
                ),
                column(6,
                  dataTableOutput("summaryTable")
                )
              )
            ),
            tabPanel("Performance",
              if(length(ProgScore_prep())==1) {
                div(
                  p(paste("Data were insufficient to process this request. There must be at least 10 leavers with complete data. Please extend the report period or choose another program.")),
                  br()
                )
              },
              fluidRow(
                column(4,
                  wellPanel(
                    gridster(width = 250, height = 250,
                      gridsterItem(col = 2, row = 2, sizex = 1, sizey = 1,
                        tags$div(class = 'grid_title', 'Adjusted Outcome Score'),
                        statusOutput('status')
                      )
                    ),
                    br(),
                    div(align="center",textOutput("significance"))
                  )
                ),
                column(8,
                  h4("What is the Adjusted Outcome Score?"),
                  p("A 2014 analysis of Georgia HMIS data found that, out of over 100 client characteristics, there are a handful that strongly predict how difficult it will be for a given client to exit to a permanent housing destination. This means that a ",strong("degree of difficulty")," can be calculated for your program -- based on (a) the types of clients who exited your program during the report period and (b) your program type. Once this is done, your degree of difficulty is compared with your ",strong("actual permanent destination rate.")," The final result is an adjusted score that is based on your program's actual performance, but takes into account the difficulty of your clientele. In other words, it is a better estimator of agency competence.")
                )
              ),
              br(),
              br(),
              plotOutput("outcomePlot")
            ),
            tabPanel("Plot",
              plotOutput("destPlot")
            )
          )
        }
      ))
    } 
  })
  
  
  #################################
  # DOWNLOADS
  #################################
  
  output$downloadEnrolls <- downloadHandler(
    filename = function() {
      paste('progs-', Sys.Date(), '.csv', sep='')
    },
    content = function(file) {
      write.csv(qResults_Enrolls(), file,na="",row.names=FALSE)
    }
  )
  
  output$downloadProgs <- downloadHandler(
    filename = function() {
      paste('progs-', Sys.Date(), '.csv', sep='')
    },
    content = function(file) {
      write.csv(qResults_Progs(), file,na="",row.names=FALSE)
    }
  )
})
