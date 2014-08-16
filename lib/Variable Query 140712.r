##########################################################################
# Title: Querying indicator variables for general use and for use in predictive models
# Programmer: Jason Rodriguez
# Last updated: 7/12/2014
# Output file: Exits_[SFY].csv
##########################################################################

setwd("~")
# source("connectionkey.r")

for(i in c(1,2,3,13,14)) {

query <- dbGetQuery(connection,paste("
  SELECT unique 
    PE.Program_Enrollment_Key z_Program_Enrollment_Key,
    PE.Client_Key z_Client_Key, 
    PE.Household_Key z_Household_Key,
    Program_Type_Code z_Program_Type_Code,
    Program_Type z_Program_Type,
    Agency_Key z_Agency_Key,
    Agency_Name z_Agency_Name,
    PE.Program_Key z_Program_Key,
    Program_Name z_Program_Name,
    /*
    /* PROGRAM ENROLLMENT CHARACTERISTICS */
    Program_Entry_Date PE_Program_Entry_Date,
    Program_Exit_Date PE_Program_Exit_Date,
    Program_Exit_Date - Program_Entry_Date + 1 PE_Length_of_Stay,
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
    /* OUTCOME */
      Destination_Code Dest_Code,
      case when Destination_Code in (3,10,11,23,22,21,20,19) then 1 else (case when not Destination_Code in (8,9) then 0 end) end Dest_Permanent,
      case when Destination_Code in (807, 806, 122, 14, 13, 12) then 1 else (case when not Destination_Code in (8,9) then 0 end) end Dest_Temporary,
      case when Destination_Code in (4, 5, 6, 7, 15, 106, 107, 109) then 1 else (case when not Destination_Code in (8,9) then 0 end) end Dest_Institutional,
      case when Destination_Code in (110, 16, 2, 1, 111, 121) then 1 else (case when not Destination_Code in (8,9) then 0 end) end Dest_Homeless,
      case when Destination_Code is null or Destination_Code in (8,9,17) then 1 else (case when not Destination_Code in (8,9) then 0 end) end Dest_Unknown
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
  WHERE
    Program_Exit_Date >= to_date('7/1/2013','mm/dd/yyyy') and
    Program_Exit_Date <= to_date('6/30/2014','mm/dd/yyyy') and 
    Program_Type_Code in (",i,") and
    not Agency_Key in (2171,11128,82,15308,8908) and
    not PPI.Program_Key in (6996)
",sep=""))


summary(query)
length(query[,1])

setwd("Z:/Documents/Pathways/Pred Measures/Datasets")
if(i==1) {
  write.table(query,file="Exits_SFY2014.csv",sep=",",na="",row.names=FALSE)
} else {
  write.table(query,file="Exits_SFY2014.csv",sep=",",na="",row.names=FALSE,col.names=FALSE,append=TRUE)
}

}

query <- read.csv("Exits_SFY2014.csv",stringsAsFactors=FALSE)
setwd("Z:/GitHub/shiny-apps-oracle/lib")
query2 <- merge(query,read.csv("County classifications.csv"),by.x=c("Z_AGENCY_NAME","Z_PROGRAM_NAME"),by.y=c("AGENCY_NAME","PROGRAM_NAME"),all.x=TRUE)
write.table(query2,file="Exits_SFY2014.csv",sep=",",na="",row.names=FALSE,col.names=FALSE,append=TRUE)
