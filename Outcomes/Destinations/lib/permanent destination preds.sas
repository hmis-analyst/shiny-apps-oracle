libname jdata "Z:\Documents\Pathways\Pred Measures\Datasets";

proc import datafile="Z:\Documents\Pathways\Pred Measures\Datasets\HMIS Preds_Prev_SFY2014.csv"
  out=HMIS_data
  dbms=csv
  replace;
  getnames=yes;
run;

ods rtf;
proc contents data=HMIS_data;
run;
ods rtf;

data HMIS_data_2;
  set HMIS_data;
  if Dem_Race_Nonwhite=1 and Dem_Race_Black = 0 then Dem_Race_OtherMinority = 1;
    else if Dem_Race_Nonwhite ~= . then Dem_Race_OtherMinority = 0;
  if Inc_Any_Entry = 1 and Inc_Earned_Entry = 0 then Inc_Custom_Other = 1;
    else if Inc_Any_Entry ~=. then Inc_Custom_Other = 0;
  if Ben_Any_Entry = 1 and Ben_SNAP_Entry = 0 then Ben_Custom_Other = 1;
    else if Ben_Any_Entry ~=. then Ben_Custom_Other = 0;
  where HH_Head=1;
  drop HH_Head HH_Members Age_at_Entry;
run;

proc mi data=HMIS_data_2 out=HMIS_data_3 MINIMUM=0 MAXIMUM=1 ROUND=1;
	var DEM_RACE_NONWHITE--BEN_RENTTEMP_EXIT;
run;

/* Emergency shelter */
proc logistic data=HMIS_data_2 plots=roc;
  model o_DestType_P (event="1") = DEM_RACE_NONWHITE--BEN_RENTTEMP_EXIT Dem_Race_OtherMinority
    / selection=backward rsquare outroc=roc1;
  where z_Program_Type_Code = 1;
  output out = outdata_test;
run;

ods rtf;
proc logistic data=HMIS_data_2 plots=roc;
  model 
    o_DestType_P (event="1") = Dem_Race_Nonwhite Dem_Ethnicity_Latino Dem_Gender_Male Dem_Vet HH_Has_Children HH_Not_Only_Adult CS_PNR_Homeless
	CS_Prior_Homeless_Enroll SN_PhysDis_Entry SN_HIV_Entry Inc_SSDI_Entry Inc_VetDisPmt_Entry Inc_OtherSource_Entry Inc_EarnedIncome_Same 
    Inc_Earned_Exit Inc_SSI_Exit Inc_SSDI_Exit Inc_OtherSource_Exit Ben_One_Exit
    / lackfit;
  where z_Program_Type_Code = 1;
  output out = outdata_es l = Lower p = Predicted u = Upper RESDEV=resdev;
run;
ods rtf close;

/* Transitional housing */
proc logistic data=HMIS_data_2 plots=roc;
  model o_DestType_P (event="1") = DEM_RACE_NONWHITE--BEN_RENTTEMP_EXIT Dem_Race_OtherMinority
    / selection=backward rsquare outroc=roc1;
  where z_Program_Type_Code = 2;
run;

ods rtf;
proc logistic data=HMIS_data_2 plots=roc;
  model 
    o_DestType_P (event="1") = Dem_Race_Otherminority Dem_Race_Black Dem_Gender_Male Dem_Vet HH_Has_Children SN_HIV_Entry SN_SubstAbuse_Entry 
	Inc_Custom_Other Inc_Earned_Entry Inc_UnempIns_Entry Ben_Custom_Other Ben_One_Entry Ben_Multiple_Entry Ben_SNAP_Entry Inc_EarnedIncome_Same Inc_One_Exit 
    Inc_Multiple_Exit Inc_UnempIns_Exit Inc_ChildSupport_Exit
    / lackfit;
  where z_Program_Type_Code = 2;
  output out = outdata_th l = Lower p = Predicted u = Upper RESDEV=resdev;
run;
ods rtf close;

/* Permanent supportive housing */
proc logistic data=HMIS_data_2 plots=roc;
  model o_DestType_P (event="1") = DEM_RACE_NONWHITE--BEN_RENTTEMP_EXIT Dem_Race_OtherMinority
    / selection=backward rsquare outroc=roc1;
  where z_Program_Type_Code = 3;
run;

ods rtf;
proc logistic data=HMIS_data_2 plots=roc;
  model 
    o_DestType_P (event="1") = Dem_Gender_Male HH_Has_Children SN_DisabCond_Entry Inc_VetDisPmt_Entry Ben_Custom_Other Ben_SNAP_Entry
    / lackfit;
  where z_Program_Type_Code = 3;
  output out = outdata_psh l = Lower p = Predicted u = Upper RESDEV=resdev;
run;
ods rtf close;

/* Rapid re-housing */
proc logistic data=HMIS_data_2 plots=roc;
  model o_DestType_P (event="1") = DEM_RACE_NONWHITE--BEN_RENTTEMP_EXIT
    / selection=backward rsquare outroc=roc1;
  where z_Program_Type_Code = 14;
run;

ods rtf;
proc logistic data=HMIS_data_2 plots=roc;
  model 
    o_DestType_P (event="1") = Dem_Vet SN_DevelDis_Entry Inc_SSI_Entry Inc_VetDisPmt_Entry Inc_ChildSupport_Entry Inc_Alimony_Entry
    Ben_Other_Entry Inc_EarnedIncome_Increase Inc_One_Exit Inc_Multiple_Exit Ben_RentAssist_Exit
    / lackfit;
  where z_Program_Type_Code = 14;
  output out = outdata_rrh l = Lower p = Predicted u = Upper RESDEV=resdev;
run;
ods rtf close;

/* Prevention */
proc logistic data=HMIS_data_2 plots=roc;
  model o_DestType_P (event="1") = DEM_RACE_NONWHITE--SN_DISABCOND_ENTRY INC_ANY_ENTRY--SN_DISABCOND_EXIT
    / selection=backward rsquare outroc=roc1;
  where z_Program_Type_Code = 13;
run;

ods rtf;
proc logistic data=HMIS_data_2 plots=roc;
  model 
    o_DestType_P (event="1") = CS_Prior_Homeless_Enroll SN_DisabCond_Entry Inc_SSDI_Entry Ben_Multiple_Entry Ben_Medicaid_Entry
    / lackfit;
  where z_Program_Type_Code = 13;
  output out = outdata_rrh l = Lower p = Predicted u = Upper RESDEV=resdev;
run;
ods rtf close;


/* Residuals plot */
proc gplot data=outdata_rrh;
  plot resdev*predicted;
  symbol1 v=hash c=magenta h=1 w=1;
run; quit;
