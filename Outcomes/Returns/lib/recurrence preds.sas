libname jdata "Z:\Documents\Pathways\Pred Measures\Datasets";

proc import datafile="Z:\Documents\Pathways\Pred Measures\Datasets\Exits_RRH_Apr13-Mar14_Rec.csv"
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
  drop HH_Head HH_Members Age_at_Entry;* SN_SUBSTABUSE_ENTRY;
run;


/* Emergency shelter */
ods rtf;
proc logistic data=HMIS_data_2 plots=roc;
  model Recurrence (event="1") = PE_LENGTH_OF_STAY--BEN_RENTTEMP_EXIT 
    / selection=backward rsquare outroc=roc1;
  where z_Program_Type_Code = 1;
  output out = outdata_test;
run;
ods rtf close;

ods rtf;
proc logistic data=HMIS_data_2 plots=roc;
  model 
    Recurrence (event="1") = PE_LENGTH_OF_STAY DEM_RACE_NONWHITE DEM_ETHNICITY_LATINO DEM_GENDER_MALE AGE_18TO24_ENTRY
	HH_HAS_CHILDREN CS_PNR_HOMELESS CS_PRIOR_HOMELESS_ENROLL SN_PHYSDIS_ENTRY SN_CHRONHEALTH_ENTRY SN_SUBSTABUSE_ENTRY
	SN_DV_ENTRY INC_ONE_ENTRY INC_MULTIPLE_ENTRY INC_EARNED_ENTRY INC_UNEMPINS_ENTRY INC_SSI_ENTRY INC_SSDI_ENTRY
	INC_TANF_ENTRY INC_SS_ENTRY INC_OTHERSOURCE_ENTRY BEN_MULTIPLE_ENTRY BEN_MEDICAID_ENTRY BEN_OTHER_ENTRY
	INC_EARNEDINCOME_INCREASE INC_UNEMPINS_EXIT INC_VETDISPMT_EXIT INC_CHILDSUPPORT_EXIT BEN_ONE_EXIT BEN_SNAP_EXIT
	BEN_MEDICAID_EXIT BEN_VAMS_EXIT
    / lackfit;
  where z_Program_Type_Code = 1;
  output out = outdata_es l = Lower p = Predicted u = Upper RESDEV=resdev;
run;
ods rtf close;

/* Transitional housing */
ods rtf;
proc logistic data=HMIS_data_2 plots=roc;
  model Recurrence(event="1") = PE_LENGTH_OF_STAY--BEN_RENTTEMP_EXIT 
    / selection=backward rsquare outroc=roc1;
  where z_Program_Type_Code = 2;
run;
ods rtf close;

ods rtf;
proc logistic data=HMIS_data_2 plots=roc;
  model 
    Recurrence (event="1") = DEM_GENDER_MALE DEM_VET CS_PRIOR_HOMELESS_ENROLL INC_SSDI_ENTRY INC_EARNEDINCOME_INCREASE
    / lackfit;
  where z_Program_Type_Code = 2;
  output out = outdata_th l = Lower p = Predicted u = Upper RESDEV=resdev;
run;
ods rtf close;

/* Permanent supportive housing */
ods rtf;
proc logistic data=HMIS_data_2 plots=roc;
  model Recurrence (event="1") = PE_LENGTH_OF_STAY--BEN_RENTTEMP_EXIT 
    / selection=backward rsquare outroc=roc1;
  where z_Program_Type_Code = 3;
run;
ods rtf close;

ods rtf;
proc logistic data=HMIS_data_2 plots=roc;
  model 
    Recurrence (event="1") = DEM_RACE_BLACK DEM_GENDER_MALE BEN_MULTIPLE_ENTRY BEN_ANY_EXIT BEN_ONE_EXIT
    / lackfit;
  where z_Program_Type_Code = 3;
  output out = outdata_psh l = Lower p = Predicted u = Upper RESDEV=resdev;
run;
ods rtf close;

/* Rapid re-housing */
proc logistic data=HMIS_data_2 plots=roc;
  model Recurrence (event="1") = PE_LENGTH_OF_STAY--BEN_RENTTEMP_EXIT
    / selection=backward rsquare outroc=roc1;
  where z_Program_Type_Code = 14;
run;

ods rtf;
proc logistic data=HMIS_data_2 plots=roc;
  model 
    Recurrence (event="1") = DEM_VET BEN_TANFCC_ENTRY
    / lackfit;
  where z_Program_Type_Code = 14;
  output out = outdata_rrh l = Lower p = Predicted u = Upper RESDEV=resdev;
run;
ods rtf close;


/* Residuals plot */
proc gplot data=outdata_rrh;
  plot resdev*predicted;
  symbol1 v=hash c=magenta h=1 w=1;
run; quit;
