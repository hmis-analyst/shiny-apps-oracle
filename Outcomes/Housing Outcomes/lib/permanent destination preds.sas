libname jdata "Z:\Documents\Pathways\Pred Measures\Datasets";
libname outdata "Z:\GitHub\shiny-apps-oracle\Outcomes\Housing Outcomes";

proc import datafile="Z:\Documents\Pathways\Pred Measures\Datasets\Exits_SFY2014_Rec.csv"
  out=HMIS_data
  dbms=csv
  replace;
  getnames=yes;
run;

ods rtf;
proc contents data=HMIS_data_2;
run;
ods rtf close;

data HMIS_data_2;
  set HMIS_data;
  if z_Program_Type_Code=1 then ProgType_ES=1; else ProgType_ES=0;
  if z_Program_Type_Code=2 then ProgType_TH=1; else ProgType_TH=0;
  if z_Program_Type_Code=3 then ProgType_PSH=1; else ProgType_PSH=0;
  if z_Program_Type_Code=13 then ProgType_Prev=1; else ProgType_Prev=0;
  if z_Program_Type_Code=14 then ProgType_RRH=1; else ProgType_RRH=0;
  Inc_Any_Exit = Any_Income_Exit;
  *where HH_Head=1;
  drop Inc_One_Entry Inc_One_Exit Inc_OtherSource_Entry Inc_OtherSource_Exit Ben_TANFOther_Entry Ben_TANFOther_Exit Ben_Other_Entry Ben_Other_Exit Inc_EarnedIncome_Same
    Ben_One_Entry Ben_One_Exit Any_Income_Exit hh_members;
run;

proc freq data=hmis_data_2;
tables hh_head*Dest_perm_90;
run;


/* Emergency shelter */
proc logistic data=HMIS_data_2 plots=roc;
  model Dest_Perm_90 (event="1") = PE_LENGTH_OF_STAY--BEN_RENTTEMP_EXIT Rural 
    / selection=backward rsquare outroc=roc1;
  where progtype_es=1;
run;

* Model 1;
ods rtf;
proc logistic data=HMIS_data_2 plots=roc outest=outest_es;
  model 
    Dest_Perm_90 (event="1") = PE_Length_of_Stay Dem_Race_Nonwhite Dem_Gender_Male Age_Under18_Exit Age_18to24_Exit HH_Unaccompanied
	HH_Has_Teenage_Male CS_PNR_Homeless CS_Prior_Homeless_Enroll SN_DisabCond_Entry SN_PhysDis_Entry SN_HIV_Entry
	Inc_Multiple_Entry Inc_VetDisPmt_Entry Inc_SS_Entry Inc_VetPension_Entry Ben_SCHIP_Entry 
	Inc_EarnedIncome_Increase Inc_Earned_Exit Inc_SSI_Exit Inc_SSDI_Exit Ben_Any_Exit Ben_SNAP_Exit Ben_SCHIP_Exit Rural
    / lackfit;
  where progtype_es=1;
  output out = outdata_es l = Lower p = Predicted u = Upper RESDEV=resdev;
run;
ods rtf close;

/* Residuals plot */
proc gplot data=outdata_es;
  plot resdev*predicted;
  symbol1 v=hash c=magenta h=1 w=1;
run; quit;


/* Transitional housing */
proc logistic data=HMIS_data_2 plots=roc;
  model Dest_Perm_90 (event="1") = PE_LENGTH_OF_STAY--BEN_RENTTEMP_EXIT Rural
    / selection=backward rsquare outroc=roc1;
  where progtype_th=1;
run;

* Model 1;
ods rtf;
proc logistic data=HMIS_data_2 plots=roc outest=outest_th;
  model 
    Dest_Perm_90 (event="1") = PE_Length_of_Stay Dem_Race_Black Dem_Gender_Male Dem_Vet Age_Under18_Exit HH_Not_Only_Adult
	CS_Prior_Homeless_Enroll SN_PhysDis_Entry SN_SubstAbuse_Entry Ben_VAMS_Entry Ben_TANFCC_Entry Inc_EarnedIncome_Increase Inc_Earned_Exit
	Inc_SSI_Exit Inc_SSDI_Exit Ben_Any_Exit Ben_SNAP_Exit Rural
    / lackfit;
  where progtype_th=1;
  output out = outdata_th l = Lower p = Predicted u = Upper RESDEV=resdev;
run;
ods rtf close;

/* Residuals plot */
proc gplot data=outdata_th;
  plot resdev*predicted;
  symbol1 v=hash c=magenta h=1 w=1;
run; quit;


/* Permanent supportive housing */
proc logistic data=HMIS_data_2 plots=roc;
  model Dest_Perm_90 (event="1") = PE_LENGTH_OF_STAY--BEN_RENTTEMP_EXIT Rural 
    / selection=backward rsquare outroc=roc1;
  where progtype_psh=1;
run;

* Model 1;
ods rtf;
proc logistic data=HMIS_data_2 plots=roc outest=outest_psh;
  model 
    Dest_Perm_90 (event="1") = PE_Length_of_Stay Dem_Gender_Male Dem_Vet HH_Not_Only_Adult Ben_Multiple_Entry Inc_SSI_Exit Ben_Multiple_Exit;
  where progtype_psh=1;
  output out = outdata_psh l = Lower p = Predicted u = Upper RESDEV=resdev;
run;
ods rtf close;

/* Residuals plot */
proc gplot data=outdata_psh;
  plot resdev*predicted;
  symbol1 v=hash c=magenta h=1 w=1;
run; quit;


/* Rapid re-housing */
proc logistic data=HMIS_data_2 plots=roc;
  model Dest_Perm_90 (event="1") = PE_LENGTH_OF_STAY--BEN_RENTTEMP_EXIT Rural 
    / selection=backward rsquare outroc=roc1;
  where progtype_rrh=1;
run;

* Model 1;
ods rtf;
proc logistic data=HMIS_data_2 plots=roc outest=outest_rrh;
  model 
    Dest_Perm_90 (event="1") = PE_Length_of_Stay Dem_Vet Age_18to24_Entry Age_18to24_Exit HH_Head HH_Unaccompanied HH_Not_Only_Adult 
    HH_Has_Teenager CS_PNR_Homeless CS_Prior_Homeless_Enroll SN_DevelDis_Entry SN_HIV_Entry SN_MentIll_Entry SN_DV_Entry Inc_Any_Entry
	Inc_Multiple_Entry Inc_Earned_Entry Inc_TANF_Entry Inc_ChildSupport_Entry Inc_Alimony_Entry Ben_SNAP_Entry Ben_Medicaid_Entry
	Ben_SCHIP_Entry Inc_Earned_Exit Ben_RentTemp_Exit;
  where progtype_rrh=1;
  output out = outdata_rrh l = Lower p = Predicted u = Upper RESDEV=resdev;
run;
ods rtf close;

/* Residuals plot */
proc gplot data=outdata_rrh;
  plot resdev*predicted;
  symbol1 v=hash c=magenta h=1 w=1;
run; quit;


/* Prevention */
proc logistic data=HMIS_data_2 plots=roc;
  model Dest_Perm_90 (event="1") = PE_LENGTH_OF_STAY--BEN_RENTTEMP_EXIT Rural 
    / selection=backward rsquare outroc=roc1;
  where progtype_prev=1;
run;

* Model 1;
ods rtf;
proc logistic data=HMIS_data_2 plots=roc outest=outest_prev;
  model 
    Dest_Perm_90 (event="1") = PE_Length_of_Stay Dem_Race_Black HH_Not_Only_Adult HH_Has_Teenage_Female HH_Has_Teenager CS_Prior_Homeless_Enroll
	SN_PhysDis_Entry SN_MentIll_Entry SN_SubstAbuse_Entry Inc_VetDisPmt_Entry Ben_Multiple_Entry Ben_Medicaid_Entry Ben_SCHIP_Entry Ben_VAMS_Entry
	Inc_VetDisPmt_Exit Ben_SCHIP_Exit;
  where progtype_prev=1;
  output out = outdata_prev l = Lower p = Predicted u = Upper RESDEV=resdev;
run;
ods rtf close;

/* Residuals plot */
proc gplot data=outdata_prev;
  plot resdev*predicted;
  symbol1 v=hash c=magenta h=1 w=1;
run; quit;


