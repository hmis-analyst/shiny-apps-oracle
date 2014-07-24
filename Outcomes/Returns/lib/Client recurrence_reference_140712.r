##########################################################################
# Title: Testing heads of household for recurrence
# Programmer: Jason Rodriguez
# Last updated: 7/12/2014
# Output file: 
##########################################################################

# source("connectionkey.r")

setwd("Z:/Documents/Pathways/Pred Measures/Datasets")

for (j in c("1","2","3","14")) {

prog_type <- switch(j,
  "1" = "ES",
  "2" = "TH",
  "3" = "PSH",
  "14" = "RRH"
)

Exits <- read.csv(paste("Exits",prog_type,"Apr13-Mar14.csv",sep="_"),stringsAsFactors=FALSE)
Exits2 <- 
  if(j %in% c(1,2,3,14)) {
    Exits[which(Exits$DEST_PERMANENT==1 & Exits$HH_HEAD==1),]
  } 

# Get number of observations in Exits, store in "n"
n <- length(Exits2[,1])

for (i in 1:n) {
  # Retrieve the observation's client key, store in "Client"
  Client <- Exits2[i,"Z_CLIENT_KEY"]
  # Retrieve the date of enrollment termination, store in "Date"
  Date <- substr(Exits2[i,"PE_PROGRAM_EXIT_DATE"],0,10)
  # Establishing that a client has returned to homelessness, Rule L.2.b
  # "[R]e-entry into the same program type within 30 days of program exit ... is not counted as a return to homelessness."
  # Only applies to clients leaving TH, PSH, or RRH programs.
  Rule_R2b <- data.frame(ProgLimit="",Wait30="")
  Rule_R2b[,1:2] <- 
    if(!is.na(Exits2[i,"DEST_CODE"])) {
      # Destination code #20 is "Rental by client with other (non-VASH ongoing housing subsidy)"
      # Destination category 20 could refer to a rapid re-housing enrollment.
      if(Exits2[i,"DEST_CODE"] == 20) {
        c("and not Program_Type_Code = 14","+30")
      } else {
        # Permanent supportive housing
        if(Exits2[i,"DEST_CODE"] == 3) {
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
        Exits2[i,"Return_Date"] <- if(Exits2[i,"DEST_CODE"] %in% c(1,2,16)) {
          Exits2[i,"PE_PROGRAM_EXIT_DATE"]
        } else {
          # Otherwise, Return_Date = Next_Homeless
          if(length(Temp[1,"NEXT_HOMELESS"])==0) {NA}
          else {
            max(Temp[1,"NEXT_HOMELESS"],Exits2[i,"PE_PROGRAM_EXIT_DATE"])
          }
        }
        # Create new Exits2 column called Next_Entry
        Exits2[i,"Next_Entry"] <- 
          if(length(Temp[1,"NEXT_ENTRY"])==0) {NA}
          else {Temp[1,"NEXT_ENTRY"]}
        # Create new Exits2 column to identify a recurrent enrollment. It is set to 0 if Return_Date is null; else, it is set to 1.
        Exits2[i,"Recurrence"] <- if(is.na(Exits2[i,"Return_Date"])) {1} else {0}
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
}
remove(Temp)

#-------------------
    # Enrollment's termination date is converted to valid R date format
    Exits2[,"PE_PROGRAM_EXIT_DATE"] <- as.Date(Exits2[,"PE_PROGRAM_EXIT_DATE"])
    # Client's return date and next program entry are converted to valid R date format. (I'm not sure why it forces me to do it a different way.)
    Exits2[,"Return_Date"] <- as.Date(as.POSIXct(Exits2[,"Return_Date"], origin = "1970-01-01"))
    Exits2[,"Next_Entry"] <- as.Date(as.POSIXct(Exits2[,"Next_Entry"], origin = "1970-01-01"))
    # Calculates how many days it took for the client to return to homelessness after the enrollment's termination date
    Exits2[,"Days_Until_Return"] <- as.numeric(Exits2[,"Return_Date"] - Exits2[,"PE_PROGRAM_EXIT_DATE"])

#-------------------
# Rule R.3
Exits2[which(Exits2$Days_Until_Return>90),"Recurrence"] <- 0

#-------------------
# Rules L.1.b, L.2.b, and L.3.b
# Exit3 removes enrollments with a "false exit" from homelessness
# See "Rules for leaving homelessness" at https://github.com/hmis-analyst/shiny-apps-oracle/wiki/Homelessness-Recurrence
Exits3 <-
  Exits2[
    which(
      # Rule #1b
      ( 
        j %in% c(1,2) &
        Exits2[,"DEST_PERMANENT"]==1 & 
        Exits2[,"Days_Until_Return"]>=30
      ) | 
      # Rule #1c
      (
        i %in% c(1,2) &
        (
          Exits2[,"DEST_TEMPORARY"]==1 | 
          Exits2[,"DEST_INSTITUTIONAL"]==1
        ) & 
        Exits2[,"Days_Until_Return"]>=90
      ) |
      # Rule #3
      (
        i %in% c(3,14) &
        Exits2[,"PE_LENGTH_OF_STAY"] + Exits2[,"Days_Until_Return"] >= 30
      ) |
      # Rule #4
      Exits2[,"Recurrence"]==0
    )
  ,]

write.csv(Exits2,file=paste("Exits",prog_type,"Apr13-Mar14_Rec.csv",sep="_"),na="",row.names=FALSE)

}
prog_type<-"RRH"
query <- read.csv(paste("Exits",prog_type,"Apr13-Mar14_Rec.csv",sep="_"))
summary(query)
length(query[,1])