##########################################################################
# Title: Calculating Reference Outcome Scores for Recurrence App
# Programmer: Jason Rodriguez
# Last updated: 7/12/2014
# Output file: Program data_[program type]_[SFY].csv
##########################################################################

# source("connectionkey.r")

library(plyr)
setwd("Z:/Documents/Pathways/Pred Measures/Datasets")

#--------------
# USER INPUT
# Specify program type (ES, TH, RRH, PSH, or Prev)
prog_type <- "Prev"
#--------------

# Read HMIS Predictors query into new object.
# This requires that query was outputted to the HMIS Predictors.csv file beforehand.
# Run Predictor Query [date].r to update this file.
query <- read.csv(paste("Exits",prog_type,"Apr13-Mar14_Rec.csv",sep="_"))

# Create new variables
query$DEM_RACE_OTHERMINORITY <- ifelse(
  query$DEM_RACE_NONWHITE==1 & query$DEM_RACE_BLACK==0,
  1,
  ifelse(
    !is.na(query$DEM_RACE_NONWHITE),
    0,
    NA
  )
)
query$INC_CUSTOM_OTHER <- ifelse(
  query$INC_ANY_ENTRY==1 & query$INC_EARNED_ENTRY==0,
  1,
  ifelse(
    !is.na(query$INC_ANY_ENTRY),
    0,
    NA
  )
)
query$BEN_CUSTOM_OTHER <- ifelse(
  query$BEN_ANY_ENTRY==1 & query$BEN_SNAP_ENTRY==0,
  1,
  ifelse(
    !is.na(query$BEN_ANY_ENTRY),
    0,
    NA
  )
)

setwd("Z:/Documents/Pathways/Pred Measures/Datasets")

# Import maximum likelihood estimates (MLE) for selected program type
MLE <- read.csv(paste("Z:/GitHub/shiny-apps-oracle/Outcomes/Recurrence/lib/MLE",prog_type,"Apr13-Mar14_Rec.csv",sep="_"))

# Compute logits
logit <- MLE[1,"Estimate"]+rowSums(MLE[2:length(MLE[,1]),"Estimate"]*query[,as.character(MLE[2:length(MLE[,1]),"Parameter"])])

# Compute permanent destination likelihoods
likelihood <- exp(logit)/(exp(logit)+1)

# Create new data frame showing program key, likelihood, and actual outcome for each client
program_data <- data.frame(
  PROGRAM_KEY = query$Z_PROGRAM_KEY,
  likelihood = likelihood,
  O_DESTTYPE_P = ifelse(is.na(query$O_DESTTYPE_P),0,query$O_DESTTYPE_P)
)

# Reduce program data to clients where a likelihood has been computed
program_data_valid <- program_data[which(!is.na(program_data$likelihood)),]

# Aggregate the data into program-level statistics
program_data_p <- ddply(program_data_valid,"PROGRAM_KEY",summarize,p=mean(likelihood))
program_data_agg <- ddply(program_data,"PROGRAM_KEY",summarize,dest_perm=sum(O_DESTTYPE_P),n=length(PROGRAM_KEY))
program_data_agg2 <- merge(program_data_p,program_data_agg,by="PROGRAM_KEY",all.x=TRUE,all.y=TRUE)

#Calculate Adjusted Outcome Scores, based on phi statistic
attach(program_data_agg2)
program_data_agg2$adjscore <- NA
for (i in 1:length(program_data_agg2[,1])) {
  if(!is.na(program_data_agg2$p[i])) {
    phi <- chisq.test(x=c(dest_perm[i],n[i]-dest_perm[i]),p=c(p[i],1-p[i]))$statistic[[1]]/n[i]
    phi2 <- ifelse(dest_perm[i]/n[i] < p[i], phi*-1, phi)
    phi3 <- ifelse(phi2 < -1, -100, ifelse(phi2 > 1, 100, phi2*100))
    phi4 <- phi3/2+50
    program_data_agg2$adjscore[i] <- phi4
  }
}

# Discard programs where 10 or fewer clients exited
program_data_agg3 <- program_data_agg2[which(n>10),]
detach(program_data_agg2)

# Divide programs' Adjusted Outcome Scores into deciles
attach(program_data_agg3)
program_data_agg3$decile <- cbind(cut(p,quantile(p,(0:10)/10,na.rm=TRUE),include.lowest=TRUE))
detach(program_data_agg3)
program_data_agg4 <- merge(program_data_agg2,program_data_agg3[,c("PROGRAM_KEY","decile")],by="PROGRAM_KEY",all.x=TRUE)

# Set the default for "Exclude" to missing. 
# User can manually exclude a program by opening CSV file and typing any value into the Exclude column.
program_data_agg4$exclude <- NA

# Draw a histogram of Adjusted Outcome Scores. For any outliers, set Exclude=1. Redraw histogram.
hist(program_data_agg4[which(!is.na(program_data_agg4$decile)),"adjscore"])
# program_data_agg4[which(program_data_agg4$adjscore <40),"exclude"] <- 1
hist(program_data_agg4[which(!is.na(program_data_agg4$decile) & is.na(program_data_agg4$exclude)),"adjscore"])

# Save program-level performance scores
setwd("Z:/GitHub/shiny-apps-oracle/Recurrence/Destinations/lib")
write.csv(program_data_agg4,file=paste("Prog data",prog_type,"Apr13-Mar14_Rec.csv",sep="_"),na="",row.names=FALSE)

