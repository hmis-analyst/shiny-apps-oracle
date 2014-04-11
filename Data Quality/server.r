# .libPaths("~/R/win-library/3.0")
# install.packages("devtools",repos="http://cran.rstudio.com/")
# devtools::install_github("shiny-incubator", "rstudio")
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


# Establish JDBC connection using RJDBC
source("~/HMIS Data Analyst/lib/key.r")

# Create a function to modify date format
dateMod <- function(x) {paste(substr(x,6,7),"/",substr(x,9,10),"/",substr(x,1,4),sep="")}

# Define server logic required to query/graph HMIS gender data
shinyServer(function(input, output, session) {
  
  
  #################################
  # USER SELECTIONS
  #################################
  
  # Store report level selection
  reportLevel <- reactive({
    # Take a dependency on input$update by reading it. (Nothing is actually done with the value.)
    input$update
    #Reactivity is invalidated unless update button is pressed
    isolate(input$reportLevel)
  })
  output$text1 <- renderText({ 
    # Take a dependency on input$update by reading it. (Nothing is actually done with the value.)
    input$update
    #Reactivity is invalidated unless update button is pressed
    isolate(ifelse(input$reportLevel=="Program","Clients","Programs"))
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
        WHERE Agency_Name like '",unlist(strsplit(input$agencySelect,"'"))[1],"%' 
        ORDER BY Program_Name"
      ,sep=""))[[1]]
    )))
  })
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
        } 
        else {
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
  # Store the name of the final group/agency/program selection
  finalSelect_Text <- reactive({
    # Take a dependency on input$update by reading it. (Nothing is actually done with the value.)
    input$update
    #Reactivity is invalidated unless update button is pressed
    isolate(
      if(input$reportLevel=="Group") {
        input$groupSelect
      } 
      else{
        if(input$reportLevel=="Agency") {
          input$agencySelect
        } 
        else {
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
      RIGHT JOIN Program_Community_Information PCI 
        on PPI.Program_Key = PCI.Program_Key 
      RIGHT JOIN Community_Group_Information CGI 
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
  })
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
        Program_Type is not null and ",
        finalSelect_Table(),input$reportLevel,"_Key=",finalSelect_Key(),"
      ORDER BY Program_Type",
    sep=""))[[1]])
  })
  output$programTypes <- renderUI({
    if(progCount()==0) return(HTML(paste("<p>Program types</p><p><em>This",input$reportLevel,"has no programs")))
    checkboxGroupInput("programTypes","Program types",as.list(programTypes_Reduced()),
      selected=programTypes_Reduced())
  })
  programTypesSelect <- reactive({
    if(input$reportLevel=="Program") return()
    programTypesSelect <- " and Program_Type in ("
    for(i in 1:length(input$programTypes)) {
      programTypesSelect <- 
        paste(programTypesSelect,"'",ifelse(i>1,",'",""),input$programTypes[i],sep="")
    }
    programTypesSelect <- paste(programTypesSelect,"')",sep="")
  })
  
  
  #################################
  # MAIN PANEL UI
  #################################
  
  output$mainPanel <- renderUI({
    # Take a dependency on input$update by reading it. (Nothing is actually done with the value.)
    input$update
    
    if (input$printable==TRUE) {
      #Reactivity is invalidated unless update button is pressed
      isolate(mainPanel(
        h2("DATA QUALITY REPORT",align="center"), br(),
        (if(input$reportLevel=="Group") 
        {h3(input$groupSelect,align="center")}),
        (if(input$reportLevel %in% c("Agency","Program")) 
        {h3(input$agencySelect,align="center")}),
        (if(input$reportLevel=="Program") 
        {h3(input$programSelect,align="center")}),
        br(),
        h4(paste("Report Period:  ",dateMod(beginSelect()),"-",dateMod(endSelect())),align="center"),
        h3(class="break","Summary",align="center"),
        dataTableOutput("dqTable_short"),
        h3(class="break","Data Elements",align="center"),
        dataTableOutput("dqTable"),
        h3(class="break",textOutput("text1"),align="center"),
        if(input$reportLevel=="Program") {dataTableOutput("Violations")}
        else {dataTableOutput("progsTable")},
        br(), br(),
        div(align="center",plotOutput("plot"))
      ))
    }
    else {
      div(
        h4("Main Panel",align="center"),
        tabsetPanel(
          tabPanel("Summary",
                   div(downloadButton("downloadSummary","Download Summary"),align="right"),
                   dataTableOutput("dqTable_short")
          ),
          tabPanel("Data Elements",
                   div(downloadButton("downloadDQ","Download Elements"),align="right"),
                   dataTableOutput("dqTable")
          ),
          tabPanel(textOutput("text1"),
                   if(input$reportLevel=="Program") 
                   {div(div(downloadButton("downloadClients","Download Clients"),align="right"),dataTableOutput("Violations"))}
                   else 
                   {div(div(downloadButton("downloadProgs","Download Programs"),align="right"),br(),dataTableOutput("progsTable"))}
          ),
          tabPanel("Plot",
                   plotOutput("plot")
          )
        )
      )
    } 
  })
  

  #################################
  # MAIN QUERY
  #################################
    
  dataQuality <- reactive({
    if (progCount2()==0) return()
    # Take a dependency on input$update by reading it. (Nothing is actually done with the value.)
    input$update
    progress <- Progress$new(session)
    progress$set(message="Retrieving program data",detail="Please wait a moment...")
    #Reactivity is invalidated unless update button is pressed
    isolate(
      dataQuality <- dbGetQuery(connection,paste("
      SELECT unique PE.Program_Key,
          Agency_Name,
          Program_Name,
          count(unique PE.Program_Enrollment_Key) Enrolls,
          count(unique PE.Client_Key) Clients,
          count(unique case when Program_Entry_Date - Date_of_Birth > 18 
            then Program_Enrollment_Key end) Adults,
          count(unique case when Unaccompanied_Child = 'Yes' 
            then Program_Enrollment_Key end) UnaChild,
          count(unique case when Program_Exit_Date <= to_date('",endSelect(),"','yyyy-mm-dd') 
            then Program_Enrollment_Key end) Leavers,
          count(unique case when Name_First = 'MISSING' then Program_Enrollment_Key end) Name_F_M,
          count(unique case when Name_Last = 'MISSING' then Program_Enrollment_Key end) Name_L_M,
          count(unique case when not ID_Type = 1 then Program_Enrollment_Key end) SSN_M,
          count(unique case when not DOB_Type = 1 or Date_of_Birth is null 
            then Program_Enrollment_Key end) DOB_M,
          count(unique case when Race_Code is null then Program_Enrollment_Key end) Race_M,
          count(unique case when Ethnicity_Code is null then Program_Enrollment_Key end) Ethn_M,
          count(unique case when Gender_Code is null then Program_Enrollment_Key end) Gender_M,
          count(unique case when Veteran_Status_Code is null then Program_Enrollment_Key end) Vet_M,
          count(unique case when Disabling_Condition is null then Program_Enrollment_Key end) Disab_M,
          count(unique case when Prior_Nights_Residence_Code is null 
            then Program_Enrollment_Key end) PNR_M,
          count(unique case when Zip_Quality_Code is null then Program_Enrollment_Key end) Zip_M,
          count(unique case when Housing_Status_Code is null 
            then Program_Enrollment_Key end) HStatus_M,
          count(unique case when CCI_N.Verified_Answer is null 
            then Program_Enrollment_Key end) Inc_N_M,
          count(unique case when CCI_X.Verified_Answer is null and 
            Program_Exit_Date <= to_date('",endSelect(),"','yyyy-mm-dd') 
            then Program_Enrollment_Key end) Inc_X_M,
          count(unique case when CNB_N.Verified_Answer is null 
            then Program_Enrollment_Key end) Ben_N_M,
          count(unique case when CNB_X.Verified_Answer is null and 
            Program_Exit_Date <= to_date('",endSelect(),"','yyyy-mm-dd') 
            then Program_Enrollment_Key end) Ben_N_M,
          count(unique case when Physical_Disability is null 
            then Program_Enrollment_Key end) Disab_P_M,
          count(unique case when Developmental_Disability is null 
            then Program_Enrollment_Key end) Disab_D_M,
          count(unique case when Chronic_Health_Condition is null 
            then Program_Enrollment_Key end) Disab_C_M,
          count(unique case when HIV_AIDS is null then Program_Enrollment_Key end) Disab_H_M,
          count(unique case when Mental_Illness is null then Program_Enrollment_Key end) Disab_M_M,
          count(unique case when Substance_Abuse is null then Program_Enrollment_Key end) Disab_S_M,
          count(unique case when Dom_Vio_Survivor is null then Program_Enrollment_Key end) Disab_DV_M,
          count(unique case when Program_Exit_Date <= to_date('",endSelect(),"','yyyy-mm-dd') and 
            Destination_Code is null then Program_Enrollment_Key end) Dest_M,
          count(unique case when ID_Type in (8,9) then Program_Enrollment_Key end) SSN_DKR,
          count(unique case when DOB_Type in (8,9)then Program_Enrollment_Key end) DOB_DKR,
          count(unique case when Race_Code in (15,16) then Program_Enrollment_Key end) Race_DKR,
          count(unique case when Ethnicity_Code in (8,9) then Program_Enrollment_Key end) Ethn_DKR,
          count(unique case when Gender_Code in (8,9) then Program_Enrollment_Key end) Gender_DKR,
          count(unique case when Veteran_Status_Code in (8,9) then Program_Enrollment_Key end) Vet_DKR,
          count(unique case when Disabling_Condition in (8,9) then Program_Enrollment_Key end) Disab_DKR,
          count(unique case when Prior_Nights_Residence_Code in (8,9) 
            then Program_Enrollment_Key end) PNR_DKR,
          count(unique case when Zip_Quality_Code in (8,9) then Program_Enrollment_Key end) Zip_DKR,
          count(unique case when Housing_Status_Code in (8,9) 
            then Program_Enrollment_Key end) HStatus_DKR,
          count(unique case when CCI_N.Verified_Answer in (8,9) 
            then Program_Enrollment_Key end) Inc_N_DKR,
          count(unique case when CCI_X.Verified_Answer in (8,9) and 
            Program_Exit_Date <= to_date('",endSelect(),"','yyyy-mm-dd') 
            then Program_Enrollment_Key end) Inc_X_DKR,
          count(unique case when CNB_N.Verified_Answer in (8,9) 
            then Program_Enrollment_Key end) Ben_N_DKR,
          count(unique case when CNB_X.Verified_Answer in (8,9) and 
            Program_Exit_Date <= to_date('",endSelect(),"','yyyy-mm-dd') 
            then Program_Enrollment_Key end) Ben_X_DKR,
          count(unique case when Physical_Disability in (8,9) 
            then Program_Enrollment_Key end) Disab_P_DKR,
          count(unique case when Developmental_Disability in (8,9) 
            then Program_Enrollment_Key end) Disab_D_DKR,
          count(unique case when Chronic_Health_Condition in (8,9) 
            then Program_Enrollment_Key end) Disab_C_DKR,
          count(unique case when HIV_AIDS in (8,9) then Program_Enrollment_Key end) Disab_H_DKR,
          count(unique case when Mental_Illness in (8,9) then Program_Enrollment_Key end) Disab_M_DKR,
          count(unique case when Substance_Abuse in (8,9) then Program_Enrollment_Key end) Disab_S_DKR,
          count(unique case when Dom_Vio_Survivor in (8,9) then Program_Enrollment_Key end) Disab_DV_DKR,
          count(unique case when Program_Exit_Date <= to_date('",endSelect(),"','yyyy-mm-dd') and 
            Destination_Code in (8,9) then Program_Enrollment_Key end) Dest_DKR
        FROM Program_Enrollment PE
        JOIN Client_Information CI
          on PE.Client_Key = CI.Client_Key
        JOIN Program_Profile_Info PPI
          on PE.Program_Key = PPI.Program_Key
        FULL JOIN Program_Community_Information PCI
          on PE.Program_Key = PCI.Program_Key
        FULL JOIN Community_Group_Information CGI
          on PCI.Group_Key = CGI.Group_Key
        LEFT JOIN Client_Status_Information CSI
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
            Program_Enrollment_Key, 
            Collect_Stage, 
            sum(Physical_Disability) Physical_Disability, 
            sum(Developmental_Disability) Developmental_Disability, 
            sum(Chronic_Health_Condition) Chronic_Health_Condition, 
            sum(HIV_AIDS) HIV_AIDS, 
            sum(Mental_Illness) Mental_Illness, 
            sum(Substance_Abuse) Substance_Abuse, 
            sum(Dom_Vio_Survivor) Dom_Vio_Survivor 
          FROM Client_Special_Needs_Info 
          GROUP BY 
            Program_Enrollment_Key, 
            Collect_Stage
        ) CSNI
          on PE.Program_Enrollment_Key = CSNI.Program_Enrollment_Key
        WHERE
          (Program_Exit_Date >= to_date('",beginSelect(),"','yyyy-mm-dd') or Program_Exit_Date is null) and
          Program_Entry_Date <= to_date('",endSelect(),"','yyyy-mm-dd') and
          CSI.Collect_Stage = 1 and 
          CSNI.Collect_Stage = 1 and ",
          finalSelect_Table(),input$reportLevel,"_Key=",finalSelect_Key(),programTypesSelect(),"  
        GROUP BY PE.Program_Key, Agency_Name, Program_Name
      ",sep=""))
    )
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
    progress$close()    
    return(dataQuality)
  })
    
  
  Violations <- reactive({
    if (progCount2()==0 | input$reportLevel != "Program") return()
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
          LOS_Issue, 
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
            case when count(LOS_Issue)>0 then 'x' end LOS_Issue, 
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
              count(unique LOS_Issue) Violations
              
          FROM (
            
            SELECT unique
              PE.Program_Enrollment_Key,
              CI.Client_Key,
              case when Name_First = 'MISSING' then CI.Client_Key end FN_M,
              case when Name_Last = 'MISSING' then CI.Client_Key end LN_M,
              case when not ID_Type = 1 then CI.Client_Key end ID_M,
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
              case when Physical_Disability is null then PE.Program_Enrollment_Key end PD_M,
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
                
              case when ((Program_Type_Code = 1 and to_date('",endSelect(),"','yyyy-mm-dd') - 
                Program_Entry_Date > 180) or (Program_Type_Code = 2 and 
                to_date('",endSelect(),"','yyyy-mm-dd') - Program_Entry_Date > 720)) and 
                Program_Exit_Date is null then PE.Program_Enrollment_Key end LOS_Issue
                
            FROM Program_Enrollment PE
              
            RIGHT JOIN Program_Profile_Info PPI
              on PE.Program_Key = PPI.Program_Key
              
            JOIN Client_Information CI
              on PE.Client_Key = CI.Client_Key
              
            LEFT JOIN Client_Status_Information CSI
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
                Program_Enrollment_Key, 
                Collect_Stage, 
                sum(Physical_Disability) Physical_Disability, 
                sum(Developmental_Disability) Developmental_Disability, 
                sum(Chronic_Health_Condition) Chronic_Health_Condition, 
                sum(HIV_AIDS) HIV_AIDS, 
                sum(Mental_Illness) Mental_Illness, 
                sum(Substance_Abuse) Substance_Abuse, 
                sum(Dom_Vio_Survivor) Dom_Vio_Survivor 
              FROM Client_Special_Needs_Info
              GROUP BY Program_Enrollment_Key, Collect_Stage
            ) CSNI
              on PE.Program_Enrollment_Key = CSNI.Program_Enrollment_Key
              
            WHERE 
              (Program_Exit_Date >= to_date('",beginSelect(),"','yyyy-mm-dd') or 
                Program_Exit_Date is null) and
              Program_Entry_Date <= to_date('",endSelect(),"','yyyy-mm-dd') and
              CSI.Collect_Stage = 1 and
              CSNI.Collect_Stage = 1 and 
              PPI.Program_Key=",finalSelect_Key()," 
            
          )
            
          GROUP BY Client_Key
            
          ORDER BY Violations Desc

        ) 
          
        WHERE 
          Violations > 0
        
      ",sep="")))
    
    names(Violations) <- c("Client Key","Missing General Info","Missing Program Info",
      "Missing Income / Benefits","Missing Special Needs","DKR General Info","DKR Program Info",
      "DKR Income / Benefits","DKR Special Needs","Length of Stay Issue")
    progress$close()
    return(Violations[,1:10])
  })
  
  #################################
  # TABLES
  #################################
  
  dqReport <- reactive({
    if (progCount2()==0) return()
      dqReport <- data.frame(
      
        Data_Element=c("Total Clients","Total Adults","Total Unaccompanied Children","Total Leavers",
          "First Name","Last Name","Social Security Number","Date of Birth","Race","Ethnicity","Gender",
          "Veteran Status","Disabling Condition","Residence Prior to Program Entry",
          "Zip Code of Last Permanent Address","Housing Status (at entry)","Income (at entry)",
          "Income (at exit)","Non-Cash Benefits (at entry)","Non-Cash Benefits (at exit)",
          "Physical Disablity (at entry)","Developmental Disability (at entry)",
          "Chronic Health Condition (at entry)","HIV/AIDS (at entry)","Mental Health (at entry)",
          "Substance Abuse (at entry)","Domestic violence (at entry)","Destination (at exit)","TOTAL"),
        
        Applicable_Records=c(sum(dataQuality()[["ENROLLS"]]),sum(dataQuality()[["ADULTS"]]),
          sum(dataQuality()[["UNACHILD"]]),sum(dataQuality()[["LEAVERS"]]),rep(sum(dataQuality()[["ENROLLS"]]),7),
          sum(dataQuality()[["ADULTS"]]),rep(sum(dataQuality()[["ENROLLS"]]),5),sum(dataQuality()[["LEAVERS"]]),
          sum(dataQuality()[["ENROLLS"]]),sum(dataQuality()[["LEAVERS"]]),rep(sum(dataQuality()[["ENROLLS"]]),7),
          sum(dataQuality()[["LEAVERS"]]),sum(dataQuality()[["APP_REC"]])),
        
        Missing=c(NA,NA,NA,NA,as.numeric(colSums(dataQuality()[grep("^NAME_F_M$",colnames(dataQuality())):
          grep("^DEST_M$",colnames(dataQuality()))])),sum(dataQuality()[["SUM_MISS"]])),
          
        DKR=c(NA,NA,NA,NA,0,0,as.numeric(colSums(dataQuality()[grep("^SSN_DKR$",colnames(dataQuality())):
          grep("^DEST_DKR$",colnames(dataQuality()))])),sum(dataQuality()[["SUM_DKR"]]))
        
      )
    
      names(dqReport) <- c("Data Element","Applicable Records","Missing","DKR")
    
      dqReport[,"Missing + DKR (%)"] <- ifelse(is.na(dqReport[,"Missing"]) | is.na(dqReport[,"DKR"]),"",
        ifelse(dqReport[,"Applicable Records"]==0 & !is.na(dqReport[,"Missing"]) & !is.na(dqReport[,"DKR"]),"0%",
        round((dqReport[,"Missing"]+dqReport[,"DKR"])/dqReport[,"Applicable Records"]*100,1)))
    
      return(dqReport)
  })
  
  dqReport_short <- reactive({
    if (progCount2()==0) return()   
      dqReport_short <- data.frame(
        Category = c("Universal","Program-Specific","TOTAL"),
        App_Rec = c(sum(dqReport()[5:12,2]),sum(dqReport()[13:29,2]),dqReport()[29,2]),
        M = c(sum(dqReport()[5:12,3]),sum(dqReport()[13:28,3]),dqReport()[29,3]),
        DKR = c(sum(dqReport()[5:12,4]),sum(dqReport()[13:28,4]),dqReport()[29,4]),
        MDKR = round(c(sum(dqReport()[5:12,3:4])/sum(dqReport()[5:12,2]),
          sum(dqReport()[13:28,3:4])/sum(dqReport()[13:28,2]),
          sum(dqReport()[29,3:4])/dqReport()[29,2])*100,1)
      )
      names(dqReport_short) <- c("Data Element Category", "Applicable Records","Missing","DKR", "Missing + DKR (%)")
      return(dqReport_short)
  })
  
  
  output$dqTable_short <- renderDataTable({
    dqReport_short()
  },options=list(fnRowCallback = I('
      function(nRow, aData, iDisplayIndex, iDisplayIndexFull) {
        // Column alignment
        $("td:eq(1)", nRow).css("text-align", "right");
        $("td:eq(2)", nRow).css("text-align", "right");
        $("td:eq(3)", nRow).css("text-align", "right");
        $("td:eq(4)", nRow).css("text-align", "right");
        // Make column 4 values bold
        $("td:eq(4)", nRow).css("font-weight", "bold");
        // Set conditional font colors for values in column 2
        if (parseFloat(aData[2]) > 0) {
          $("td:eq(2)", nRow).css("color", "red");
          $("td:eq(2)", nRow).css("font-weight", "bold");
        };
        // Set conditional font colors for values in column 4
        $("td:eq(4)", nRow).css("color", "green");
        if (parseFloat(aData[4]) >= 2.5) {
          $("td:eq(4)", nRow).css("color", "orange");
        };
        if (parseFloat(aData[4]) >= 5) {
          $("td:eq(4)", nRow).css("color", "red");
        };
      }
    '),bAutoWidth=FALSE,bFilter=0,bPaginate=0,bLengthChange=0,bSort=0,bInfo=0,iDisplayLength=29,
    aoColumns=list(list(bSearchable=FALSE),list(bSearchable=FALSE),list(bSearchable=FALSE),
    list(bSearchable=FALSE),list(bSearchable=FALSE))))

  output$dqTable <- renderDataTable({
    dqReport()
  },options=list(
    fnRowCallback = I('
      function(nRow, aData, iDisplayIndex, iDisplayIndexFull) {
        // Column alignment
        $("td:eq(1)", nRow).css("text-align", "right");
        $("td:eq(2)", nRow).css("text-align", "right");
        $("td:eq(3)", nRow).css("text-align", "right");
        $("td:eq(4)", nRow).css("text-align", "right");
        // Make column 4 values bold
        $("td:eq(4)", nRow).css("font-weight", "bold");
        // Set conditional font colors for values in column 2
        if (parseFloat(aData[2]) > 0) {
          $("td:eq(2)", nRow).css("color", "red");
          $("td:eq(2)", nRow).css("font-weight", "bold");
        };
        // Set conditional font colors for values in column 4
        $("td:eq(4)", nRow).css("color", "green");
        if (parseFloat(aData[4]) >= 2.5) {
          $("td:eq(4)", nRow).css("color", "orange");
        };
        if (parseFloat(aData[4]) >= 5) {
          $("td:eq(4)", nRow).css("color", "red");
        };
      }
    '),bAutoWidth=FALSE,bFilter=0,bPaginate=0,bLengthChange=0,bSort=0,bInfo=0,iDisplayLength=29,
    aoColumns=list(list(bSearchable=FALSE),list(bSearchable=FALSE),list(bSearchable=FALSE),
    list(bSearchable=FALSE),list(bSearchable=FALSE))))
  
  progsTable <- reactive({
    if (progCount2()==0) return()
      dqReportProgs <- dataQuality()[,
        c("PROGRAM_KEY","AGENCY_NAME","PROGRAM_NAME","CLIENTS","ENROLLS","APP_REC","PCT_MISSDKR")]
      dqReportProgs <- dqReportProgs[order(-dqReportProgs[,7]),]
      dqReportProgs[,7] <- round(dqReportProgs[,7]*100,1)
      names(dqReportProgs) <- c("Program Key","Agency Name","Program Name","Clients","Enrollments","Applicable Records","Missing + DKR (%)")
      return(dqReportProgs)
  })
  
  output$progsTable <- renderDataTable({
    progsTable()
  },options=list(
    fnRowCallback = I('
      function(nRow, aData, iDisplayIndex, iDisplayIndexFull) {
        // Column alignment
        $("td:eq(3)", nRow).css("text-align", "right");
        $("td:eq(4)", nRow).css("text-align", "right");
        $("td:eq(5)", nRow).css("text-align", "right");
        $("td:eq(6)", nRow).css("text-align", "right");
        // Make column 6 values bold
        $("td:eq(6)", nRow).css("font-weight", "bold");
        // Set conditional font colors for values in column 6
        $("td:eq(6)", nRow).css("color", "green");
        if (parseFloat(aData[6]) >= 2.5) {
          $("td:eq(6)", nRow).css("color", "orange");
        };
        if (parseFloat(aData[6]) >= 5) {
          $("td:eq(6)", nRow).css("color", "red");
        };
      }
    '),bAutoWidth=FALSE,bPaginate=0,bLengthChange=0,bInfo=0,iDisplayLength=500,
      aoColumns=list(list(bSearchable=FALSE),list(bSearchable=FALSE),list(bSearchable=FALSE),
      list(bSearchable=FALSE),list(bSearchable=FALSE),list(bSearchable=FALSE),list(bSearchable=FALSE))
  ))
  
  output$Violations <- renderDataTable({
    Violations()
  },options=list(
      fnRowCallback = I('
      function(nRow, aData, iDisplayIndex, iDisplayIndexFull) {
        // Column alignment
        $("td:eq(1)", nRow).css("text-align", "center");
        $("td:eq(2)", nRow).css("text-align", "center");
        $("td:eq(3)", nRow).css("text-align", "center");
        $("td:eq(4)", nRow).css("text-align", "center");
        $("td:eq(5)", nRow).css("text-align", "center");
        $("td:eq(6)", nRow).css("text-align", "center");
        $("td:eq(7)", nRow).css("text-align", "center");
        $("td:eq(8)", nRow).css("text-align", "center");
        $("td:eq(9)", nRow).css("text-align", "center");
      }
    '),bAutoWidth=FALSE,bFilter=0,bPaginate=0,bLengthChange=0,bInfo=0,iDisplayLength=500,
      aoColumns=list(list(bSearchable=FALSE),list(bSearchable=FALSE),list(bSearchable=FALSE),
      list(bSearchable=FALSE),list(bSearchable=FALSE),list(bSearchable=FALSE),list(bSearchable=FALSE),
      list(bSearchable=FALSE),list(bSearchable=FALSE),list(bSearchable=FALSE))
  ))
  

  #################################
  # PLOT
  #################################
    
  progsPlot <- reactive({
    if (progCount2()==0 | input$reportLevel=="Program") return()
    # Take a dependency on input$update by reading it. (Nothing is actually done with the value.)
    input$update
    progress <- Progress$new(session)
    progress$set(message="Creating chart",detail="Please wait a moment...")
      #Reactivity is invalidated unless update button is pressed
      isolate(
        print(
          ggplot(dataQuality(),aes(x=factor(PROGRAM_KEY),y=PCT_MISSDKR*100,group=1)) + 
            geom_point(size=4,stat="identity",col=dataQuality()[,"PLOT_COL"],lwd=3) + 
            theme(axis.text.x=element_text(angle=90,vjust=.5)) + 
            geom_abline(intercept=5, slope=0,col="red") + 
            geom_abline(intercept=2.5, slope=0,col="darkorange") +
            ggtitle(paste("Overall Data Quality for Programs in ", finalSelect_Text(),"\nReport Period: ",
              substr(beginSelect(),6,7),"/",substr(beginSelect(),9,10),"/",substr(beginSelect(),1,4),
              " - ",substr(endSelect(),6,7),"/",substr(endSelect(),9,10),"/",
              substr(endSelect(),1,4),sep="")) + 
            xlab("Program Key") + 
            ylab("Missing, Don't know, and Refused Responses (%)")
        )
      )
    progress$close()
  })
  
  
  elementsPlot <- reactive({
    progress <- Progress$new(session)
    progress$set(message="Creating chart",detail="Please wait a moment...")
    Missing <- c(sum(dqReport()[5:12,"Missing"]),sum(dqReport()[13:16,"Missing"]),
      sum(dqReport()[17:20,"Missing"]),sum(dqReport()[21:28,"Missing"]))/
      c(sum(dqReport()[5:12,"Applicable Records"]),sum(dqReport()[13:16,"Applicable Records"]),
      sum(dqReport()[17:20,"Applicable Records"]),sum(dqReport()[21:28,"Applicable Records"]))
      
    DKR <- c(sum(dqReport()[5:12,"DKR"]),sum(dqReport()[13:16,"DKR"]),
      sum(dqReport()[17:20,"DKR"]),sum(dqReport()[21:28,"DKR"]))/
      c(sum(dqReport()[5:12,"Applicable Records"]),sum(dqReport()[13:16,"Applicable Records"]),
      sum(dqReport()[17:20,"Applicable Records"]),sum(dqReport()[21:28,"Applicable Records"]))
  
    x <- data.frame(
      Element = seq(1:6),
      label = c("","","General","Program","Income/Benefits","Special Needs"),
      Missing = c(0,0,Missing),
      DKR = c(0,0,DKR),
      OK = c(0,0,1-Missing-DKR)
    )

    graphData <- melt(x, id.vars=1:2)

    print(elementsPlot <- ggplot(graphData, aes(x=Element, y=value,fill=variable)) + 
      geom_bar(width=.9,stat="identity") +
      scale_fill_manual(values=c("firebrick2","yellow","green2"),name="Status of Client Data") + 
      coord_polar(theta="y") +
      xlab("") + 
      ylab("") +
      geom_text(data=graphData, hjust=1.02, aes(x=Element, y=0,label=label)) +
      ggtitle("Data Health") +
      theme_minimal() +
      theme(
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_blank(),
        axis.text.y = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks = element_blank(),
        plot.title = element_text(size = 25)
      )
    )
    progress$close()
  })
  
  
  output$plot <- renderPlot({
    # Take a dependency on input$update by reading it. (Nothing is actually done with the value.)
    input$update
    #Reactivity is invalidated unless update button is pressed
    isolate(ifelse(input$reportLevel=="Program",return(elementsPlot()),return(progsPlot())))
  })
  
  
  #################################
  # DOWNLOADS
  #################################
  
  output$downloadSummary <- downloadHandler(
    filename = function() {
      paste('summary-', Sys.Date(), '.csv', sep='')
    },
    content = function(file) {
      write.csv(dqReport_short(), file,na="",row.names=FALSE)
    }
  )
  
  output$downloadDQ <- downloadHandler(
    filename = function() {
      paste('elements-', Sys.Date(), '.csv', sep='')
    },
    content = function(file) {
      write.csv(dqReport(), file,na="",row.names=FALSE)
    }
  )
  
  output$downloadClients <- downloadHandler(
    filename = function() {
      paste('clients-', Sys.Date(), '.csv', sep='')
    },
    content = function(file) {
      write.csv(Violations(), file,na="",row.names=FALSE)
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
