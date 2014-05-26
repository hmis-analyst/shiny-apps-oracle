# .libPaths("~/R/win-library/3.1")
# install.packages("RJDBC",repos="http://cran.rstudio.com/")
# install.packages("ggplot2",repos="http://cran.rstudio.com/")
# install.packages("reshape2",repos="http://cran.rstudio.com/")
# install.packages("stringr",repos="http://cran.rstudio.com/")
#-------------------
# Load shiny, RODBC, and ggplot2 packages
library(shinyIncubator)
library(RJDBC)
library(ggplot2)
library(reshape2)
library(stringr)
library(plyr)
#-------------------
libPath1 <- "~/HMIS Data Analyst/lib/"
libPath2 <- "../../lib/"

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
  # Determine the program types associated with the user's group/agency/program selection
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
  programTypes_Reduced2 <- reactive({
    programTypes_Reduced()[which(programTypes_Reduced() %in% c("ES","TH"))]
  })
  output$programTypes <- renderUI({
    if(progCount()==0) return(HTML(paste("<p>Program types</p><p><em>This",input$reportLevel,"has no programs")))
    checkboxGroupInput("programTypes","Program types",as.list(programTypes_Reduced2()),
      selected=programTypes_Reduced2())
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
  recWin <- reactive({
    Sys.Date()-2-input$daterange[2]
  })
  #-------------------
  #################################
  # RECIDIVISM ALGORITHM
  #################################
  #-------------------
  # STEP 1: QUERYING THE INITIAL SAMPLE
  # Retrieve all clients placed in housing during the report period
  # I.e., those whose destinations make them "formerly homeless"
  Placements <- reactive({
    input$update
    progress <- Progress$new(session)
    progress$set(message="Retrieving program enrollments",detail="Please wait a moment...")
    isolate(
      Placements <- dbGetQuery(connection,paste("
        /* Query the basic information needed for each enrollment */
        SELECT unique
          1 counter,
          Program_Enrollment_Key,
          Client_Key, 
          Household_Key,
          Program_Key,
          Agency_Name,
          Program_Name,
          Program_Type,
          Program_Exit_Date,
          Destination_Code,
          DC.Description Destination,
          case when Destination_Code in (120, 119, 117, 118, 116, 113, 23, 22, 21, 20, 19, 11, 10, 3) then 1 else 0 end Dest_Permanent,
          case when Destination_Code in (4, 5, 6, 7, 15, 106, 107, 109) then 1 else 0 end Dest_Institutional,
          case when Destination_Code in (807, 806, 122, 14, 13, 12) then 1 else 0 end Dest_Temporary,
          case when Destination_Code is null or Destination_Code in (8,9,17) then 1 else 0 end Dest_Unknown
        FROM Program_Enrollment PE
        JOIN Program_Community_Information PCI
          on PE.Program_Key = PCI.Program_Key
        JOIN Community_Group_Information CGI
          on PCI.Group_Key = CGI.Group_Key 
        /* Join program information */
        JOIN Program_Profile_Info PPI
          on PE.Program_Key = PPI.Program_Key
        LEFT JOIN Destination_Codes DC
          on PE.Destination_Code = DC.Code_Key
        WHERE 
          /* Only include enrollments that have a program exit date within the report period */
          Program_Exit_Date >= to_date('",beginSelect(),"','yyyy-mm-dd') and
          Program_Entry_Date <= to_date('",endSelect(),"','yyyy-mm-dd') and
          /* Only include enrollments where the client exited to a permanent or temporary housing destination */
          Destination_Code in (3, 10, 11, 12, 13, 14, 19, 20, 21, 22, 23, 113, 116, 117, 118, 119, 120, 122, 806, 807) and
          /* Only include enrollments of homeless programs */
          Program_Type_Code in (1,2) and ",
          finalSelect_Table(),input$reportLevel,"_Key=",finalSelect_Key(),programTypesSelect()
      ,sep=""))
    )
    progress$close()
    return(Placements)
  })
  #-------------------
  # STEP 2: TESTING FOR CLIENT RECIDIVISM 
  Placements2 <- reactive({
    input$update
    progress <- Progress$new(session)
    Placements2 <- Placements()
    # Get number of observations in Placements, store in "n"
    n <- length(Placements2[,1])
    if(n==0 | is.na(Placements2[1,"CLIENT_KEY"])) return(Placements2)
    isolate(
      # Loop through every enrollment in Placements and determine, client by client, whether each came back into HMIS as homeless.
      # The result of this procedure is a new binary variable for recidivism (1 = did return, 0 = did not return).
      for (i in 1:n) {
        # Retrieve the observation's client key, store in "Client"
        Client = Placements2[i,"CLIENT_KEY"]
        # Retrieve the date of enrollment termination, store in "Date"
        Date = substr(Placements2[i,"PROGRAM_EXIT_DATE"],0,10)
        # Queries the next homeless program entry date for tshe current client (if such an entry exists)
        Temp <- dbGetQuery(connection, paste("
          SELECT 
            /* Selects the soonest program entry date in the record set */
            min(Program_Entry_Date) Next_Entry, 
            P.Client_Key
            FROM Program_Enrollment P
          /* Join program information */
          JOIN Program_Profile_Info PPI
            on P.Program_Key = PPI.Program_Key
          WHERE 
            /* Only include enrollments that begin later than the original observation's program exit date */
            Program_Entry_Date > to_date('",Date,"','yyyy-mm-dd') and
            /* Only include enrollments that are associated with an ES or TH program */
            Program_Type in ('ES','TH') and
            /* Only consider enrollments that share the same client key as the current observation */
            P.Client_Key = ",Client,"
            /* Grouping may seem redundant, but it is necessary to make an aggregate function work, such as min() */
          GROUP BY P.Client_Key
        ",sep=""))
        # Create new Placements column. If Temp query is empty, set to NA for current observation; else, set to Next_Entry
  	    Placements2[i,"Next_Entry"] <- if(length(Temp[,"NEXT_ENTRY"])==0) {NA} else {Temp[,"NEXT_ENTRY"]}
        # Create new Placements column to identify a recidivistic enrollment. It is set to 0 if Placements[i,"Next_Entry"] is null; else, it is set to 1.
        Placements2[i,"Recidivism"] <- if(is.na(Placements2[i,"Next_Entry"])) {0} else {1}
        progress$set(message=paste("Testing clients for recidivism..."))
        progress$set(detail = paste(floor(i/n*100),"% complete",sep=""), value=i/n)
      }
    )
    remove(Temp)
    # Enrollment's termination date is converted to valid R date format
    Placements2[,"PROGRAM_EXIT_DATE"] <- as.Date(Placements2[,"PROGRAM_EXIT_DATE"])
	  # Client's next program entry is converted to valid R date format. I'm not sure why it forces me to do it a different way.
	  Placements2[,"Next_Entry"] <- as.Date(as.POSIXct(Placements2[,"Next_Entry"], origin = "1970-01-01"))
	  # Calculates how many days it took for the client to return to homelessness after the enrollment's termination date
	  Placements2[,"Days_Until_Return"] <- as.numeric(Placements2[,"Next_Entry"] - Placements2[,"PROGRAM_EXIT_DATE"])
    Placements2[,"Recidivism_Type"] <- 10
    isolate(Placements2[which(Placements2$Days_Until_Return>recWin()),"Recidivism"] <- 0)
    if(length(Placements2[which(Placements2$Recidivism==1 & Placements2$Days_Until_Return<90),1])!=0) {
      Placements2[which(Placements2$Recidivism==1 & Placements2$Days_Until_Return<90),"Recidivism_Type"] <- 1
    }
    if(length(Placements2[which(Placements2$Recidivism==1 & Placements2$Days_Until_Return>=90 & Placements2[,"Days_Until_Return"]<180),1])!=0) {
      Placements2[which(Placements2$Recidivism==1 & Placements2$Days_Until_Return>=90 & Placements2[,"Days_Until_Return"]<180),"Recidivism_Type"] <- 2
    }
    if(length(Placements2[which(Placements2$Recidivism==1 & Placements2$Days_Until_Return>=180 & Placements2[,"Days_Until_Return"]<270),1])!=0) {
      Placements2[which(Placements2$Recidivism==1 & Placements2$Days_Until_Return>=180 & Placements2[,"Days_Until_Return"]<270),"Recidivism_Type"] <- 3
    }
    if(length(Placements2[which(Placements2$Recidivism==1 & Placements2$Days_Until_Return>=270 & Placements2[,"Days_Until_Return"]<365),1])!=0) {
      Placements2[which(Placements2$Recidivism==1 & Placements2$Days_Until_Return>=270 & Placements2[,"Days_Until_Return"]<365),"Recidivism_Type"] <- 4
    }
    if(length(Placements2[which(Placements2$Recidivism==1 & Placements2$Days_Until_Return>=365 & Placements2[,"Days_Until_Return"]<455),1])!=0) {
      Placements2[which(Placements2$Recidivism==1 & Placements2$Days_Until_Return>=365 & Placements2[,"Days_Until_Return"]<455),"Recidivism_Type"] <- 5
    }
    if(length(Placements2[which(Placements2$Recidivism==1 & Placements2$Days_Until_Return>=455 & Placements2[,"Days_Until_Return"]<545),1])!=0) {
      Placements2[which(Placements2$Recidivism==1 & Placements2$Days_Until_Return>=455 & Placements2[,"Days_Until_Return"]<545),"Recidivism_Type"] <- 6
    }
    if(length(Placements2[which(Placements2$Recidivism==1 & Placements2$Days_Until_Return>=545 & Placements2[,"Days_Until_Return"]<635),1])!=0) {
      Placements2[which(Placements2$Recidivism==1 & Placements2$Days_Until_Return>=545 & Placements2[,"Days_Until_Return"]<635),"Recidivism_Type"] <- 7
    }
    if(length(Placements2[which(Placements2$Recidivism==1 & Placements2$Days_Until_Return>=635 & Placements2[,"Days_Until_Return"]<730),1])!=0) {
      Placements2[which(Placements2$Recidivism==1 & Placements2$Days_Until_Return>=635 & Placements2[,"Days_Until_Return"]<730),"Recidivism_Type"] <- 8
    }
    if(length(Placements2[which(Placements2$Recidivism==1 & Placements2$Days_Until_Return>=730),1])!=0) {
      Placements2[which(Placements2$Recidivism==1 & Placements2$Days_Until_Return>=730),"Recidivism_Type"] <- 9
    }
    progress$close()
    return(Placements2)
  })
  Placements3 <- reactive({
    Placements2()[which(Placements2()[,"Recidivism"]==0 | (Placements2()[,"DEST_PERMANENT"]==1 & Placements2()[,"Days_Until_Return"]>=30) | 
        ((Placements2()[,"DEST_TEMPORARY"]==1 | Placements2()[,"DEST_INSTITUTIONAL"]==1) & Placements2()[,"Days_Until_Return"]>=90)),]
  })
  output$recRate <- renderText({paste("Recidivism Rate: ",round(mean(Placements3()[,"Recidivism"])*100,1),"%",sep="")})
  output$exitNum <- renderText({paste("A total of",length(Placements3()[,1]),"exits from homelessness were assessed.",
    if(length(Placements2()[,1])-length(Placements3()[,1])!=0) {paste("This EXCLUDES",length(Placements2()[,1])-length(Placements3()[,1]),"false exits.")})})
  Recidivism <- reactive({sum(Placements3()[,"Recidivism"])})
  #-------------------
  #################################
  # TABLES
  #################################
  #-------------------
  enrollTable <- reactive({
    enrollTable <- Placements3()[,!(names(Placements3()) %in% c("COUNTER","DESTINATION_CODE","DEST_TEMPORARY","DEST_INSTITUTIONAL","DEST_PERMANENT","DEST_UNKNOWN"))]
  }
  )
  #-------------------
  output$barData <- renderDataTable({
    if(Recidivism()==0) return()
    barData <- barData()[,c("label","num","rate")]
    barData[which(is.na(barData$num)),c("num","rate")] <- 0
    totalRow <- data.frame(label="TOTAL",num=sum(barData$num),rate=sum(barData$rate))
    barData <- rbind(barData,totalRow)
    names(barData) <- c("Months Until Recidivism","Instances of Recidivism","% of Exits")
    return(barData)
  },options=list(
      fnRowCallback = I('
        function(nRow, aData, iDisplayIndex, iDisplayIndexFull) {
          // Column alignment
          $("td:eq(0)", nRow).css("text-align", "left");
          $("td:eq(1)", nRow).css("text-align", "right");
          $("td:eq(2)", nRow).css("text-align", "right");
        }
      '),
      bAutoWidth=FALSE,bPaginate=0,bLengthChange=0,bInfo=0,bSort=0,bFilter=0,
      aoColumns=list(list(bSearchable=FALSE),list(bSearchable=FALSE),list(bSearchable=FALSE))
    )
  )
  #-------------------
  progsTable <- reactive({
    progsTable <- 
      ddply(
        Placements3(),
        c("PROGRAM_KEY","AGENCY_NAME","PROGRAM_NAME","PROGRAM_TYPE"),
        summarize,
        numTot=length(Recidivism),
        numRec=sum(Recidivism),
        rate=round(mean(Recidivism)*100,1)
      )
    names(progsTable) <- c("PROGRAM KEY","AGENCY NAME","PROGRAM NAME","PROGRAM TYPE","EXITS FROM HOMELESSNESS","INSTANCES OF RECIDIVISM","RECIDIVISM %")
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
      aoColumns=list(list(bSearchable=FALSE),list(bSearchable=FALSE),list(bSearchable=FALSE),list(bSearchable=TRUE),list(bSearchable=FALSE),list(bSearchable=FALSE),list(bSearchable=FALSE))
    )
  )  
  
  
  #################################
  # PLOT
  #################################
  
  barData <- reactive({
    frame <- data.frame(
      label = c("1-3", "3-6", "6-9", "9-12", "12-15","15-18","18-21","21-24",">= 24", "None"),
      Recidivism_Type = seq(1:10),
      Recidivism_Type_Txt = c("A","B","C","D","E","F","G","H","I","J"),
      col = c("#67000D","#A50F15","#CB181D","#EF3B2C","#FB6A4A","#FC9272","#FCBBA1","#FEE0D2","#FFF5F0","#BAE4B3")
    )
    Placements_Summary <- ddply(Placements3()[which(Placements3()[,"Recidivism_Type"]!=10),],"Recidivism_Type",summarize,num=length(Recidivism))
    barData <- merge(frame[1:max(Placements_Summary$Recidivism_Type),],Placements_Summary,by="Recidivism_Type",all.x=TRUE)
    barData <- barData[order(barData[,"Recidivism_Type"]),]
    barData[,"rate"] <- round(barData[,"num"]/length(Placements3()[,1])*100,1)
    return(barData)
  })
  
  output$barChart <- renderPlot({
    if(Recidivism()==0) return()
    print(barChart <- ggplot(data=barData(),aes(x=Recidivism_Type_Txt,y=rate,fill=Recidivism_Type_Txt)) + 
      geom_bar(stat="identity") +
      scale_fill_manual(
        values=as.character(barData()[,"col"])
      ) +
      theme(legend.position="none", axis.text.x = element_text(angle = 45, hjust = 1,size=18),axis.text.y = element_text(size=18),plot.title = element_text(size =18)) +
      scale_x_discrete(breaks=barData()[,"Recidivism_Type_Txt"], labels=barData()[,"label"]) +
      ggtitle("The Swiftness of Recidivism") +
      ylab("% of exits") +
      xlab("Months until recidivism")
  )})
  
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
        if(input$reportLevel!="Program") {div(
          h4("Main Panel",align="center"),
          tabsetPanel(
            tabPanel("Summary",
              div(downloadButton("downloadEnrolls","Download Enrollments"),align="right"),
              h4(textOutput("recRate")),
              HTML(
                paste("<p>Based on your <i>end date</i> selection, the 
                  <a href=https://github.com/hmis-analyst/shiny-apps-oracle/wiki/Recidivism-(for-ES-and-TH-programs)#terms-to-remember target='_blank'>recidivism window</a> 
                  was<strong>",recWin(),"days</strong></p>"
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
        )}
        else{div(
          h4("Main Panel",align="center"),
          tabsetPanel(
            tabPanel("Summary",
              div(downloadButton("downloadEnrolls","Download Enrollments"),align="right"),
              h4(textOutput("recRate")),
              HTML(
                paste("<p>Based on your <i>end date</i> selection, the 
                  <a href=https://github.com/hmis-analyst/shiny-apps-oracle/wiki/Recidivism-(for-ES-and-TH-programs)#terms-to-remember target='_blank'>recidivism window</a> 
                  was<strong>",recWin(),"days</strong></p>"
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
