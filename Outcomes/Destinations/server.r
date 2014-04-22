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

# Establish JDBC connection using RJDBC
drv <- JDBC("oracle.jdbc.OracleDriver",classPath="../../lib/ojdbc6.jar", " ")
source("~/HMIS Data Analyst/lib/connectionkey.r",local=TRUE)

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
  source("../../lib/Data Options.server.r", local=TRUE)

  
  #################################
  # MAIN QUERIES
  #################################
  
  qResults_Enrolls <- reactive({
    if(progCount2()==0) return()
    # Take a dependency on input$update by reading it. (Nothing is actually done with the value.)
    input$update
    progress <- Progress$new(session)
    progress$set(message="Retrieving data",detail="Please wait a moment...")
    #Reactivity is invalidated unless update button is pressed
    isolate(
      # Query gender breakdown  based on user selections    
      qResults_Enrolls <- dbGetQuery(connection,paste("
        SELECT unique
          PE.Program_Enrollment_Key,
          PPI.Program_Key,
          Agency_Name,
          Program_Name,
          DC.Description Destination,
          case when Destination_Code is null or Destination_Code in (8,9,17) then 'Unknown'
               when Destination_Code in (110, 16, 2, 1, 111, 121) then 'Homeless'
               when Destination_Code in (4, 5, 6, 7, 15, 106, 107, 109) then 'Institutional'
               when Destination_Code in (807, 806, 122, 14, 13, 12) then 'Temporary housing'
               when Destination_Code in (18, 120, 119, 117, 118, 116, 113, 23, 22, 21, 20, 19, 11, 10, 3) then 'Permanent housing'
            end Destination_Category
        FROM Program_Enrollment PE
        JOIN Program_Community_Information PCI
          on PE.Program_Key = PCI.Program_Key
        JOIN Community_Group_Information CGI
          on PCI.Group_Key = CGI.Group_Key 
        JOIN Program_Profile_Info PPI
          on PE.Program_Key = PPI.Program_Key
        LEFT JOIN Destination_Codes DC
          on PE.Destination_Code = DC.Code_Key
        WHERE
          (Program_Exit_Date >= to_date('",beginSelect(),"','yyyy-mm-dd') or Program_Exit_Date is null) and
          Program_Entry_Date <= to_date('",endSelect(),"','yyyy-mm-dd') and ",
          finalSelect_Table(),input$reportLevel,"_Key=",finalSelect_Key(),programTypesSelect()       
      ,sep=""))
    )
    names(qResults_Enrolls) <- c("PROGRAM ENROLLMENT KEY","PROGRAM KEY","AGENCY NAME","PROGRAM NAME","DESTINATION","DESTINATION CATEGORY")
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
          count(unique Program_Enrollment_Key) Leavers,
          count(unique case when Destination_Code is null or Destination_Code in (8,9,17) then Program_Enrollment_Key end) Unknown,
          count(unique case when Destination_Code in (110, 16, 2, 1, 111, 121) then Program_Enrollment_Key end) Homeless,
          count(unique case when Destination_Code in (4, 5, 6, 7, 15, 106, 107, 109) then Program_Enrollment_Key end) Institutional,
          count(unique case when Destination_Code in (807, 806, 122, 14, 13, 12) then Program_Enrollment_Key end) Temporary, 
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
        finalSelect_Table(),input$reportLevel,"_Key=",finalSelect_Key(),programTypesSelect() 
      ,sep=""))
    )
    progress$close()
    if(input$mdkr==FALSE) {
      return(qResults_short)
    } 
    else {
      qResults_short$LEAVERS <- qResults_short$LEAVERS - qResults_short$UNKNOWN 
      qResults_short <- qResults_short[,!(names(qResults_short) %in% "UNKNOWN")]
      return(qResults_short)
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
          count(unique case when Destination_Code is not null or Destination_Code in (8,9,17) then Program_Enrollment_Key end) Unknown,
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
  
  
  #################################
  # TABLES
  #################################
  
  output$summaryTable <- renderDataTable({
    qResults_short()
  },options=list(
      fnRowCallback = I('
        function(nRow, aData, iDisplayIndex, iDisplayIndexFull) {
          // Column alignment
          $("td:eq(1)", nRow).css("text-align", "right");
          $("td:eq(2)", nRow).css("text-align", "right");
          $("td:eq(3)", nRow).css("text-align", "right");
          $("td:eq(4)", nRow).css("text-align", "right");
          $("td:eq(5)", nRow).css("text-align", "right");
        }
      '),
      bAutoWidth=FALSE,bFilter=0,bPaginate=0,bLengthChange=0,bSort=0,bInfo=0,iDisplayLength=29,
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
    graphdata <- data.frame(
      Destination = factor(c("Homeless","Institutional","Temporary","Permanent")),
      Value = c(qResults_short()$HOMELESS,qResults_short()$INSTITUTIONAL,
        qResults_short()$TEMPORARY,qResults_short()$PERMANENT),
      color = c("firebrick2","darkorchid","royalblue2","green4")
    )
    if(input$mdkr==FALSE) {
      UNKNOWN <- data.frame( 
        Destination = "Unknown",
        Value = qResults_short()$UNKNOWN,
        color = "gray45"
      )
      graphdata <- rbind(UNKNOWN,graphdata)
    }
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
              dataTableOutput("summaryTable")
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
