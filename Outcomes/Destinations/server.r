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
        WHERE Agency_Name='",str_replace_all(input$agencySelect,"'","''"),"'
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
    isolate(progress$set(message="Retrieving data",detail="Please wait a moment..."))
    #Reactivity is invalidated unless update button is pressed
    isolate(
      # Query gender breakdown  based on user selections    
      qResults_short <- dbGetQuery(connection,paste("
        SELECT 
          count(PE.Program_Enrollment_Key) Leavers,
          count(case when Destination_Code not in (110, 16, 2, 1, 111, 121,4, 5, 6, 7, 15, 106, 107, 
            109, 807, 806, 122, 14, 13, 12,18, 120, 119, 117, 118, 116, 113, 23, 22, 21, 20, 19, 11, 
            10, 3) then 1 end) Unknown,
          count(case when Destination_Code in (110, 16, 2, 1, 111, 121) then 1 end) Homeless,
          count(case when Destination_Code in (4, 5, 6, 7, 15, 106, 107, 109) then 1 end) Institutional,
          count(case when Destination_Code in (807, 806, 122, 14, 13, 12) then 1 end) Temporary, 
          count(case when Destination_Code in (18, 120, 119, 117, 118, 116, 113, 23, 22, 21, 20, 19, 
            11, 10, 3) then 1 end) Permanent
        FROM Program_Enrollment PE
        JOIN Program_Community_Information PCI
          on PE.Program_Key = PCI.Program_Key
        JOIN Community_Group_Information CGI
          on PCI.Group_Key = CGI.Group_Key
        JOIN Program_Profile_Info PPI
          on PE.Program_Key = PPI.Program_Key
        WHERE
        (Program_Exit_Date >= to_date('",beginSelect(),"','yyyy-mm-dd') or Program_Exit_Date is null) and
        Program_Entry_Date <= to_date('",endSelect(),"','yyyy-mm-dd') and ",
        finalSelect_Table(),input$reportLevel,"_Key=",finalSelect_Key(),programTypesSelect() 
      ,sep=""))
    )
    isolate(progress$close())
    if(input$mdkr==FALSE) {
      return(qResults_short)
    } 
    else {
      qResults_short$LEAVERS <- qResults_short$LEAVERS - qResults_short$UNKNOWN 
      qResults_short <- qResults_short[,!(names(qResults_short) %in% "UNKNOWN")]
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
          count(PE.Program_Enrollment_Key) Leavers,
          count(case when Destination_Code not in (110, 16, 2, 1, 111, 121,4, 5, 6, 7, 15, 106, 107, 
            109, 807, 806, 122, 14, 13, 12,18, 120, 119, 117, 118, 116, 113, 23, 22, 21, 20, 19, 11, 
            10, 3) then 1 end) Unknown,
          count(case when Destination_Code in (18, 120, 119, 117, 118, 116, 113, 23, 22, 21, 20, 19, 
            11, 10, 3) then 1 end) Permanent
          
        FROM Program_Enrollment PE
          
        JOIN Program_Community_Information PCI
          on PE.Program_Key = PCI.Program_Key
          
        JOIN Community_Group_Information CGI
          on PCI.Group_Key = CGI.Group_Key
          
        JOIN Program_Profile_Info PPI
          on PE.Program_Key = PPI.Program_Key
          
        WHERE
        (Program_Exit_Date >= to_date('",beginSelect(),"','yyyy-mm-dd') or Program_Exit_Date is null) and
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
  },options=list(fnRowCallback = I('
      function(nRow, aData, iDisplayIndex, iDisplayIndexFull) {
        // Column alignment
        $("td:eq(1)", nRow).css("text-align", "right");
        $("td:eq(2)", nRow).css("text-align", "right");
        $("td:eq(3)", nRow).css("text-align", "right");
        $("td:eq(4)", nRow).css("text-align", "right");
        $("td:eq(5)", nRow).css("text-align", "right");
      }
    '),bAutoWidth=FALSE,bFilter=0,bPaginate=0,bLengthChange=0,bSort=0,bInfo=0,iDisplayLength=29,
    aoColumns=list(list(bSearchable=FALSE),list(bSearchable=FALSE),list(bSearchable=FALSE),
    list(bSearchable=FALSE),list(bSearchable=FALSE),list(bSearchable=FALSE)))
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
      if(input$mdkr==FALSE) {aoColumns=list(list(bSearchable=FALSE),list(bSearchable=FALSE),list(bSearchable=FALSE),
      list(bSearchable=FALSE),list(bSearchable=FALSE),list(bSearchable=FALSE))}
      else {aoColumns=list(list(bSearchable=FALSE),list(bSearchable=FALSE),list(bSearchable=FALSE),
      list(bSearchable=FALSE),list(bSearchable=FALSE))}
    )
  )  
  
  
  #################################
  # PLOT
  #################################

  # Create a reactive plot based on qResults_short()
  # Then make available for output into UI
  output$destPlot <- renderPlot({
    if(progCount2()==0) return()
    # Take a dependency on input$update by reading it. (Nothing is actually done with the value.)
    input$update            
      # Transform qResults_short into a format acceptable for plotting
      graphdata <- data.frame(
        Destination = factor(c("Homeless","Institutional","Temporary","Permanent")),
        Value = c(sum(qResults_short()[,"HOMELESS"]),sum(qResults_short()[,"INSTITUTIONAL"]),
          sum(qResults_short()[,"TEMPORARY"]),sum(qResults_short()[,"PERMANENT"])),
        color = c("firebrick2","darkorchid","royalblue2","green4")
      )
      if(input$mdkr==FALSE) {
        UNKNOWN <- data.frame( 
          Destination = "Unknown",
          Value = sum(qResults_short()[,"UNKNOWN"]),
          color = "gray45"
        )
        graphdata <- rbind(UNKNOWN,graphdata)
      }
      progress <- Progress$new(session)
      progress$set(message="Creating chart",detail="Please wait a moment...")
      #Graph reactivity is invalidated unless update button is pressed   	
      isolate(
        print(
          # Begin plotting with ggplot()
          # Define variables
          ggplot(data=graphdata, aes(x=factor(Destination,levels=Destination),y=Value)) +
            # Define as bar chart
            geom_bar(fill=graphdata$color,
              color="black",stat="identity") + scale_fill_manual(values=graphdata$color,name="Destination") +
            # Add text labels
            geom_text(aes(label = paste(Value," (",round(100*Value/sum(Value),digits=1),"%)",sep=""),
              size=10,vjust=-.5)) +
            # Remove legend
            theme(legend.position="none") +
            # Define title
              ggtitle(paste("Housing Outcomes for ", 
                ifelse(input$reportLevel!="Program","Programs in ",
                paste(input$agencySelect,": ",sep="")),
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
        div(align="center",plotOutput("destPlot"))
      ))
    }
    else {
      div(
        h4("Main Panel",align="center"),
        tabsetPanel(
          tabPanel("Summary",
            div(downloadButton("downloadEnrolls","Download Enrollments"),align="right"),
            dataTableOutput("summaryTable")
          ),
          if(input$reportLevel!="Program") {
            tabPanel("Programs",
              div(div(downloadButton("downloadProgs","Download Programs"),align="right"),br(),dataTableOutput("progsTable"))
            )
          },
          tabPanel("Plot",
            plotOutput("destPlot")
          )
        )
      )
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
