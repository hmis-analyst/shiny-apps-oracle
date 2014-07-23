
# Load shiny, RODBC, and ggplot2 packages
library(shinyIncubator)
library(RJDBC)
library(ggplot2)
library(stringr)
library(shiny)

libPath1 <- "C:/Users/Katherine.Arce/Documents"
libPath2 <- "../../R/RaceApp/"

# Establish JDBC connection using RJDBC
source(paste(libPath1,"myconnectionkey.R,sep=""),local=TRUE)


# Define server logic required to query/graph HMIS gender data
shinyServer(function(input, output, session) {
  
  #################################
  # USER SELECTIONS
  #################################
  
  # Count of programs associated with the user's group/agency/program selection (non-reactive)
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
source(paste(libPath2,"DataOptions-Ora.server.r",sep=""), local=TRUE)


#################################
# MAIN QUERY
#################################

# Store query results into a reactive data frame
queryResults <- reactive({
  input$update
  progress <- Progress$new(session)
  progress$set(message="Retrieving program data",detail="Please wait a moment...")
  # Query gender breakdown  based on user selections    
  isolate(
    queryResults <- dbGetQuery(connection,paste("
                                                SELECT 
                                                count(unique case when Race_Code = 5 then CI.Client_Key end) Asian,
                                                count(unique case when Race_Code = 6 then CI.Client_Key end) Black,
                                                count(unique case when Race_Code = 8 then CI.Client_Key end) White, 
                                                count(unique case when Race_Code in (7, 9, 14 ) then CI.Client_Key end) MultiOther
                                                FROM Program_Enrollment PE
                                                JOIN Client_Information CI
                                                on PE.Client_Key = CI.Client_Key
                                                JOIN Program_Community_Information PCI
                                                on PE.Program_Key = PCI.Program_Key
                                                JOIN Community_Group_Information CGI
                                                on PCI.Group_Key = CGI.Group_Key
                                                JOIN Program_Profile_Info PPI
                                                on PE.Program_Key = PPI.Program_Key
                                                WHERE
                                                (Program_Exit_Date >= to_date('",beginSelect(),"','yyyy-mm-dd') or Program_Exit_Date is null) and
                                                Program_Entry_Date <= to_date('",endSelect(),"','yyyy-mm-dd') and ",
                                                finalSelect_Table(),input$reportLevel,"_Key=",finalSelect_Key() 
                                                ,sep="")
    )
  )
  progress$close() 
  return(queryResults)
})


#################################
# PLOT
#################################

# Create a reactive plot based on queryResults()
# Then make available for output into UI
output$Plot <- renderPlot({
  if (progCount2()==0) return()
  input$update
  graphData <- data.frame(Race = c('Asian','Black', 'White','MultiOther''),Value=c(queryResults()[[1]],queryResults()[[2]]))
  progress <- Progress$new(session)
  progress$set(message="Creating chart",detail="Please wait a moment...")
  # Transform queryResults() into a format acceptable for plotting
  isolate(  
    print(
      # Begin plotting with ggplot()
      # Define variables
      ggplot(graphData,aes(x=factor(Race,levels=Race),y=Value)) + 
        # Define as bar chart
        geom_bar(fill=c("royalblue2","green4"),stat='identity') + 
        # Define title
        ggtitle(paste("Race Breakdown for ", ifelse(input$reportLevel!="Program","Programs in ",paste(input$agencySelect,": ",sep="")),
                      finalSelect_Text(),"\nReport Period: ",substr(beginSelect(),6,7),"/",substr(beginSelect(),9,10),"/",
                      substr(beginSelect(),1,4)," - ",substr(endSelect(),6,7),"/",substr(endSelect(),9,10),"/",
                      substr(endSelect(),1,4),sep="")) + 
        # Define axis labels
        xlab("Race") + 
        ylab("Number of Unique Clients")
    )
  )
  progress$close()
}) 
})