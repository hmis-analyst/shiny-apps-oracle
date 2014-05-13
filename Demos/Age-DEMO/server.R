#
#  Age Demo Server.r
#
# Load shiny, RODBC, and ggplot2 packages
library(shinyIncubator)
library(RJDBC)
library(ggplot2)
library(stringr)

libPath1 <- "~/HMIS Data Analyst/lib/"
libPath2 <- "../../lib/"

# Establish JDBC connection using RJDBC
source(paste(libPath1,"conn-Ora-Georgia_Base.r",sep=""),local=TRUE)


# Define server logic required to query/graph HMIS age data
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
        FROM D_Program_Profile_Info PPI 
        FULL JOIN D_Program_Community_Info PCI 
          on PPI.Program_Key = PCI.Program_Key 
        FULL JOIN D_Community_Group_Information CGI 
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
  source(paste(libPath2,"DataOptions-Ora-DEMO.server.r",sep=""), local=TRUE)

  
  
  #################################
  # MAIN QUERY
  #################################
  
  # Store query results into a reactive data frame
  queryResults <- reactive({
    input$update
    progress <- Progress$new(session)
    
    progress$set(message="Retrieving program data",detail="Please wait a moment...")
    # Query age breakdown  based on user selections    
    isolate(
      queryResult <- dbGetQuery(connection,paste("
          select age_group, count(*) from (
           SELECT 
            CASE
             when trunc( months_between(sysdate, DATE_OF_BIRTH) / 12 ) <= 5 then 1
             when trunc( months_between(sysdate, DATE_OF_BIRTH) / 12 ) <= 9 then 2
             when trunc( months_between(sysdate, DATE_OF_BIRTH) / 12 ) <= 14 then 3
             when trunc( months_between(sysdate, DATE_OF_BIRTH) / 12 ) <= 19 then 4
             when trunc( months_between(sysdate, DATE_OF_BIRTH) / 12 ) <= 24 then 5
             when trunc( months_between(sysdate, DATE_OF_BIRTH) / 12 ) <= 29 then 6
             when trunc( months_between(sysdate, DATE_OF_BIRTH) / 12 ) <= 34 then 7
             when trunc( months_between(sysdate, DATE_OF_BIRTH) / 12 ) <= 39 then 8
             when trunc( months_between(sysdate, DATE_OF_BIRTH) / 12 ) <= 44 then 9
             when trunc( months_between(sysdate, DATE_OF_BIRTH) / 12 ) <= 49 then 10
             when trunc( months_between(sysdate, DATE_OF_BIRTH) / 12 ) <= 54 then 11
             when trunc( months_between(sysdate, DATE_OF_BIRTH) / 12 ) <= 59 then 12
             when trunc( months_between(sysdate, DATE_OF_BIRTH) / 12 ) <= 64 then 13
             when trunc( months_between(sysdate, DATE_OF_BIRTH) / 12 ) <= 69 then 14
             when trunc( months_between(sysdate, DATE_OF_BIRTH) / 12 ) <= 74 then 15
             when trunc( months_between(sysdate, DATE_OF_BIRTH) / 12 ) <= 79 then 16
             when trunc( months_between(sysdate, DATE_OF_BIRTH) / 12 ) <= 84 then 17
             when trunc( months_between(sysdate, DATE_OF_BIRTH) / 12 ) <= 9999 then 18
            else 0
            end age_group
           
           FROM Program_Enrollment PE
           JOIN Client_Information CI
            on PE.Client_Key = CI.Client_Key
           JOIN D_Program_Community_Info PCI
            on PE.Program_Key = PCI.Program_Key
           JOIN D_Community_Group_Information CGI
            on PCI.Group_Key = CGI.Group_Key
           JOIN D_Program_Profile_Info PPI
            on PE.Program_Key = PPI.Program_Key

        WHERE
          (Program_Exit_Date >= to_date('",beginSelect(),"','yyyy-mm-dd') or Program_Exit_Date is null) and
          Program_Entry_Date <= to_date('",endSelect(),"','yyyy-mm-dd') and ",
          finalSelect_Table(),input$reportLevel,"_Key=",finalSelect_Key(), 
          "and trunc( months_between(sysdate, DATE_OF_BIRTH) / 12 ) <= 112
      )
      group by age_group
      order by age_group"
      ,sep="")
      )
    )

    progress$close() 
#    x <- queryResult[1,1]
#    y <- queryResult[1,2]
    return(queryResult)
	})
  
  
  #################################
  # PLOT
  #################################

  # Create a reactive plot based on queryResults()
  # Then make available for output into UI
	output$Plot <- renderPlot({
  a <- c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18)
  b <- c(0,0,0,0,0,0,0,0,0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
  rs <- data.frame(a,b)
	  
    if (progCount2()==0) return()
    input$update
x <- queryResults()[1]
y <- queryResults()[2]
j <- 1
for (i in x) {   
  rs[i,2] <- y[j] 
  j <- j+1
}

graphData <- data.frame(Gender = c(
  '0-5','6-9','10-14','15-19','20-24',
  '25-29','30-34','35-39','40-44','44-49',
  '50-54','55-59','60-64','65-69','70-74',
  '75-79','80-84','85-UP'),Value=rs[[2]])
#    graphData <- data.frame(Gender = c('0-12','13-18','19-35','36-65','66-99'),Value=c(queryResults()[[1]],queryResults()[[2]]))
#graphData <- data.frame(Gender = c('0-12','13-18','19-35','36-65','66-99'),c(rs()[2]))

    progress <- Progress$new(session)
    progress$set(message="Creating chart",detail="Please wait a moment...")
    # Transform queryResults() into a format acceptable for plotting
    isolate(  
		  print(
        # Begin plotting with ggplot()
        # Define variables
        ggplot(graphData,aes(x=factor(Gender,levels=Gender),y=Value)) + 
		    # Define as bar chart
        geom_bar(fill=c(
          "green4","green4","green4","green4","green4",
          "green4","green4","green4","green4","green4",
          "green4","green4","green4","green4","green4",
          "green4","green4","green4"),stat='identity',color="black") + 
		    # Define title
          ggtitle(paste("Age Breakdown for ", ifelse(input$reportLevel!="Program","Programs in ",paste(input$agencySelect,": ",sep="")),
            finalSelect_Text(),
            "\nWhere ",finalSelect_Table(),input$reportLevel,"_Key=",finalSelect_Key(),Sys.getenv("HOME"),
            "\nReport Period: ",substr(beginSelect(),6,7),"/",substr(beginSelect(),9,10),"/",
            substr(beginSelect(),1,4)," - ",substr(endSelect(),6,7),"/",substr(endSelect(),9,10),"/",
            substr(endSelect(),1,4),sep="")) + 
		    # Define axis labels
        xlab("Age") + 
        ylab("Number of Unique Clients")
      )
    )
    progress$close()
	}) 
})
