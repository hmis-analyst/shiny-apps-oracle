div(
  if(passkey==TRUE) {
    div(
      br(),
      conditionalPanel(
        condition = "input.reportLevel == 'Program'",
        textInput("passkey","Enter passkey to access client data")
      )
    )
  },
  if(APR==TRUE) {
    div(
      checkboxInput("APR", "APR records only", FALSE),
      br()
    )
  },
  # Display date range input with default values
  dateRangeInput(
    inputId = "daterange",
    label = if(exitsApp==TRUE) {"Analyze exits between..."} else {"Date range"}, 
    format='mm/dd/yyyy',
    start=if(returnsApp==TRUE) {paste(SFY()-2,"07","01",sep="-")} else {paste(SFY()-1,"07","01",sep="-")},
    end=if(returnsApp==TRUE) {paste(SFY()-1,Sys.Month(),Sys.Day()-1,sep="-")} else {Sys.Date()},
    min=paste(Sys.Year()-5,Sys.Month(),Sys.Day(),sep="-"),
    max=Sys.Date()
  ),
  br(),
  # Display radio buttons with 3 defined report levels
  radioButtons("reportLevel", "Report level",list("Group","Agency","Program"),selected="Program"),
  br(),
  # Display "Group Name" select list when user chooses "Group" report level
  conditionalPanel(
    # Define condition of this display
    condition = "input.reportLevel == 'Group'",
    # Query Pathways group names to populate select list
    selectInput(
      inputId = "groupSelect", 
      label = "Group name",
      choices = read.csv(paste(libPath2,"Group choices.csv",sep=""))$GROUP,
      selected = "[Select]"
    )
  ),
  # Display "Agency Name" select list when user chooses "Agency" or "Program" report levels
  conditionalPanel(
    # Define condition of this display
    condition = "input.reportLevel == 'Agency' | input.reportLevel == 'Program'",
    # Query agency names to populate select list
    selectInput(
      inputId = "agencySelect", 
      label = "Agency name",
      choices = as.list(c("[Select]",as.character(
        dbGetQuery(connection,"
          SELECT unique Agency_Name 
          FROM Program_Profile_Info
          ORDER BY Agency_Name"
        )[[1]]
      ))),
      selected = "[Select]"
    )
  ),
  # Display "Program Name" select list when user chooses "Program" report level
  conditionalPanel(
    # Define condition of this display
    condition = "input.reportLevel == 'Program'",
    # Call "programChoices" (reactive select list, defined in server.R)
    # Display on sidebar
    uiOutput("programChoices")
  ),
  br(),
  uiOutput("programTypes"),
  br(),br(),br(),br(),br(),br(), br()
)
