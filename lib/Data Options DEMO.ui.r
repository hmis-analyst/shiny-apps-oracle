div(
# Display date range input with default values
dateRangeInput("daterange","Date range", format='mm/dd/yyyy',start="2013-07-01",max=Sys.Date()),
br(),
# Display radio buttons with 3 defined report levels
radioButtons("reportLevel", "Report level",list("Group","Agency","Program"),selected="Program"),
br(),
# Display "Group Name" select list when user chooses "Group" report level
conditionalPanel(
  # Define condition of this display
  condition = "input.reportLevel == 'Group'",
  # Query Pathways group names to populate select list
  selectInput("groupSelect", "Group name",as.list(as.character(
    dbGetQuery(connection,"
      SELECT unique Group_Name 
      FROM D_Community_Group_Information
      ORDER BY Group_Name"
    )[[1]]
  )))
),
# Display "Agency Name" select list when user chooses "Agency" or "Program" report levels
conditionalPanel(
  # Define condition of this display
  condition = "input.reportLevel == 'Agency' | input.reportLevel == 'Program'",
  # Query agency names to populate select list
  selectInput("agencySelect", "Agency name",as.list(as.character(
    dbGetQuery(connection,"
      SELECT unique Agency_Name 
      FROM D_Program_Profile_Info
      ORDER BY Agency_Name"
    )[[1]]
  )))
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