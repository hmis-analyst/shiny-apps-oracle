#######################################
# USEFUL THINGS TO KNOW
#######################################

# Comments are preceded by -> #.
# To run a line or a selection, press ctrl-r.
# Output will appear on the R Console.
# To clear the R Console, press ctrl-l.


#######################################
# INITIAL SETUP
#######################################

# Installs the RODBC package, which allows you to connect to a remote data source via ODBC.
# Only needs to be run ONE TIME on each computer.
install.packages("RODBC")

# Loads the RODBC package into the current session.
# Needs to be run every time you open R.
library(RODBC)

# Connects to the remote data source using the ODBCDB service name.
# User authentication is required.
# Re-run if the connection times out (it will eventually).
connection <- odbcConnect("ODBCDB")


#######################################
# MAIN QUERY
# Calculates housing outcomes for SFY 2013, displayed as percentage of all clients who exited in SFY 2013.
# Select the entire query and run it (ctrl-r).
#######################################

sqlQuery(connection,"

	SELECT 
	round(count(case when Destination_Code in (110, 16, 2, 1, 111, 121) then 1 end)/count(Program_Enrollment_Key)*100,2)||'%' Homeless,
	round(count(case when Destination_Code in (4, 5, 6, 7, 15, 106, 107, 109) then 1 end)/count(Program_Enrollment_Key)*100,2)||'%' Institutional,
	round(count(case when Destination_Code in (807, 806, 122, 14, 13, 12) then 1 end)/count(Program_Enrollment_Key)*100,2)||'%' Temporary, 
	round(count(case when Destination_Code in (18, 120, 119, 117, 118, 116, 113, 23, 22, 21, 20, 19, 11, 10, 3) then 1 end)/count(Program_Enrollment_Key)*100,2)||'%' Permanent,
	round(count(case when Destination_Code in (8,9) or Destination_Code is null then 1 end)/count(Program_Enrollment_Key)*100,2)||'%' Unknown

	FROM Program_Enrollment

	WHERE 
	Program_Exit_Date >= to_date('7/1/2012','mm/dd/yyyy') and
	Program_Exit_Date <= to_date('6/30/2013','mm/dd/yyyy')

	")

