Sys.Year <- function(x) {as.numeric(format(Sys.Date(),"%Y"))}
Sys.Month <- function(x) {as.numeric(format(Sys.Date(),"%m"))}
Sys.Day <- function(x) {as.numeric(format(Sys.Date(),"%d"))}
SFY <- function(x) {if(Sys.Month() < 7) {Sys.Year()+1} else {Sys.Year()}}

# Create a function to modify date format
dateMod <- function(x) {paste(substr(x,6,7),"/",substr(x,9,10),"/",substr(x,1,4),sep="")}

# Create a function to determine if selected program type is appropriate for use in a destinations app
wrongProgType <- function(x) {length(intersect(x,c("ES","TH","PSH","Prevention","RapidReHousing")))==0}
