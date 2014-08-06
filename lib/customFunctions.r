Sys.Year <- function(x) {as.numeric(format(Sys.Date(),"%Y"))}
Sys.Month <- function(x) {as.numeric(format(Sys.Date(),"%m"))}
Sys.Day <- function(x) {as.numeric(format(Sys.Date(),"%d"))}
SFY <- function(x) {if(Sys.Month() < 7) {Sys.Year()+1} else {Sys.Year()}}

# Create a function to modify date format
dateMod <- function(x) {paste(substr(x,6,7),"/",substr(x,9,10),"/",substr(x,1,4),sep="")}

