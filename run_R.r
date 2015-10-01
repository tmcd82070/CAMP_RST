#  Attach libraries
library(RODBC)
source("source_all.r")

#  Specify parameters to the function
site <- 57000
taxon <- "161980"
min.date <- "2013-01-01"
max.date <- "2013-06-01"
output.file <- "X:/ThePlatform/CAMP_RST20151123/Outputs/weekly.effort_LAR_2015-10-01_13-22-09"

#  Call the function
weekly.effort <- F.weekly.effort( site, taxon, min.date, max.date, output.file)
