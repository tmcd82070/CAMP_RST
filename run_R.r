#  Attach libraries
library(RODBC)
source("source_all.r")

#  Specify parameters to the function
site <- 3000
taxon <- "161980"
min.date <- "2000-11-26"
max.date <- "2001-07-23"
output.file <- "X:/ThePlatform/CAMP_RST20151123/Outputs/weekly.effort_FR060_2015-09-25_08-53-16"

#  Call the function
weekly.effort <- F.weekly.effort( site, taxon, min.date, max.date, output.file)
