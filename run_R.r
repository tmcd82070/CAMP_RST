#  Attach libraries
library(RODBC)
source("source_all.r")

#  Specify parameters to the function
site <- 4000
taxon <- "161980"
min.date <- "2000-11-26"
max.date <- "2001-07-23"
output.file <- "X:/ThePlatform/CAMP_RST20150501 - Copy/Outputs/NEWdateRANGE._FR042_2015-07-06_07-04-13"
ci <- TRUE

#  Call the function
NEWdateRANGE. <- F.lifestage.passage( site, taxon, min.date, max.date, output.file, ci)
