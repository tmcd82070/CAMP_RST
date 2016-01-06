#  Attach libraries
library(RODBC)
source("source_all.r")

#  Specify parameters to the function
site <- 57000
taxon <- "161980"
run <- 3
min.date <- "2014-01-01"
max.date <- "2014-02-14"
output.file <- "L:/PSMFC_CampRST/ThePlatform/CAMP_RST20151130/Outputs/NEW_LAR_2016-01-05_16-25-23"

#  Call the function
NEW <- F.size.by.date( site, taxon, run, min.date, max.date, output.file)
