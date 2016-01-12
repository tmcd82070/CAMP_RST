#  Attach libraries
library(RODBC)
source("source_all.r")

#  Specify parameters to the function
site <- 57000
taxon <- "161980"
run <- 3
min.date <- "2014-01-01"
max.date <- "2014-06-01"
output.file <- "//LAR-FILE-SRV/Data/PSMFC_CampRST/ThePlatform/CAMP_RST20151130/Outputs/size.by.date_LAR_2016-01-11_08-38-35"

#  Call the function
size.by.date <- F.size.by.date( site, taxon, run, min.date, max.date, output.file)
