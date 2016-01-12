#  Attach libraries
library(RODBC)
source("source_all.r")

#  Specify parameters to the function
site <- 57000
taxon <- "161980"
min.date <- "2012-10-01"
max.date <- "2013-09-29"
by <-"week"
output.file <- "L:/PSMFC_CampRST/ThePlatform/CAMP_RST20160201/Outputs/TASK25_LAR_2016-01-12_11-50-53"
ci <- TRUE

#  Call the function
TASK25 <- F.run.passage( site, taxon, min.date, max.date, by, output.file, ci)
