#  Attach libraries
library(RODBC)
source("source_all.r")

#  Specify parameters to the function
site <- 57000
taxon <- "161980"
min.date <- "2001-12-21"
max.date <- "2002-07-29"
output.file <- "//lar-file-srv/Data/PSMFC_CampRST/ThePlatform/CAMP_RST20160201/Outputs/KRISTA.WEEKLY.EFFORT_LAR_2016-01-29_08-03-07"

#  Call the function
KRISTA.WEEKLY.EFFORT <- F.weekly.effort( site, taxon, min.date, max.date, output.file)
