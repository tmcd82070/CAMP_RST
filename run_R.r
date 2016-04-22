#  Attach libraries
library(RODBC)
source("source_all.r")

#  Specify parameters to the function
site <- 57000
taxon <- "161980"
min.date <- "2013-01-16"
max.date <- "2013-06-08"
output.file <- "//lar-file-srv/Data/PSMFC_CampRST/ThePlatform/CAMP_RST20160601-DougXXX-4.5/Outputs/JAREDTEST_LAR_2016-04-08_08-33-08"
ci <- TRUE

#  Call the function
JAREDTEST <- F.lifestage.passage.assignLS( site, taxon, min.date, max.date, output.file, ci)
