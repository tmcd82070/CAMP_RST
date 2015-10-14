#  Attach libraries
library(RODBC)
source("source_all.r")

#  Specify parameters to the function
site <- 34000
taxon <- "161980"
min.date <- "2011-09-01"
max.date <- "2012-08-15"
output.file <- "//LAR-FILE-SRV/Data/PSMFC_CampRST/ThePlatform/CAMP_RST20151123/Outputs/lifestage.passage_GOLF_2015-10-14_14-06-34"
ci <- TRUE

#  Call the function
lifestage.passage <- F.lifestage.passage( site, taxon, min.date, max.date, output.file, ci)
