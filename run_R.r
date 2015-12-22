#  Attach libraries
library(RODBC)
source("source_all.r")

#  Specify parameters to the function
site <- 1000
taxon <- "161980"
min.date <- "1996-09-01"
max.date <- "1997-08-01"
output.file <- "//LAR-FILE-SRV/Data/PSMFC_CampRST/ThePlatform/CAMP_RST20151130/Outputs/lifestage.passage_ST004X_2015-10-15_12-57-31"
ci <- FALSE

#  Call the function
lifestage.passage <- F.lifestage.passage( site, taxon, min.date, max.date, output.file, ci)
