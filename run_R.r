#  Attach libraries
library(RODBC)
source("source_all.r")

#  Specify parameters to the function
site <- 57000
taxon <- "161980"
min.date <- "2012-10-01"
max.date <- "2013-09-29"
output.file <- "X:/ThePlatform/CAMP_RST20150501/Outputs/Secondary_LAR_2015-05-11_08-44-54"
ci <- TRUE

#  Call the function
Secondary <- F.lifestage.passage( site, taxon, min.date, max.date, output.file, ci)
