#  Attach libraries
library(RODBC)
source("source_all.r")

#  Specify parameters to the function
site <- 57000
taxon <- "161980"
run <- 3
min.date <- "2013-10-01"
max.date <- "2014-09-29"
by <-"week"
output.file <- "X:/ThePlatform/CAMP_RST20150204/Outputs/passage_LAR_2015-04-13_08-40-44"
ci <- TRUE

#  Call the function
passage <- F.passage( site, taxon, run, min.date, max.date, by, output.file, ci)
