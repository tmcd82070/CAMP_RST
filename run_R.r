#  Attach libraries
library(RODBC)
source("source_all.r")

#  Specify parameters to the function
site <- 34000
taxon <- "161980"
min.date <- "2005-12-01"
max.date <- "2006-07-30"
by <-"month"
output.file <- "X:/ThePlatform/CAMP_RST20151123/Outputs/run.passage_GOLF_2015-10-01_10-56-01"
ci <- TRUE

#  Call the function
run.passage <- F.run.passage( site, taxon, min.date, max.date, by, output.file, ci)
