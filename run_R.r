#  Attach libraries
library(RODBC)
source("source_all.r")

#  Specify parameters to the function
site <- 57000
taxon <- "161980"
min.date <- "2013-10-01"
max.date <- "2014-09-29"
by <-"week"
output.file <- "X:/ThePlatform/CAMP_RST20150501 - Copy/Outputs/PASS.2014.WEEK.NEW.EFF_LAR_2015-06-17_07-57-27"
ci <- FALSE

#  Call the function
PASS.2014.WEEK.NEW.EFF <- F.run.passage( site, taxon, min.date, max.date, by, output.file, ci)
