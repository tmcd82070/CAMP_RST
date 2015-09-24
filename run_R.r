#  Attach libraries
library(RODBC)
source("source_all.r")

#  Specify parameters to the function
<<<<<<< HEAD
site <- 4000
taxon <- "161980"
min.date <- "2000-11-26"
max.date <- "2001-07-23"
output.file <- "X:/ThePlatform/CAMP_RST20150501 - Copy/Outputs/NEWdateRANGE._FR042_2015-07-06_07-04-13"
ci <- TRUE

#  Call the function
NEWdateRANGE. <- F.lifestage.passage( site, taxon, min.date, max.date, output.file, ci)
=======
site <- 57000
taxon <- "161980"
min.date <- "2012-10-01"
max.date <- "2013-09-29"
output.file <- "X:/ThePlatform/CAMP_RST20150501/Outputs/Secondary_LAR_2015-05-11_08-44-54"
ci <- TRUE

#  Call the function
Secondary <- F.lifestage.passage( site, taxon, min.date, max.date, output.file, ci)
>>>>>>> 9d98868ded31a228a275a2ef0e507154e8d0e2ca
