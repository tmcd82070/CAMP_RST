#  Attach libraries
library(RODBC)
source("source_all.r")

#  Specify parameters to the function
site <- 57000
taxon <- "161980"
min.date <- "2013-01-01"
max.date <- "2013-06-01"
output.file <- "X:/ThePlatform/CAMP_RST20151123/Outputs/weekly.effort_LAR_2015-10-01_13-22-09"

#  Call the function
weekly.effort <- F.weekly.effort( site, taxon, min.date, max.date, output.file)


#   --- The following was done by Trent to resolve conflicts
#       Below, Trent's original code has been commented out. 
#       Above, Jason's code. 
##
##   This is an example R script that Karen will construct using VB, then execute via R CMD BATCH
##
#
#
##   Need these lines to attach necessary libraries.
##   KAREN: THESE LINES WILL ALWAYS BE HERE.
#library(RODBC)
#setwd('T:/Working/Jason')
#source("T:/Working/Jason/source_all.R")
## source("../source_all.r")  # this will approximate loading the package, replace with library(campRst)
#
##   These parameters will be supplied by Karen's interface
##   Parameters to the function "F.passage" are "site:taxon:run:year:by:db.file"
##   KAREN: VALUES OF THESE (E.G., 17, 161980, ETC.) WILL COME FROM YOUR INTERFACE.  HAVE YOUR INTERFACE
##   WRITE LINES LIKE THIS.  THESE LINES WILL CHANGE DEPENDING ON FUNCTION CALLED
#
#
#
##   Analysis (function) parameters
#river     <- 'american'     
#site      <- 57000  # American River site id
##site      <- 5000   # Herringer riffle on Feather river
##site      <- 2000   # Steep riffle on Feather river
##site      <- 3000    # Eye riffle on Feather river
## site      <- 1000    # Caswell state park
#taxon     <- 161980
#run       <- 3
#min.date  <- "2013-01-01"
#max.date  <- "2013-06-30"
#by        <- "day"
#output.file <- paste0("../outputs/",river,"/",by,"/",river)
#by.lifestage <- FALSE   
#ci        <- TRUE
#output.type <- "odt"   # or "pdf"
#from      <- "Trent McDonald, Ph.D., WEST Incorporated"
#to        <- "Doug Threloff, USFWS CAMP Coordinator"
#return.addr <- "FISH AND WILDLIFE SERVICE!USFWS Caswell State Park Office!1234 Abbey Rd.!Caswell, California  96080!(530) 527-3043, FAX (530) 529-0292"
#
#
##   call the function
#
#passage <- F.passage( site, taxon, run, min.date, max.date, by, output.file, ci )
#
## tmp <- F.lifestage.passage( site, taxon, min.date, max.date, output.file, ci )
#
##F.allCatch.table( site, min.date, max.date, output.file )
#
##F.byCatch.table( site, min.date, max.date, output.file )
#
## F.chinookByDate.table( site, min.date, max.date, output.file )
#
#
#
###F.weekly.passage( site, taxon, run, min.date, max.date, output.file )
##
##F.release.summary(site,taxon,run,min.date,max.date,output.file)
##
##
##
##
##F.size.by.date( site, taxon, run, min.date, max.date, output.file )
##
##F.length.frequency( site, taxon, run, min.date, max.date, by.lifestage, output.file )
##
##F.size.by.date( site, taxon, run, min.date, max.date, output.file )
##
##tmp <- F.annual.passage( site, taxon, run, min.date, max.date, output.file )   
##
##F.weekly.effort( site, min.date, max.date, output.file )  #Done
##
##
###F.biweekly.report( site, min.date, max.date )
##
### need to add annual report
#
#
## ----------------------- TESTING INDIVIDUAL ROUTINES BELOW HERE -------------------------
##tmp2 <- F.get.indiv.fish.data( site, taxon, run, min.date, max.date, keep="unmarked" )
#
##tmp <- F.get.indiv.visit.data( site, run, min.date, max.date )
#
##tmp <- F.get.release.data( site, run, min.date, max.date )
#

