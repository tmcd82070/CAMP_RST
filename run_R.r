#
#   This is an example R script that Karen will construct using VB, then execute via R CMD BATCH
#


#   Need these lines to attach necessary libraries.
#setwd('T:/Working/Jason')

library(campR)
GlobalVars(db.file="../../Platform/CAMP.mdb",
					 output.dir = "../tmpR")




#   Analysis (function) parameters
river     <- 'american'     
site      <- 57000  # American River site id
#site      <- 5000   # Herringer riffle on Feather river
#site      <- 2000   # Steep riffle on Feather river
#site      <- 3000    # Eye riffle on Feather river
# site      <- 1000    # Caswell state park
taxon     <- 161980
run       <- 3
min.date  <- "2013-01-01"
max.date  <- "2013-06-30"
by        <- "day"
output.file <- paste0("../outputs/",river,"/",by,"/",river)
by.lifestage <- FALSE   
ci        <- TRUE
output.type <- "odt"   # or "pdf"
from      <- "Trent McDonald, Ph.D., WEST Incorporated"
to        <- "Doug Threloff, USFWS CAMP Coordinator"
return.addr <- "FISH AND WILDLIFE SERVICE!USFWS Caswell State Park Office!1234 Abbey Rd.!Caswell, California  96080!(530) 527-3043, FAX (530) 529-0292"


#  Call the function
#size.by.date <- F.size.by.date( site, taxon, run, min.date, max.date, output.file)


#   call the function

passage <- F.run.passage( site, taxon, min.date, max.date, by, output.file, ci )

