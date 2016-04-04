

# install RODBC
# install mvtnorm
install.packages(c("RODBC","mvtnorm"))
require("RODBC")
require("mvtnorm")

library(RODBC)

testing <- TRUE                               # points to different output folders.
platform <- 'CAMP_RST20160601-DougXXX-4.5'    # points to different platforms

paste(cat('testing == TRUE\n'))
setwd(paste0("\\\\LAR-FILE-SRV/Data/PSMFC_CampRST/ThePlatform/",platform,"/R-Interface/"))
source(paste0("\\\\LAR-FILE-SRV/Data/PSMFC_CampRST/ThePlatform/",platform,"/R-Interface/source_all_testing.R"))

theExcel <- read.csv('theExcel.csv')
theExcel <- theExcel[theExcel$Issues == '',]
rownames(theExcel) <- NULL




# specify the range, in terms of theExcel rownames, to test.
for(testi in 1:77){#34:49){   

  by <- 'All'
  river <- as.character(droplevels(theExcel[testi,]$streamName))

  if(river == ''){
    db.file <- db.file1
  } else if(river == 'Sacramento River'){
    db.file <- db.file2
  } else if(river == 'American River'){
    db.file <- db.file3
  } else if(river == ''){
    db.file <- db.file4
  } else if(river == 'Feather River'){
    db.file <- db.file5
  } else if(river == 'Stanislaus River'){
    db.file <- db.file6
  } else if(river == 'Old American Test'){
    db.file <- db.file7
  } else if(river == 'Mokelumne River'){
    db.file <- db.file8
#   } else if(river == "Knight's Landing"){
#     db.file <- db.file9
  } else if(river == "Knight's Landing"){
    db.file <- db.fileA
  }

  if(river != 'Old American Test'){
    site         <- theExcel[testi,]$siteID
    siteText     <- as.character(droplevels(theExcel[testi,]$Site))
    run          <- theExcel[testi,]$RunID
    runText      <- as.character(droplevels(theExcel[testi,]$SalmonRun))
    min.date     <- as.character(as.Date(theExcel[testi,]$minvisitTime,format = "%m/%d/%Y"))
    max.date     <- as.character(as.Date(theExcel[testi,]$maxvisitTime,format = "%m/%d/%Y"))
  } else {
#     river        <- 'american'
#     site         <- 57000
#     siteText     <- 'testing'
#     run          <- 4
#     runText      <- 'Winter'
#     min.date     <- "2013-10-01"
#     max.date     <- "2014-09-29"
  }

  taxon        <- 161980
  output.file  <- paste0("..//Outputs//",river,"//a500Run ",testi,"--",by,"_",river,"_",siteText,"_",min.date,"_",max.date)
  ci           <- TRUE
  output.type  <- "odt"
  from         <- "Trent McDonald, Ph.D., WEST Incorporated"
  to           <- "Doug Threloff, USFWS CAMP Coordinator"
  return.addr  <- "FISH AND WILDLIFE SERVICE!USFWS Caswell State Park Office!1234 Abbey Rd.!Caswell, California  96080!(530) 527-3043, FAX (530) 529-0292"

#   for(byj in 1:4){
# 
#     if(byj == 1){
#       by <- 'day'
#     } else if(byj == 2){
#       by <- 'week'
#     } else if(byj == 3){
#       by <- 'month'
#     } else if(byj == 4){
#       by <- 'year'
#     }
# 
#     output.file  <- paste0("..//Outputs//",river,"//Run ",testi,"--",by,"_",river,"_",siteText,"_",min.date,"_",max.date)
# 
#     F.run.passage      (site, taxon,      min.date, max.date, by=by,     output.file=output.file,         ci=TRUE            )
#   }
  by <- 'All'
  output.file  <- paste0("..//Outputs//",river,"//Run ",testi,"--",by,"_",river,"_",siteText,"_",min.date,"_",max.date)
#   F.lifestage.passage   (site, taxon,      min.date, max.date,            output.file,                     ci=TRUE            )
#   F.byCatch.table      ( site,             min.date, max.date,            output.file                                         )
#   F.release.summary    ( site, taxon, run, min.date, max.date,            output.file                                         )
#   F.weekly.effort      ( site, taxon,      min.date, max.date,            output.file                                         )
  F.allCatch.table     ( site,             min.date, max.date,            output.file                                         )
  F.chinookByDate.table( site,             min.date, max.date,            output.file                                         )
  
#   runs <- c(1,3,5,4)    # Spring, Fall, Late Fall Winter
#   run.names <- c('Spring','Fall','Late Fall','Winter')
#   for(j in 1:4){
#     run <- runs[j]
#     run.name <- run.names[j]
#     output.file  <- paste0("..//Outputs//",river,"//Run ",testi,"--",by,"_",river,"_",siteText,"_",min.date,"_",max.date,"_",run.name)
#     F.size.by.date    ( site, taxon, run, min.date, max.date,            output.file                                         )
#     F.length.frequency( site, taxon, run, min.date, max.date,     paste0(output.file,"_ls=F"),   by.lifestage=FALSE          )
#     F.length.frequency( site, taxon, run, min.date, max.date,     paste0(output.file,"_ls=T"),   by.lifestage=TRUE           )
#   }
}
