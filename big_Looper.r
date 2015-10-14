
library(RODBC)

testing <- TRUE                   # points to different output folders.
platform <- 'CAMP_RST20151123'    # points to different platforms

paste(cat('testing == TRUE\n'))
setwd(paste0("\\\\LAR-FILE-SRV/Data/PSMFC_CampRST/ThePlatform/",platform,"/R-Interface/"))
source(paste0("\\\\LAR-FILE-SRV/Data/PSMFC_CampRST/ThePlatform/",platform,"/R-Interface/source_all_testing.R"))

theExcel <- read.csv('theExcel.csv')
theExcel <- theExcel[theExcel$Issues == '',]
rownames(theExcel) <- NULL

#testin!!!!


# specify the range, in terms of theExcel rownames, to test.
for(testi in 42:65){
  
  for(byj in 1:4){
    
    if(byj == 1){
      by <- 'day'
    } else if(byj == 2){
      by <- 'week'
    } else if(byj == 3){
      by <- 'month'
    } else if(byj == 4){
      by <- 'year'
    } 

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
    }

    if(river != 'Old American Test'){
      site         <- theExcel[testi,]$siteID   
      siteText     <- as.character(droplevels(theExcel[testi,]$Site))
      run          <- theExcel[testi,]$RunID
      runText      <- as.character(droplevels(theExcel[testi,]$SalmonRun))
      min.date     <- as.character(as.Date(theExcel[testi,]$minvisitTime,format = "%m/%d/%Y"))
      max.date     <- as.character(as.Date(theExcel[testi,]$maxvisitTime,format = "%m/%d/%Y"))
    } else {
      river        <- 'american'  
      site         <- 57000 
      siteText     <- 'testing'
      run          <- 4
      runText      <- 'Winter'
      min.date     <- "2013-10-01"
      max.date     <- "2014-09-29"
    }

    taxon        <- 161980
    output.file  <- paste0("..//Outputs//",river,"//",by,"_",river,"_",siteText,"_",min.date,"_",max.date)        
    ci           <- TRUE
    output.type  <- "odt"
    from         <- "Trent McDonald, Ph.D., WEST Incorporated"
    to           <- "Doug Threloff, USFWS CAMP Coordinator"
    return.addr  <- "FISH AND WILDLIFE SERVICE!USFWS Caswell State Park Office!1234 Abbey Rd.!Caswell, California  96080!(530) 527-3043, FAX (530) 529-0292"
    
#     F.passage       ( site, taxon, run, min.date, max.date, by,        output.file,                ci                      )
#   }

#      F.release.summary ( site, taxon, run, min.date, max.date,            output.file                                         )
#     F.size.by.date    ( site, taxon, run, min.date, max.date,            output.file                                         )
#      F.length.frequency( site, taxon, run, min.date, max.date,     paste0(output.file,"_lifestage=T"),   by.lifestage=FALSE   )
#      F.length.frequency( site, taxon, run, min.date, max.date,     paste0(output.file,"_lifestage=F"),   by.lifestage=TRUE    )
#      F.weekly.effort   ( site, taxon,      min.date, max.date,            output.file                                         )  
    F.run.passage     ( site, taxon,      min.date, max.date, by=by,     output.file=output.file,         ci=TRUE            )
    F.lifestage.passage(site, taxon,      min.date, max.date,            output.file,                     ci=TRUE            )
  }
}