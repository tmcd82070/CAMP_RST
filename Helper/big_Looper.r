# install RODBC
# install mvtnorm
install.packages(c("RODBC"))
install.packages(c("mvtnorm"))
install.packages(c("plyr"))
install.packages(c("mclust"))
install.packages(c("car"))
install.packages(c("tidyr"))
install.packages(c("ellipse"))
install.packages(c("dplyr"))
install.packages(c("Rcpp"))
install.packages(c("lazyeval"))

require("RODBC")
require("mvtnorm")
require("plyr")    # these get added in the program run.
require("mclust")
require("car")
require("tidyr")
require("ellipse")
require("dplyr")
require("lazyeval")





testing <- TRUE                               # points to different output folders.
platform <- 'CAMP_RST20160601-DougXXX-4.5'    # points to different platforms

paste(cat('testing == TRUE\n'))
setwd(paste0("\\\\LAR-FILE-SRV/Data/PSMFC_CampRST/ThePlatform/",platform,"/R-Interface/"))
source(paste0("\\\\LAR-FILE-SRV/Data/PSMFC_CampRST/ThePlatform/",platform,"/R-Interface/source_all_testing.R"))

theExcel <- read.csv(paste0('\\\\lar-file-srv/Data/PSMFC_CampRST/ThePlatform/',platform,'/R-Interface/Helper/theExcel.csv'))
theExcel <- theExcel[theExcel$Issues == '',]
rownames(theExcel) <- NULL




# specify the range, in terms of theExcel rownames, to test.
for(testi in 5:33){#34:49){   
  
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
    min.date     <- as.character(as.Date(theExcel[testi,]$minvisitTime,format = "%m/%d/%Y"))#"2007-01-15"  #
    max.date     <- as.character(as.Date(theExcel[testi,]$maxvisitTime,format = "%m/%d/%Y"))#"2007-02-15"  #
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
  
  beg0 <- beg1 <- beg2 <- beg3 <- beg4 <- beg5 <- beg6 <- 0  # set up so jared's function can return the J report.
  
  beg0 <- Sys.time(); F.lifestage.passage               (site, taxon, min.date, max.date, output.file, ci=TRUE);                          end0 <- Sys.time(); diff.time0 <- as.numeric(end0 - beg0,units="hours");
  
  # ----- automatic lifestage assignment functions --- added 4/4/2016 based on jared's work -----
  
  # Start writing to an output file
  
  sink(paste0(output.file,'J1.txt')); beg1 <- Sys.time(); F.lifestage.passage.assignLS2group(site, taxon, min.date, max.date, paste0(output.file,"_",'J1'), ci=TRUE);         end1 <- Sys.time(); diff.time1 <- as.numeric(end1 - beg1,units="hours"); sink(); # lifestage to 2 groups, use weight var
  sink(paste0(output.file,'J2.txt')); beg2 <- Sys.time(); F.lifestage.passage.assignLS2groupNoWeight(site, taxon, min.date, max.date, paste0(output.file,"_",'J2'), ci=TRUE); end2 <- Sys.time(); diff.time2 <- as.numeric(end2 - beg2,units="hours"); sink(); # lifestage to 2 groups, don't use weight var
  sink(paste0(output.file,'J3.txt')); beg3 <- Sys.time(); F.lifestage.passage.assignLS3group(site, taxon, min.date, max.date, paste0(output.file,"_",'J3'), ci=TRUE);         end3 <- Sys.time(); diff.time3 <- as.numeric(end3 - beg3,units="hours"); sink(); # lifestage to 3 groups, use weight var
  sink(paste0(output.file,'J4.txt')); beg4 <- Sys.time(); F.lifestage.passage.assignLS3groupNoWeight(site, taxon, min.date, max.date, paste0(output.file,"_",'J4'), ci=TRUE); end4 <- Sys.time(); diff.time4 <- as.numeric(end4 - beg4,units="hours"); sink(); # lifestage to 3 groups, don't use weight var
  sink(paste0(output.file,'J5.txt')); beg5 <- Sys.time(); F.lifestage.passage.assignLS(site, taxon, min.date, max.date, paste0(output.file,"_",'J5'), ci=TRUE);               end5 <- Sys.time(); diff.time5 <- as.numeric(end5 - beg5,units="hours"); sink(); # let program decide 2/3 groups, use/don't use weight var
  sink(paste0(output.file,'J6.txt')); beg6 <- Sys.time(); F.lifestage.passage.assignLSNoWeight(site, taxon, min.date, max.date, paste0(output.file,"_",'J6'), ci=TRUE);       end6 <- Sys.time(); diff.time6 <- as.numeric(end6 - beg6,units="hours"); sink(); # let program decide 2/3 groups, don't use weight var
  
  # output time stats
  testi.time <- data.frame(testi=rep(paste0("Run ",testi),7),file=c('J0','J1','J2','J3','J4','J5','J6'),times=c(diff.time0, diff.time1, diff.time2, diff.time3, diff.time4, diff.time5, diff.time6))
  write.csv(testi.time,paste0(output.file,'JStuffTime.csv'))
  
  
  #   F.byCatch.table      ( site,             min.date, max.date,            output.file                                         )
  #   F.release.summary    ( site, taxon, run, min.date, max.date,            output.file                                         )
  #   F.weekly.effort      ( site, taxon,      min.date, max.date,            output.file                                         )
  #   F.allCatch.table     ( site,             min.date, max.date,            output.file                                         )
  #   F.chinookByDate.table( site,             min.date, max.date,            output.file                                         )
  
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