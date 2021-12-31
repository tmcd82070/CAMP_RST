
#   ---- BE SURE TO ATTACH FIRST.  OTHERWISE, YOU OVERWRITE DB.FILE WITH THE 
#   ---- PLATFORM-SPECIFIC DIRECTORY, AND NOT THE RIVER-SPECIFIC ONE.
require(campR)

#   ---- Set the current Platform development version.  
platform <- 'CAMP_RST20160715-campR1.0.0'    # points to different platforms

#   ---- Read in "theExcel" of river and constraint information to check.  
theExcel <- read.csv(paste0('\\\\lar-file-srv/Data/PSMFC_CampRST/ThePlatform/',platform,'/R/library/campR/helperCode/theExcel.csv'))
theExcel <- theExcel[theExcel$Issues == '',]
rownames(theExcel) <- NULL

#   ---- Point to data and where we want output to go.  
mdbStem <- paste0("\\\\lar-file-srv/Data/PSMFC_CampRST/ThePlatform/",platform,"/Data/TestingDBs/")
outStem <- paste0("\\\\lar-file-srv/Data/PSMFC_CampRST/ThePlatform/",platform,"/Outputs")

#theExcel <- theExcel[rownames(theExcel) != '48',]

#   ---- User variable testi to specify the range of river combos to test.  
for(testi in 77:79){   
  
  by <- 'All'
  river <- as.character(droplevels(theExcel[testi,]$streamName))

  #   ---- Assign the correct database stem and mdb.  
  if(river == 'Sacramento River'){
    db.file <- paste0(mdbStem,"CAMP_RBDD_19June20151/CAMP.mdb")
  } else if(river == 'American River'){
    db.file <- paste0(mdbStem,"CAMPAmerican2013_2015Database_23June2015/CAMP.mdb")
  } else if(river == 'Feather River'){
    db.file <- paste0(mdbStem,"CAMPFeather_17Nov2015/CAMP.mdb")
  } else if(river == 'Stanislaus River'){
    db.file <- paste0(mdbStem,"CAMPStanislaus_08Oct2015/CAMP.mdb")
  } else if(river == 'Mokelumne River'){
    db.file <- paste0(mdbStem,"CAMPMokelumne23Sept2015/CAMP.mdb")
  } else if(river == "Knight's Landing"){
    db.file <- paste0(mdbStem,"CAMPKnightsTinsdaleNEW_04Feb2016/CAMP.mdb")
  } else if(river == "Battle Clear"){
    db.file <- paste0(mdbStem,"CAMP_BattleClear_13Jan2016/CAMP.mdb")
  } 
  
  site         <- theExcel[testi,]$siteID
  siteText     <- as.character(droplevels(theExcel[testi,]$Site))
  run          <- theExcel[testi,]$RunID
  runText      <- as.character(droplevels(theExcel[testi,]$SalmonRun))
  min.date     <- as.character(as.Date(theExcel[testi,]$minvisitTime,format = "%m/%d/%Y"))
  max.date     <- as.character(as.Date(theExcel[testi,]$maxvisitTime,format = "%m/%d/%Y"))

  taxon        <- 161980
  ci           <- TRUE
  output.type  <- "odt"
  from         <- "Trent McDonald, Ph.D., WEST Incorporated"
  to           <- "Doug Threloff, USFWS CAMP Coordinator"
  return.addr  <- "FISH AND WILDLIFE SERVICE!USFWS Caswell State Park Office!1234 Abbey Rd.!Caswell, California  96080!(530) 527-3043, FAX (530) 529-0292"
  
  #   ---- Run function run.passage over the four possible temporal periods.  
  for(byj in 2:2){
    if(byj == 1){
      by <- 'day'
    } else if(byj == 2){
      by <- 'week'
    } else if(byj == 3){
      by <- 'month'
    } else if(byj == 4){
      by <- 'year'
    }
    #output.file  <- paste0(outStem,"/",river,"/Run ",testi,"--",by,"_",river,"_",siteText,"_",min.date,"_",max.date)
    #F.run.passage(site,taxon,min.date,max.date,by=by,output.file=output.file,ci=TRUE)
    
    #   ---- Reclassify lifeStage by forklength. 
    output.file  <- paste0(outStem,"/",river,"/Run ",testi,"--",by,"_",river,"_",siteText,"_",min.date,"_",max.date,"FL")
    F.lifestage.passage.forkLength(site, taxon, min.date, max.date,by,output.file,ci=TRUE,autoLS=FALSE,reclassifyFL=TRUE)
  }
  by <- 'All'
  output.file  <- paste0(outStem,"/",river,"/Run ",testi,"--",by,"_",river,"_",siteText,"_",min.date,"_",max.date)
  #
  #beg0 <- Sys.time()
  passageWithLifeStageAssign(site, taxon, min.date, max.date,output.file,ci=TRUE,autoLS=FALSE,reclassifyFL=FALSE)
  #end0 <- Sys.time()
  #diff.time0 <- as.numeric(end0 - beg0,units="hours")
  
#   
#   
#   # ----- automatic lifestage assignment functions --- added 4/4/2016 based on jared's work -----
#   
#    # Start writing to an output file
#   
#   #sink(paste0(output.file,'J1.txt')); 
#   output.fileJ1 <- output.file
#   output.file <- paste0(output.fileJ1,"_",'J1')
#   #beg1 <- Sys.time(); 
#   F.lifestage.passage.assignLS2group(site,taxon,min.date,max.date, output.file,ci=TRUE,autoLS=TRUE,reclassifyFL=FALSE)         
#   #end1 <- Sys.time(); 
#   #diff.time1 <- as.numeric(end1 - beg1,units="hours"); 
#   output.file <- output.fileJ1
#   #sink(); # lifestage to 2 groups, use weight var
#   
#   #sink(paste0(output.file,'J2.txt')); 
#   output.fileJ2 <- output.file
#   output.file <- paste0(output.file,"_",'J2')
#   #beg2 <- Sys.time(); 
#   F.lifestage.passage.assignLS2groupNoWeight(site,taxon,min.date,max.date,output.file,ci=TRUE,autoLS=TRUE,reclassifyFL=FALSE) 
#   #end2 <- Sys.time(); 
#   #diff.time2 <- as.numeric(end2 - beg2,units="hours"); 
#   output.file <- output.fileJ2
#   #sink(); # lifestage to 2 groups, don't use weight var
#   
#   #sink(paste0(output.file,'J3.txt')); 
#   output.fileJ3 <- output.file   
#   output.file <- paste0(output.file,"_",'J3')
#   #beg3 <- Sys.time(); 
#   F.lifestage.passage.assignLS3group(site,taxon,min.date,max.date,output.file,ci=TRUE,autoLS=TRUE,reclassifyFL=FALSE)       
#   #end3 <- Sys.time(); 
#   #diff.time3 <- as.numeric(end3 - beg3,units="hours"); 
#   output.file <- output.fileJ3
#   #sink(); # lifestage to 3 groups, use weight var
#   
#   #sink(paste0(output.file,'J4.txt')); 
#   output.fileJ4 <- output.file    
#   output.file <- paste0(output.file,"_",'J4')
#   #beg4 <- Sys.time(); 
#   F.lifestage.passage.assignLS3groupNoWeight(site,taxon,min.date,max.date,output.file,ci=TRUE,autoLS=TRUE,reclassifyFL=FALSE) 
#   #end4 <- Sys.time(); 
#   #diff.time4 <- as.numeric(end4 - beg4,units="hours"); 
#   output.file <- output.fileJ4
#   #sink(); # lifestage to 3 groups, don't use weight var
#   
#   #sink(paste0(output.file,'J5.txt')); 
#   output.fileJ5 <- output.file    
#   output.file <- paste0(output.file,"_",'J5')
#   #beg5 <- Sys.time(); 
#   F.lifestage.passage.assignLS(site,taxon,min.date,max.date,output.file,ci=TRUE,autoLS=TRUE,reclassifyFL=FALSE)            
#   #end5 <- Sys.time(); 
#   #diff.time5 <- as.numeric(end5 - beg5,units="hours"); 
#   output.file <- output.fileJ5
#   #sink(); # let program decide 2/3 groups, use/don't use weight var
#   
#   #sink(paste0(output.file,'J6.txt'));
#   output.fileJ6 <- output.file    
#   output.file <- paste0(output.file,"_",'J6')
#   #beg6 <- Sys.time();
#   F.lifestage.passage.assignLSNoWeight(site,taxon,min.date,max.date,output.file,ci=TRUE,autoLS=TRUE,reclassifyFL=FALSE)       
#   #end6 <- Sys.time(); 
#   #diff.time6 <- as.numeric(end6 - beg6,units="hours"); 
#   output.file <- output.fileJ6
#   #sink(); # let program decide 2/3 groups, don't use weight var
#   
# #   # output time stats
# #   testi.time <- data.frame(testi=rep(paste0("Run ",testi),7),file=c('J0','J1','J2','J3','J4','J5','J6'),times=c(diff.time0,diff.time1,diff.time2,diff.time3, diff.time4, diff.time5, diff.time6))
# #   write.csv(testi.time,paste0(output.file,'JStuffTime.csv'))
#   
#   
#   F.byCatch.table      ( site,             min.date, max.date,            output.file                                         )
#   F.release.summary    ( site, taxon, run, min.date, max.date,            output.file                                         )
    F.weekly.effort      ( site, taxon,      min.date, max.date,            output.file                                         )
#   F.allCatch.table     ( site,             min.date, max.date,            output.file                                         )
#   F.chinookByDate.table( site,             min.date, max.date,            output.file                                         )
#   
#   runs <- c(1,3,5,4)    # Spring, Fall, Late Fall Winter
#   run.names <- c('Spring','Fall','Late Fall','Winter')
#   for(j in 1:4){
#     run <- runs[j]
#     run.name <- run.names[j]
#     output.file  <- paste0(outStem,"/",river,"/Run ",testi,"--",by,"_",river,"_",siteText,"_",min.date,"_",max.date,"_",run.name)
#     F.size.by.date    ( site, taxon, run, min.date, max.date,            output.file                                         )
#     F.length.frequency( site, taxon, run, min.date, max.date,     paste0(output.file,"_ls=F"),   by.lifestage=FALSE          )
#     F.length.frequency( site, taxon, run, min.date, max.date,     paste0(output.file,"_ls=T"),   by.lifestage=TRUE           )
#   }
}