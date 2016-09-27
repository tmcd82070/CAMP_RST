


# install RODBC
# install mvtnorm
install.packages(c("RODBC","mvtnorm"))
require("RODBC")
require("mvtnorm")

library(RODBC)

testing <- TRUE                   # points to different output folders.
platform <- "CAMP_RST20160823-4.5 - Gamma"    # points to different platforms
excelName <- "FeatherExcel"
reportRun <- c("B","C","J")

paste(cat('testing == TRUE\n'))
setwd(paste0("\\\\LAR-FILE-SRV/Data/PSMFC_CampRST/ThePlatform/",platform,"/R-Interface/"))
source(paste0("\\\\LAR-FILE-SRV/Data/PSMFC_CampRST/ThePlatform/",platform,"/R-Interface/source_all_testing.R"))

#   ---- Read in the desired Excel scheme.  
theExcel <- read.csv(paste0(excelName,".csv"))
rownames(theExcel) <- NULL


outStem <- paste0("\\\\lar-file-srv/Data/PSMFC_CampRST/ThePlatform/",platform,"/Outputs")





#   ---- Identify the possible reports we can run, and folder stems we can create.  
nn <- c("label","folder","report","function")
a <- c("A","EstProdAllRunsLSReport","later"                       ,"passageWithLifeStageAssign")
b <- c("B","EstProdAllRunsReport"  ,"run.passage"                 ,"F.run.passage")
c <- c("C","PassageEst_FL_Fall"    ,"lifestage.passage.forkLength","F.lifestage.passage.forkLength")
d <- c("D","AllCatchTable"         ,"later"                       ,"F.allCatch.table")
e <- c("E","ByCatchTable"          ,"later"                       ,"F.byCatch.table")
f <- c("F","ChinookByDate"         ,"later"                       ,"F.chinookByDate.table")
g <- c("G","ReleaseSummary"        ,"later"                       ,"F.release.summary")
h <- c("H","SizeByDate"            ,"later"                       ,"F.size.by.date")
i <- c("I","LengthFreq"            ,"later"                       ,"F.length.frequency")
j <- c("J","WeeklyEffortReport"    ,"weekly.effort"               ,"F.weekly.effort")

#   ---- Clean up our requested report list for use in making folders. 
masterReports <- as.data.frame(rbind(a,b,c,d,e,f,g,h,i,j),stringsAsFactors=FALSE)
names(masterReports) <- nn
rownames(masterReports) <- NULL
masterReports <- masterReports[masterReports$label %in% reportRun,]







#   ---- Build up the request folder structure.  
streamNames <- unique(theExcel$streamName)
nStreamNames <- length(streamNames)

reportLabels <- masterReports$label
reportFolders <- masterReports$folder
reportTitles <- masterReports$report
nReports <- length(reportFolders)

#   ---- Given the 'theExcel', loop over the streams.  
for(i in 1:nStreamNames){
  
  #   ---- Reduce the master 'theExcel' to one stream.  
  theStreamName <- as.character(droplevels(streamNames[i]))
  the1stExcel <- theExcel[theExcel$streamName == theStreamName,]
  seasons <- unique(theFirstExcel$Season)
  nSeasons <- length(seasons)
  
  #   ---- Make individual folder for the stream.
  makeTheDir(paste0(outStem,"/",theStreamName))
  
  #   ---- Given the stream, loop over the seasons.  
  for(j in 1:nSeasons){
    
    #   ---- Reduce the stream-based 'theExcel' to one record (Season).
    theSeason <- seasons[j]
    the2ndExcel <- the1stExcel[the1stExcel$Season == theSeason,]
    
    #   ---- Given we are going to compile reports for this Season,
    #   ---- create a folder structure to house the results.
    
    #   ---- Make individual folder for a Season. 
    makeTheDir(paste0(outStem,"/",theStreamName,"/",theSeason))
    
    #   ---- Given the Season, loop over the desired reports.  
    for(k in 2:nReports){
      
      theReportLabel <- reportLabels[k]
      theReportFolder <- reportFolders[k]
      theReportTitle <- reportTitles[k]
      
      #   ---- Make individual folder for a report
      makeTheDir(paste0(outStem,"/",theStreamName,"/",theSeason,"/",theReport))
      
      #   ---- At this point, we're ready to create reports for this row in 'theExcel.'
      taxon       <- 161980
      ci          <- TRUE
      site        <- the2ndExcel$siteID
      siteText    <- as.character(droplevels(the2ndExcel$Site))
      min.date    <- as.character(as.Date(the2ndExcel$minvisitTime,format="%m/%d/%Y"))
      max.date    <- as.character(as.Date(the2ndExcel$maxvisitTime,format="%m/%d/%Y"))
      by          <- "All"
      outFileStem <- paste0(outStem,"/",theStreamName,"/",theSeason,"/",theReportFolder)
      outFile     <- paste0(theReportTitle,"-",siteText)
      outAll      <- paste0(outFileStem,"/",outFile,"-")
      
      #   ---- Set up the db.file text string, so R knows where to find the database.
      if(theStreamName == ''){
        db.file <- db.file1
      } else if(theStreamName == 'Sacramento River'){
        db.file <- db.file2
      } else if(theStreamName == 'American River'){
        db.file <- db.file3
      } else if(theStreamName == ''){
        db.file <- db.file4
      } else if(theStreamName == 'Feather River'){
        db.file <- db.file5
      } else if(theStreamName == 'Stanislaus River'){
        db.file <- db.file6
      } else if(theStreamName == 'Old American Test'){
        db.file <- db.file7
      } else if(theStreamName == 'Mokelumne River'){
        db.file <- db.file8
        #   } else if(theStreamName == "Knight's Landing"){
        #     db.file <- db.file9
      } else if(theStreamName == "Knight's Landing"){
        db.file <- db.fileA
      } else if(theStreamName == "Battle Clear"){
        db.file <- db.file1
      }
      
      #   ---- Create the ALL runs report.  
      if( theReportLabel == "B" ){
        
        #   ---- Run function run.passage over the four possible temporal periods.  
        for(byj in 4:4){
               if(byj == 1){by <- 'day'  } 
          else if(byj == 2){by <- 'week' } 
          else if(byj == 3){by <- 'month'} 
          else if(byj == 4){by <- 'year' }
          
          outAll  <- paste0(outFileStem,"/",by,"-",outFile,"-")
          F.run.passage(site,taxon,min.date,max.date,by=by,output.file=outAll,ci=TRUE)
          
          #   ---- If desired, remove some of the output.  
          theFiles <- dir(outFileStem)
          theFiles <- theFiles[grep("Late fall|Spring|Winter|Unassigned",theFiles)]
          file.remove(paste0(outFileStem,"/",theFiles))
        }
      }
        
      #   ---- Create the forklength report. 
      if( theReportLabel == "C" ){
          
        #   ---- Run function lifestage.passage.forkLength over the four possible temporal periods.  
        for(byj in 2:2){
          if(byj == 1){by <- 'day'  } 
          else if(byj == 2){by <- 'week' } 
          else if(byj == 3){by <- 'month'} 
          else if(byj == 4){by <- 'year' }
            
          outAll  <- paste0(outFileStem,"/",by,"-",outFile,"-")
          F.lifestage.passage.forkLength(site, taxon, min.date, max.date,by,output.file=outAll,ci=TRUE,autoLS=FALSE,reclassifyFL=TRUE)
        } 
      }
      
      #   ---- Create the weekly effort report.  
      if(  theReportLabel == "J"  ){
        by <- "All"
        outAll <- paste0(outFileStem,"/",outFile,"-")
        F.weekly.effort(site,taxon,min.date,max.date,outAll)
      }
      
    }
  }
}






































#   ---- Helper function to make directories, if they don't already exist.  
makeTheDir <- function(theDir){
  ifelse(!dir.exists(file.path(theDir)), dir.create(file.path(theDir),showWarnings=TRUE,recursive=TRUE), FALSE)
  theDir <- NULL
}
























for(i in 1:nCellsDigi){
  #   ---- Make individual folders.
  nightDir <- paste0("//LAR-FILE-SRV/Data/BTPD_2016/Analysis/Overnights/Results/Nightly/",cellsWithData[i])
  ifelse(!dir.exists(file.path(nightDir)), dir.create(file.path(nightDir),showWarnings=TRUE,recursive=TRUE), FALSE)
  #   ---- Make individual folders: FMP with ...
  FMP1Dir <- paste0("//LAR-FILE-SRV/Data/BTPD_2016/Analysis/Overnights/Results/Nightly/",cellsWithData[i],"/FMP1")
  ifelse(!dir.exists(file.path(FMP1Dir)), dir.create(file.path(FMP1Dir),showWarnings=TRUE,recursive=TRUE), FALSE)
  #   ---- Make individual folders: FMP with ...
  FMP2Dir <- paste0("//LAR-FILE-SRV/Data/BTPD_2016/Analysis/Overnights/Results/Nightly/",cellsWithData[i],"/FMP2")
  ifelse(!dir.exists(file.path(FMP2Dir)), dir.create(file.path(FMP2Dir),showWarnings=TRUE,recursive=TRUE), FALSE)
  #   ---- Make individual folders: FMP with ...
  FMP3Dir <- paste0("//LAR-FILE-SRV/Data/BTPD_2016/Analysis/Overnights/Results/Nightly/",cellsWithData[i],"/FMP3")
  ifelse(!dir.exists(file.path(FMP3Dir)), dir.create(file.path(FMP3Dir),showWarnings=TRUE,recursive=TRUE), FALSE)
  #   ---- Make individual folders:  Doublies for Centroids.
  Dbl1Dir <- paste0("//LAR-FILE-SRV/Data/BTPD_2016/Analysis/Overnights/Results/Nightly/",cellsWithData[i],"/Dbl1")
  ifelse(!dir.exists(file.path(Dbl1Dir)), dir.create(file.path(Dbl1Dir),showWarnings=TRUE,recursive=TRUE), FALSE)
  #   ---- Make individual folders:  Doublies for Clippings.
  Dbl2Dir <- paste0("//LAR-FILE-SRV/Data/BTPD_2016/Analysis/Overnights/Results/Nightly/",cellsWithData[i],"/Dbl2")
  ifelse(!dir.exists(file.path(Dbl2Dir)), dir.create(file.path(Dbl2Dir),showWarnings=TRUE,recursive=TRUE), FALSE)
}
