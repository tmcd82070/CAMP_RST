



#   ---- Set variables necessary for Big Looper completion.  
platform <- "CAMP_RST20161015-campR1.0.1"    
#excelName <- "FeatherExcel"
#excelName <- "AmericanExcel"
reportRun <- c("B")#c("B","C","J")

#   ---- Get necessary packages in order.  
install.packages(c("RODBC","mvtnorm"))
require("RODBC")
require("mvtnorm")

#   ---- Install the working version of campR.  
#   ---- Prior to running this step, make sure you install the zip folder.
require(campR)

library(RODBC)


#   ---- Read in the desired Excel scheme.  This is kept in the helperCode folder of the inst folder
#   ---- in the campR package development folder in CAMP_RST20160601.  
theExcel <- read.csv(paste0("//lar-file-srv/Data/PSMFC_CampRST/ThePlatform/CAMP_RST20160601-DougXXX-4.5/R-Interface/campR/inst/helperCode/",excelName,".csv"))
rownames(theExcel) <- NULL

#   ---- Modify theExcel further here, if desired.  Otherwise, delete or comment out.
theExcel <- theExcel[1:2,]

#   ---- Tell the Big Looper where to put all the output.  
theStem <- paste0("\\\\lar-file-srv/Data/PSMFC_CampRST/ThePlatform/",platform)
outStem <- paste0(theStem,"/Outputs")

#   ---- Identify the possible reports we can run, and folder stems we can create.  
#   ---- This section should not be ameliorated.  
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
  seasons <- unique(the1stExcel$Season)
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
    for(k in 1:nReports){
      
      theReportLabel <- reportLabels[k]
      theReportFolder <- reportFolders[k]
      theReportTitle <- reportTitles[k]
      
      #   ---- Make individual folder for a report
      makeTheDir(paste0(outStem,"/",theStreamName,"/",theSeason,"/",theReportFolder))
      
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
      if(theStreamName == 'Sacramento River'){
        db.file <- paste0(theStem,"/Data/TestingDBs/CAMP_RBDD_19June20151/CAMP.mdb")
      } else if(theStreamName == 'American River'){
        db.file <- paste0(theStem,"/Data/TestingDBs/CAMPAmerican2013_2015Database_23June2015/CAMP.mdb")
      } else if(theStreamName == 'Feather River'){
        db.file <- paste0(theStem,"/Data/TestingDBs/CAMP_Feather_8July2016/CAMP.mdb")
      } else if(theStreamName == 'Stanislaus River'){
        db.file <- paste0(theStem,"/Data/TestingDBs/CAMPStanislaus_08Oct2015/CAMP.mdb")
      } else if(theStreamName == 'Mokelumne River'){
        db.file <- paste0(theStem,"/Data/TestingDBs/CAMPMokelumne23Sept2015/CAMP.mdb")
      } else if(theStreamName == "Knight's Landing"){
        db.file <- paste0(theStem,"/Data/TestingDBs/CAMPKnightsTinsdaleNEW_04Feb2016/CAMP.mdb")
      } else if(theStreamName == "Battle Clear"){
        db.file <- paste0(theStem,"/Data/TestingDBs/CAMP_BattleClear_13Jan2016/CAMP.mdb")
      }
      
      #   ---- Given the appropriate text string, connect to the database.  
      cat(paste("DB file:", db.file, "\n"))
      ch <- odbcConnectAccess(db.file)
      close(ch)
      
      
      #   ---- Create the ALL runs report.  
      if( theReportLabel == "B" ){
        
        #   ---- Run function run.passage over the four possible temporal periods.  
        for(byj in 3:3){
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
      if(  theReportLabel == "J" ){
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
