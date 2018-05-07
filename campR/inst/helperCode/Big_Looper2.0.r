


#   ---- Set variables necessary for Big Looper completion. 
RVersion <- "3.4.3"
TestingPlatform <- "CAMP_RST20170115-campR1.1.0"       #  What the CAMP people will use; i.e., the static R in the Platform.  Use this most of the time.
excelName <- "theExcel"

#   ---- Set the reports you want to run.  Use the key below. 
# reportRun <- c("A","B","C","D","E","F","G","H","I","J","K","L","M","N","O","P","Q","R","S","T","U","V","W","X","Y","Z")
# reportRun <- c("A","B","C","D","E","F","G","H","I","J","K","L","M","N","O","P")
reportRun <- c("A","B","C","D","E","F","G","H","I","J")
# reportRun <- c("K","L","M","N","O","P")
# reportRun <- c("B","R")
# reportRun <- c("Q")
# reportRun <- c("R","S")

#   ---- Assign the version of R to use.  A real check uses the version of R CAMP people use.
#.libPaths(paste0("C:/Users/jmitchell/Documents/R/win-library/",RVersion))
.libPaths(paste0("//lar-file-srv/Data/PSMFC_CampRST/ThePlatform/",TestingPlatform,"/R/library"))[1]

#   ---- Get necessary packages in order.  
# install.packages(c("RODBC","mvtnorm"))
# require("RODBC",lib.loc=paste0("C:/Users/jmitchell/Documents/R/win-library/",RVersion))
# require("mvtnorm",lib.loc=paste0("C:/Users/jmitchell/Documents/R/win-library/",RVersion))
# require("splines",lib.loc=paste0("C:/Users/jmitchell/Documents/R/win-library/",RVersion))
unloadNamespace("campR")
unloadNamespace("RODBC")
detach("package:RPostgres",unload=TRUE)
detach("package:campR", unload=TRUE)
require(campR)

#   ---- Read in the desired Excel scheme.  This is kept in the helperCode folder of the inst folder
#   ---- in the campR package development folder in CAMP_RST20160601.  
theExcel <- read.csv(paste0("//lar-file-srv/Data/PSMFC_CampRST/ThePlatform/CAMP_RST20160601-DougXXX-4.5/R-Interface/campR/inst/helperCode/",excelName,".csv"))
rownames(theExcel) <- NULL
theExcel <- theExcel[!is.na(theExcel$siteID),]
theExcel <- theExcel[!(theExcel$streamName == "Sacramento River" & theExcel$Season %in% c(2014,2015)),]  # Exclude out years that seemingly have no releases.  

#   ---- Modify theExcel further here, if desired.  Otherwise, delete or comment out.
# theExcel <- theExcel[c(1,5,13,15,24,26,32,52,73),]                 # For Enh Eff development (no RBDD)
# theExcel <- theExcel[c(1,5,13,15,24,26,32,36,52,73),]              # For Enh Eff development (yes RBDD)
# theExcel <- theExcel[c(36),]                                       # For Enh Eff development (only RBDD)

# theExcel <- theExcel[theExcel$streamName == "American River",]     # American
# theExcel <- theExcel[theExcel$streamName == "Feather River",]      # Feather 
# theExcel <- theExcel[theExcel$streamName == "Stanislaus River",]   # Stanislaus
# theExcel <- theExcel[theExcel$streamName == "Sacramento River",]   # Sacramento
# theExcel <- theExcel[!(theExcel$streamName == "Sacramento River"),]   # All but Sacramento

#   ---- Tell the Big Looper where to put all the output.  
# theStem <- paste0("\\\\lar-file-srv/Data/PSMFC_CampRST/ThePlatform/",TestingPlatform)
# outStem <- paste0(theStem,"/Outputs")

theStem <- "C:/Users/jmitchell/Desktop"
outStem <- paste0(theStem,"/Outputs")


#   ---- Identify the possible reports we can run, and folder stems we can create.  
#   ---- This section should not be ameliorated.  
nn <- c("label","folder","report","function")
a  <- c("A" ,"EstProdAllRunsLSReport"    ,"ls.run.passage"                      ,"passageWithLifeStageAssign")
b  <- c("B" ,"EstProdAllRunsReport"      ,"run.passage"                         ,"F.run.passage")
c  <- c("C" ,"PassageEst_FL_Fall"        ,"lifestage.passage.forkLength"        ,"F.lifestage.passage.forkLength")
d  <- c("D" ,"AllCatchTable"             ,"all.catch"                           ,"F.allCatch.table")
e  <- c("E" ,"ByCatchTable"              ,"by.catch"                            ,"F.byCatch.table")
f  <- c("F" ,"ChinookByDate"             ,"chinook.by.date"                     ,"F.chinookByDate.table")
g  <- c("G" ,"ReleaseSummary"            ,"release.summary"                     ,"F.release.summary")
h  <- c("H" ,"SizeByDate"                ,"size.by.date"                        ,"F.size.by.date")
i  <- c("I" ,"LengthFreq"                ,"length.freq"                         ,"F.length.frequency")
j  <- c("J" ,"WeeklyEffortReport"        ,"weekly.effort"                       ,"F.weekly.effort")
k  <- c("K" ,"AutoLS_2Group_YesWgt"      ,"auto.ls.2grp_yWgt"                   ,"F.lifestage.passage.assignLS2group")
l  <- c("L" ,"AutoLS_2Group_NoWgt"       ,"auto.ls.2grp_nWgt"                   ,"F.lifestage.passage.assignLS2groupNoWeight")
m  <- c("M" ,"AutoLS_3Group_YesWgt"      ,"auto.ls.3grp_yWgt"                   ,"F.lifestage.passage.assignLS3group")
n  <- c("N" ,"AutoLS_3Group_NoWgt"       ,"auto.ls.3grp_nWgt"                   ,"F.lifestage.passage.assignLS3groupNoWeight")
o  <- c("O" ,"AutoLS_2or3_AutoWgt"       ,"auto.ls.2or3grp_autoWgt"             ,"F.lifestage.passage.assignLS")
p  <- c("P" ,"AutoLS_2or3_NoWgt"         ,"auto.ls.2or3grp_nWgt"                ,"F.lifestage.passage.assignLSNoWeight")
q  <- c("Q" ,"Enhanced_Eff_Get_Betas"    ,"run.passage.enh"                     ,"F.run.passage.enh")
r  <- c("R" ,"EstProdAllRunsReportENH"   ,"run.passage.enheffT"                 ,"F.run.passage")
s  <- c("S" ,"EstProdAllRunsLSReportENH" ,"ls.run.passage.enheffT"              ,"passageWithLifeStageAssign")
t  <- c("T" ,"PassageEst_FL_FallENH"     ,"lifestage.passage.forkLength.enheffT","F.lifestage.passage.forkLength")
u  <- c("U" ,"AutoLS_2Group_YesWgtENH"   ,"auto.ls.2grp_yWgt.enheffT"           ,"F.lifestage.passage.assignLS2group")
v  <- c("V" ,"AutoLS_2Group_NoWgtENH"    ,"auto.ls.2grp_nWgt.enheffT"           ,"F.lifestage.passage.assignLS2groupNoWeight")
w  <- c("W" ,"AutoLS_3Group_YesWgtENH"   ,"auto.ls.3grp_yWgt.enheffT"           ,"F.lifestage.passage.assignLS3group")
x  <- c("X" ,"AutoLS_3Group_NoWgtENH"    ,"auto.ls.3grp_nWgt.enheffT"           ,"F.lifestage.passage.assignLS3groupNoWeight")
y  <- c("Y" ,"AutoLS_2or3_AutoWgtENH"    ,"auto.ls.2or3grp_autoWgt.enheffT"     ,"F.lifestage.passage.assignLS")
z  <- c("Z" ,"AutoLS_2or3_NoWgtENH"      ,"auto.ls.2or3grp_nWgt.enheffT"        ,"F.lifestage.passage.assignLSNoWeight")
aa <- c("AA","AnnualizeCovars"           ,"annualize.covars"                    ,"annualizeCovars")



#   ---- Clean up our requested report list for use in making folders. 
masterReports <- as.data.frame(rbind(a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z,aa),stringsAsFactors=FALSE)
names(masterReports) <- nn
rownames(masterReports) <- NULL
masterReports <- masterReports[masterReports$label %in% reportRun,]

#   ---- Build up the request folder structure.  
streamNames <- unique(paste0(theExcel$streamName,"--",theExcel$Site))
nStreamNames <- length(streamNames)

reportLabels <- masterReports$label
reportFolders <- masterReports$folder
reportTitles <- masterReports$report
nReports <- length(reportFolders)

#   ---- Helper function to make directories, if they don't already exist.  
makeTheDir <- function(theDir){
  ifelse(!dir.exists(file.path(theDir)), dir.create(file.path(theDir),showWarnings=TRUE,recursive=TRUE), FALSE)
  theDir <- NULL
}



#   ---- Set all the global variables away from the default that you want.  

# db.file                         <<- "..\\Data\\CAMP.mdb"
# output.dir                      <<- "..\\outputs"
# sql.code.dir                    <<- file.path(find.package("campR"),"sql")
# samplePeriodCutTime             <<- "04:00:00"
# max.ok.gap                      <<- 2
# fishingGapMinutes               <<- 10080
# knotMesh                        <<- 15
# halfConeMulti                   <<- 2
# sample.size.forkLength          <<- 100
# sample.size.forkLengthAndWeight <<- 100
# weight.prop.forkLength          <<- 0.5
# forkLength.mean.diff            <<- 10
# time.zone                       <<- "America/Los_Angeles"
# Yes.code                        <<- 1
# No.code                         <<- 2
# seed                            <<- 884
# forkLengthCutPoints             <<- data.frame(lifeStage=c("FL1 leq 41mm","FL2 42-72mm","FL3 73-110mm","FL4 geq 111mm"),cutPoints=c(41,72,110,9999))
# passageRounder                  <<- 4
# eff.min.spline.samp.size        <<- 10
# unassd.sig.digit                <<- 1

#i <- j <- k <- 1


#   ---- If we are building annualized covariates for enhnaced efficiency, we need this.  
if( "AA" %in% reportRun ){
  annual_records <- NULL
}

#   ---- Given the 'theExcel', loop over the streams.  
for(i in 1:nStreamNames){
  
  #   ---- Reduce the master 'theExcel' to one stream.  
  theStreamName <- strsplit(streamNames[i],"--",fixed=TRUE)[[1]][1] #as.character(droplevels(streamNames[i]))
  theSiteName <- strsplit(streamNames[i],"--",fixed=TRUE)[[1]][2]
  the1stExcel <- theExcel[theExcel$streamName == theStreamName & theExcel$Site == theSiteName,]
  seasons <- unique(the1stExcel$Season)
  nSeasons <- length(seasons)
  
  #   ---- Make individual folder for the stream.
  makeTheDir(paste0(outStem,"/",theStreamName,"--",theSiteName))
  
  #   ---- Given the stream, loop over the seasons.  
  for(j in 1:nSeasons){
    
    #   ---- Reduce the stream-based 'theExcel' to one record (Season).
    theSeason <- seasons[j]
    the2ndExcel <- the1stExcel[the1stExcel$Season == theSeason,]
    
    #   ---- Given we are going to compile reports for this Season,
    #   ---- create a folder structure to house the results.
    
    #   ---- Make individual folder for a Season. 
    makeTheDir(paste0(outStem,"/",paste0(theStreamName,"--",theSiteName),"/",theSeason))
    
    #   ---- Given the Season, loop over the desired reports.  
    for(k in 1:nReports){
      
      theReportLabel <- reportLabels[k]
      theReportFolder <- reportFolders[k]
      theReportTitle <- reportTitles[k]
      
      #   ---- Make individual folder for a report
      makeTheDir(paste0(outStem,"/",paste0(theStreamName,"--",theSiteName),"/",theSeason,"/",theReportFolder))
      
      #   ---- At this point, we're ready to create reports for this row in 'theExcel.'
      taxon       <- 161980
      ci          <- TRUE
      site        <- the2ndExcel$siteID
      siteText    <- as.character(droplevels(the2ndExcel$Site))
      min.date    <- as.character(as.Date(the2ndExcel$minvisitTime,format="%m/%d/%Y"))
      max.date    <- as.character(as.Date(the2ndExcel$maxvisitTime,format="%m/%d/%Y"))
      by          <- "All"
      outFileStem <- paste0(outStem,"/",paste0(theStreamName,"--",theSiteName),"/",theSeason,"/",theReportFolder)
      outFile     <- paste0(theReportTitle,"-",siteText)
      outAll      <- paste0(outFileStem,"/",outFile,"-")

      #   ---- Set up the db.file text string, so R knows where to find the database.
      if(theStreamName == 'Sacramento River'){
        #db.file <- "L:/PSMFC_CampRST/ThePlatform/CAMP_RST20161212-campR1.0.0/Data/TestingDBs/CAMP_RBDD_19June20151/CAMP.mdb"
        db.file <- paste0(theStem,"/Data/TestingDBs/newRBDDCAMP_17July2017/CAMP.mdb")
        #db.file <- "C:/Users/jmitchell/Desktop/Test/CAMP.mdb"
      } else if(theStreamName == 'American River'){
        db.file <- paste0(theStem,"/Data/TestingDBs/newAmericanCAMP_21July2017/CAMP.mdb")
        #db.file <- "C:/Users/jmitchell/Desktop/Test/American/CAMP.mdb"
        #db.file <- "L:/PSMFC_CampRST/ThePlatform/CAMP_RST20161212-campR1.0.0/Data/oldTestingDBs/CAMPAmerican2013_2015Database_23June2015/CAMP.mdb"
      } else if(theStreamName == 'Feather River'){
        db.file <- paste0(theStem,"/Data/TestingDBs/newFeatherCAMP_21July2017/CAMP.mdb")
      } else if(theStreamName == 'Stanislaus River'){
        db.file <- paste0(theStem,"/Data/TestingDBs/newStanislausCAMP_21July2017/CAMP.mdb")
      } else if(theStreamName == 'Mokelumne River'){
        db.file <- paste0(theStem,"/Data/TestingDBs/newMokelumneCAMP_10Aug2017/CAMP.mdb")
      } else if(theStreamName == "Knight's Landing"){
        db.file <- paste0(theStem,"/Data/TestingDBs/CAMPKnightsTinsdaleNEW_04Feb2016/CAMP.mdb")
      } else if(theStreamName == "Battle Clear"){
        db.file <- paste0(theStem,"/Data/TestingDBs/newClearBattleCAMP_20July2017/CAMP.mdb")
      }
      
      #   ---- Given the appropriate text string, connect to the database.  
      cat(paste("DB file:", db.file, "\n"))
      ch <- odbcConnectAccess(db.file)
      close(ch)

    
      #   ---- Create the by lifestage and run report.  
      if( theReportLabel == "A" ){
        by <- "All"
        outAll <- paste0(outFileStem,"/",outFile,"-")
        F.passageWithLifeStageAssign(site,taxon,min.date,max.date,output.file=outAll,ci=TRUE,autols=FALSE,nls=NULL,weightuse=NULL,enhmodel=FALSE)
      }
      
      #   ---- Create the ALL runs report -- NO ENHANCED EFFICIENCY.  
      if( theReportLabel == "B" ){
        
        #   ---- Run function run.passage over the four possible temporal periods.  
        for(byj in 1:4){
                 if(byj == 1){by <- 'day'  } 
            else if(byj == 2){by <- 'week' } 
            else if(byj == 3){by <- 'month'} 
            else if(byj == 4){by <- 'year' }

          outAll  <- paste0(outFileStem,"/",by,"-",outFile,"-")
          output.file <- outAll
          F.run.passage(site,taxon,min.date,max.date,by=by,output.file=outAll,ci=TRUE,enhmodel=FALSE)
          
          #   ---- If desired, remove some of the output.  
          #theFiles <- dir(outFileStem)
          #theFiles <- theFiles[grep("Late fall|Spring|Winter|Unassigned",theFiles)]
          #file.remove(paste0(outFileStem,"/",theFiles))
        }
      }
        
      #   ---- Create the forklength report. 
      if( theReportLabel == "C" ){
          
        #   ---- Run function lifestage.passage.forkLength over the four possible temporal periods.  
        for(byj in 1:4){
               if(byj == 1){by <- 'day'  } 
          else if(byj == 2){by <- 'week' } 
          else if(byj == 3){by <- 'month'} 
          else if(byj == 4){by <- 'year' }
            
          outAll  <- paste0(outFileStem,"/",by,"-",outFile,"-")
          output.file <- outAll
          F.lifestage.passage.forkLength(site, taxon, min.date, max.date,by,output.file=output.file,ci=TRUE,enhmodel=FALSE)
        } 
      }
      
      #   ---- Create the all-catch table.  
      if( theReportLabel == "D" ){
        outAll <- paste0(outFileStem,"/",outFile,"-")
        output.file <- outAll
        F.allCatch.table( site,min.date,max.date,output.file)
      }
      
      #   ---- Create the by-catch table.  
      if( theReportLabel == "E" ){
        outAll <- paste0(outFileStem,"/",outFile,"-")
        output.file <- outAll
        F.byCatch.table(site,min.date,max.date,output.file)
      }
      
      #   ---- Create the Chinook-by-date report.
      if( theReportLabel == "F" ){
        outAll <- paste0(outFileStem,"/",outFile,"-")
        output.file <- outAll
        F.chinookByDate.table(site,min.date,max.date,output.file)
      }
      
      #   ---- Create the release summary report. 
      if( theReportLabel == "G" ){      
        runs <- c(1,3,5,4)    # Spring, Fall, Late Fall Winter
        run.names <- c('Spring','Fall','Late Fall','Winter')
        for(l in 1:4){
          run <- runs[l]
          run.name <- run.names[l]
          outAll <- paste0(outFileStem,"/",outFile,".",run.name,"-")
          output.file <- outAll
          F.release.summary(site,taxon,run,min.date,max.date,output.file)
        }
      } 

      #   ---- Create the size-by-date report.  
      if( theReportLabel == "H" ){
        runs <- c(1,3,5,4)    # Spring, Fall, Late Fall, Winter
        run.names <- c('Spring','Fall','Late Fall','Winter')
        for(l in 1:4){
          run <- runs[l]
          run.name <- run.names[l]
          outAll <- paste0(outFileStem,"/",outFile,".",run.name,"-")
          output.file <- outAll
          F.size.by.date(site,taxon,run,min.date,max.date,output.file)
        }
      }
      
      #   ---- Create the length frequency report.  
      if( theReportLabel == "I" ){
        runs <- c(1,3,5,4)    # Spring, Fall, Late Fall, Winter
        run.names <- c('Spring','Fall','Late Fall','Winter')
        for(l in 1:4){
          run <- runs[l]
          run.name <- run.names[l]
          outAll <- paste0(outFileStem,"/",outFile,".",run.name,"-","_ls=F")
          output.file <- outAll
          F.length.frequency(site,taxon,run,min.date,max.date,output.file,by.lifestage=FALSE)
          outAll <- paste0(outFileStem,"/",outFile,".",run.name,"-","_ls=T")
          output.file <- outAll
          F.length.frequency(site,taxon,run,min.date,max.date,output.file,by.lifestage=TRUE)
        }
      }
      
      #   ---- Create the weekly effort report.  
      if(  theReportLabel == "J" ){
        outAll <- paste0(outFileStem,"/",outFile,"-")
        output.file <- outAll
        F.weekly.effort(site,taxon,min.date,max.date,outAll)
      }
 
      #   ---- Create automatic lifestage report:  lifestage to 2 groups and use weight variable.
      if( theReportLabel == "K" ){
        outAll <- paste0(outFileStem,"/",outFile,"-")
        output.file <- outAll
        F.passageWithLifeStageAssign(site,taxon,min.date,max.date,output.file,ci=TRUE,autols=TRUE,nls=2,weightuse=TRUE,enhmodel=FALSE)         
      }

      #   ---- Create automatic lifestage report:  lifestage to 2 groups and don't use weight variable.
      if( theReportLabel == "L" ){
        outAll <- paste0(outFileStem,"/",outFile,"-") 
        output.file <- outAll
        F.passageWithLifeStageAssign(site,taxon,min.date,max.date,output.file,ci=TRUE,autols=TRUE,nls=2,weightuse=FALSE,enhmodel=FALSE) 
      }
    
      #   ---- Create automatic lifestage report:  lifestage to 3 groups and use weight variable.
      if( theReportLabel == "M" ){
        outAll <- paste0(outFileStem,"/",outFile,"-") 
        output.file <- outAll
        F.passageWithLifeStageAssign(site,taxon,min.date,max.date,output.file,ci=TRUE,autols=TRUE,nls=3,weightuse=TRUE,enhmodel=FALSE)       
      }
      
      #   ---- Create automatic lifestage report:  lifestage to 3 groups and don't use weight variable.
      if( theReportLabel == "N" ){
        outAll <- paste0(outFileStem,"/",outFile,"-")
        output.file <- outAll
        F.passageWithLifeStageAssign(site,taxon,min.date,max.date,output.file,ci=TRUE,autols=TRUE,nls=3,weightuse=FALSE,enhmodel=FALSE) 
      }
      
      #   ---- Create automatic lifestage report:  let program decide 2 or 3 groups and use weight variable.
      if( theReportLabel == "O" ){
        outAll <- paste0(outFileStem,"/",outFile,"-")
        output.file <- outAll
        F.passageWithLifeStageAssign(site,taxon,min.date,max.date,output.file,ci=TRUE,autols=TRUE,nls=1,weightuse=TRUE,enhmodel=FALSE)            
      }
      
      #   ---- Create automatic lifestage report:  let program decide 2 or 3 groups and don't use weight variable.
      if( theReportLabel == "P" ){
        outAll <- paste0(outFileStem,"/",outFile,"-")
        output.file <- outAll
        F.passageWithLifeStageAssign(site,taxon,min.date,max.date,output.file,ci=TRUE,autols=TRUE,nls=1,weightuse=FALSE,enhmodel=FALSE)       
      }
      
      #   ---- Create enhanced efficiency beta estimates and associated plots and output.  
      if( theReportLabel == "Q" ){
        outAll <- paste0(outFileStem,"/",outFile,"-")
        
        #   ---- The fitting of enhanced efficiency models is assumed to be done in R directly; i.e., not via the Platform.  
        #   ---- Read in the specialized functions for this task explicitly.  
        #require(splines)
        #require(mvtnorm)
        
        #   ---- Git working directory.  
        source("//lar-file-srv/Data/PSMFC_CampRST/ThePlatform/CAMP_RST20160601-DougXXX-4.5/R-Interface/campR/inst/enhEffCode/get_release_data.enh.R")
        source("//lar-file-srv/Data/PSMFC_CampRST/ThePlatform/CAMP_RST20160601-DougXXX-4.5/R-Interface/campR/inst/enhEffCode/run_passage.enh.R")
        source("//lar-file-srv/Data/PSMFC_CampRST/ThePlatform/CAMP_RST20160601-DougXXX-4.5/R-Interface/campR/inst/enhEffCode/est_passage.enh.R")
        source("//lar-file-srv/Data/PSMFC_CampRST/ThePlatform/CAMP_RST20160601-DougXXX-4.5/R-Interface/campR/inst/enhEffCode/est_efficiency.enh.R")
        source("//lar-file-srv/Data/PSMFC_CampRST/ThePlatform/CAMP_RST20160601-DougXXX-4.5/R-Interface/campR/inst/enhEffCode/eff_model.enh.R")
        source("//lar-file-srv/Data/PSMFC_CampRST/ThePlatform/CAMP_RST20160601-DougXXX-4.5/R-Interface/campR/inst/enhEffCode/passageWithLifeStageAssign.enh.R")
        
        #   ---- Function run.passage.enh expects a 'by' that isn't used.   
        by <- 'year' 
        output.file <- outAll
        F.run.passage.enh(site,taxon,min.date,max.date,by=by,output.file=outAll,ci=TRUE)

      }
      
      #   ---- Create the ALL runs report -- WITH ENHANCED EFFICIENCY.  
      if( theReportLabel == "R" ){
        
        #   ---- Run function run.passage over the four possible temporal periods.  
        for(byj in 1:4){
               if(byj == 1){by <- 'day'  } 
          else if(byj == 2){by <- 'week' }
          else if(byj == 3){by <- 'month'}
          else if(byj == 4){by <- 'year' }
          
          outAll  <- paste0(outFileStem,"/",by,"-",outFile,"-")
          output.file <- outAll
          enhmodel <- TRUE
          F.run.passage(site,taxon,min.date,max.date,by=by,output.file=outAll,ci=TRUE,enhmodel=enhmodel)
          
          #   ---- If desired, remove some of the output.  
          #theFiles <- dir(outFileStem)
          #theFiles <- theFiles[grep("Late fall|Spring|Winter|Unassigned",theFiles)]
          #file.remove(paste0(outFileStem,"/",theFiles))
        }
      }
      
      
      
      
      
      
      #   ---- Create the by lifestage and run report -- YES ENHANCED EFFICIENCY.
      if( theReportLabel == "S" ){
        by <- "All"
        outAll <- paste0(outFileStem,"/",outFile,"-")
        F.passageWithLifeStageAssign(site,taxon,min.date,max.date,output.file=outAll,ci=TRUE,autols=FALSE,nls=NULL,weightuse=NULL,enhmodel=TRUE)
      }
      
      #   ---- Create the forklength report -- YES ENHANCED EFFICIENCY. 
      if( theReportLabel == "T" ){
        
        #   ---- Run function lifestage.passage.forkLength over the four possible temporal periods.  
        for(byj in 1:4){
               if(byj == 1){by <- 'day'  } 
          else if(byj == 2){by <- 'week' } 
          else if(byj == 3){by <- 'month'} 
          else if(byj == 4){by <- 'year' }
          
          outAll  <- paste0(outFileStem,"/",by,"-",outFile,"-")
          output.file <- outAll
          F.lifestage.passage.forkLength(site, taxon, min.date, max.date,by,output.file=output.file,ci=TRUE,enhmodel=TRUE)
        } 
      }
      
      #   ---- Create automatic lifestage report:  lifestage to 2 groups and use weight variable -- YES ENHANCED EFFICIENCY.
      if( theReportLabel == "U" ){
        outAll <- paste0(outFileStem,"/",outFile,"-")
        output.file <- outAll
        F.passageWithLifeStageAssign(site,taxon,min.date,max.date,output.file,ci=TRUE,autols=TRUE,nls=2,weightuse=TRUE,enhmodel=TRUE)         
      }
      
      #   ---- Create automatic lifestage report:  lifestage to 2 groups and don't use weight variable -- YES ENHANCED EFFICIENCY.
      if( theReportLabel == "V" ){
        outAll <- paste0(outFileStem,"/",outFile,"-") 
        output.file <- outAll
        F.passageWithLifeStageAssign(site,taxon,min.date,max.date,output.file,ci=TRUE,autols=TRUE,nls=2,weightuse=FALSE,enhmodel=TRUE) 
      }
      
      #   ---- Create automatic lifestage report:  lifestage to 3 groups and use weight variable -- YES ENHANCED EFFICIENCY.
      if( theReportLabel == "W" ){
        outAll <- paste0(outFileStem,"/",outFile,"-") 
        output.file <- outAll
        F.passageWithLifeStageAssign(site,taxon,min.date,max.date,output.file,ci=TRUE,autols=TRUE,nls=3,weightuse=TRUE,enhmodel=TRUE)       
      }
      
      #   ---- Create automatic lifestage report:  lifestage to 3 groups and don't use weight variable -- YES ENHANCED EFFICIENCY.
      if( theReportLabel == "X" ){
        outAll <- paste0(outFileStem,"/",outFile,"-")
        output.file <- outAll
        F.passageWithLifeStageAssign(site,taxon,min.date,max.date,output.file,ci=TRUE,autols=TRUE,nls=3,weightuse=FALSE,enhmodel=TRUE) 
      }
      
      #   ---- Create automatic lifestage report:  let program decide 2 or 3 groups and use weight variable -- YES ENHANCED EFFICIENCY.
      if( theReportLabel == "Y" ){
        outAll <- paste0(outFileStem,"/",outFile,"-")
        output.file <- outAll
        F.passageWithLifeStageAssign(site,taxon,min.date,max.date,output.file,ci=TRUE,autols=TRUE,nls=1,weightuse=TRUE,enhmodel=TRUE)            
      }
      
      #   ---- Create automatic lifestage report:  let program decide 2 or 3 groups and don't use weight variable -- YES ENHANCED EFFICIENCY.
      if( theReportLabel == "Z" ){
        outAll <- paste0(outFileStem,"/",outFile,"-")
        output.file <- outAll
        F.passageWithLifeStageAssign(site,taxon,min.date,max.date,output.file,ci=TRUE,autols=TRUE,nls=1,weightuse=FALSE,enhmodel=TRUE)       
      }
      
      #   ---- Estimate annualized covariates.  We build this report here in the Big Looper;  all others create external output.  
      if( theReportLabel == "AA" ){
        # aa_record <- annualizeCovars(site,min.date,max.date,theSeason,taxon)
        # output.file  <- paste0(outFileStem,"/",outFile,"-",theSeason)
        # save(aa_record,file=paste0(output.file,"-","aa_record.RData"))
        # annual_records <- rbind(annual_records,aa_record)
        
        #   ---- It is likely the loop broke, it was done in pieces, etc.  So, read in each of the aa_records,
        #   ---- from all their folders. Be sure theExcel is everything you want to include.  To use this part,
        #   ---- turn off the four lines above used to make aa_record and save it.  
        
        #   ---- After all aa_records are made, run this whole section.  To be safe, you can do use_data after. 
        output.file <- paste0(outFileStem,"/",outFile,"-",theSeason)
        load(file=paste0(output.file,"-","aa_record.RData"))
        annual_records <- rbind(annual_records,aa_record)
        
        #   ---- Check to see if we have considered the last aa_record, so as to save the final built annual_records.
        if(i == nStreamNames & j == nSeasons & k == nReports){

          #   ---- Estimate a grand mean, per site.
          for(ss in 1:length(unique(annual_records$site))){
            siteMean <- apply(annual_records[annual_records$site == unique(annual_records$site)[ss],],2,function(x) mean(x[!is.nan(x)]))
            siteMean[names(siteMean) == "Season"] <- -9999
            annual_records <- rbind(annual_records,siteMean)
          }
          devtools::use_data(annual_records,internal=FALSE,overwrite=TRUE)
        }
        #   ---- End of all section to do at once.  
        
        
      }
    }
  }
}

