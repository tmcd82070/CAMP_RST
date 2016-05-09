F.get.catch.data <- function( site, taxon, min.date, max.date,autoLS=FALSE,nLS=NULL,weightUse=NULL ){
  ##
  ##   Fetch the catch data for a SINGLE TAXON from an Access data base. Do some initial
  ##   computations, like dates.
  ##
  ##   input:
  ##   db = full path and name of the Access data base to retrieve data from
  ##   tables = vector with named components containing names
  ##           of the table in db to pull values from
  ##   site = site ID of place we want to do estimates for.
  ##   taxon = the taxon number(s) (from luTaxon) to retrieve.  If a scalar, only
  ##       one taxon is retrieved.  If vector of taxon id's, the sum of all
  ##       taxons is retrieved.
  ##   run = the single run ID of the fish we want.  If run = NA, all records for the fish
  ##       will be pulled.
  ##   min.date = minimum date for data to include. This is a text string in the format %Y-%m-%d, or YYYY-MM-DD
  ##   max.date = maximum date for data to include.  Same format as min.date
  ##   autoLS = FALSE, nothing new is done. autoLS =TRUE the life stage is assigned using a mixture distribution/clustering analysis
  ##   nLS = NULL, ignored if autoLS is false, specify the number of groups to be fit
  ##   useWeight = NULL, ignored if autoLS is false, NULL = program decides if to use weight, FALSE = weight is not used for assigning lifestage
  ##
  ##   To be included in the catch data, a record has to be from the site,
  ##   of the correct taxon, of the correct run, and between min and max dates.
  ##

  ##f.banner <- function( x ){
  ##    cat("\n")
  ##    cat(paste(rep("=",50), collapse=""));
  ##    cat(x);
  ##    cat(paste(rep("=",50), collapse=""));
  ##    cat("\n")
  ##}

  ##   *****
  
  if(autoLS){
    ## this is the catch data with weight
    catch <- getCatchDataWeight(taxon,site,min.date,max.date)
    catch <- catch[!is.na(catch$SampleMinutes) & catch$SampleMinutes >= 30,]
    ## need to open a channel for later
#     db <- get( "db.file", env=.GlobalEnv )
#     ch <- odbcConnectAccess(db)
#     close(ch)
  }else{  # old, original way, prior to lifeStage assignment algorithm.
    nvisits <- F.buildReportCriteria( site, min.date, max.date )
    
    if( nvisits == 0 ){
      warning("Your criteria returned no trapVisit table records.")
      return()
    }
    
    #   *****
    #   Open ODBC channel
    db <- get( "db.file", env=.GlobalEnv )
    ch <- odbcConnectAccess(db)
    
    #   *****
    #   This SQL file develops the hours fished and TempSamplingSummary table
    F.run.sqlFile( ch, "QrySamplePeriod.sql", R.TAXON=taxon )
    
    #   *****
    #   This SQL generates times when the traps were not fishing
    F.run.sqlFile( ch, "QryNotFishing.sql" )
    
    #   *****
    #   This SQL generates unmarked fish by run and life stage
    F.run.sqlFile( ch, "QryUnmarkedByRunLifestage.sql", R.TAXON=taxon )
    
    #   *****
    #   Now, fetch the result
    catch <- sqlFetch( ch, "TempSumUnmarkedByTrap_Run_Final" )
    close(ch)
  } #end else
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  ######################################
  ## Jared's Addition
  ## this is where the life stage assignment will happen from a function call
  if(autoLS){
    cat('Starting mixture distribution estimation to assign life stage. \n')
    ##write.csv(catch,paste0(output.file,site,'.csv'),row.names=FALSE)
    cat('\n')
    cat('\n')
    cat('\n')
    cat('\n')
    cat('\n')
    cat('\n')
    
<<<<<<< Updated upstream
    # jason adds so we can see which of the J reports this run comes from.  
    if(exists("testing")){
      if(testing == TRUE){
        theBegs <- c(beg0,beg1,beg2,beg3,beg4,beg5,beg6)
        theJ <- max(theBegs)
        numJ <<- which(c(theBegs) == theJ) - 1
        if(length(numJ) > 1){
          numJ <<- numJ[2]  # a tie -- happens during testing.
        }
      }
    }
    catch <- assignLifeStage(DATA=catch,groupN=nLS,USEWeight=weightUse)  # swap out old catch with the new catch.
=======
#     #   ---- Jason adds so we can see which of the J reports this run originates.  
#     if(exists("testing")){
#       if(testing == TRUE){
#         theBegs <- c(beg0,beg1,beg2,beg3,beg4,beg5,beg6)
#         theJ <- max(theBegs)
#         numJ <<- which(c(theBegs) == theJ) - 1
#       }
#     }  possibly obsolete -- 5/9/2016 am.
    
    #   ---- Swap out the old catch with the new catch.
    catch <- assignLifeStage(DATA=catch,groupN=nLS,USEWeight=weightUse)  
>>>>>>> Stashed changes
    
    # for debugging
    jCatch <<- catch
    #catch <- jCatch
    
<<<<<<< Updated upstream
    catch$lifeStage <- catch$bioLS
    catch$bioLS <- NULL
=======
    #   ---- Preserve the biologist-assigned lifestages.
    #catch$lifeStage <- catch$bioLS
    #catch$bioLS <- NULL
>>>>>>> Stashed changes
    
    cat('\n')
    cat('\n')
    cat('\n')
    cat('The mixture distribution estimation to assign life stage is over. \n')
    cat('<^><^><^><^><^><^><^><^><^><^><^><^><^><^><^><^><^><^><^><^><^><^><^><^><^><^> \n')
    ## for debugging
    ##return(catch)
    ##stop('That is good enough')
  }
  
  ######################################
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  # add 3/8/2016 -- look for long gaps.
  theLongCatches <- catch[catch$SampleMinutes > fishingGapMinutes,c('SampleDate','StartTime','EndTime','SampleMinutes','TrapStatus','siteID','siteName','trapPositionID','TrapPosition')]      # fishingGapMinutes set in source file -- it's global.
  nLongCatches <- nrow(theLongCatches)
  if(nLongCatches > 0){
    
    warning("Long gaps were found in the data so the LongGapLoop series will be run.")
    
    db <- get( "db.file", env=.GlobalEnv )
    ch <- odbcConnectAccess(db)
    
    # SQL code to find the gaps, and modify trapPositionIDs accordingly.
    F.run.sqlFile( ch, "QryFishingGaps.sql", R.FISHGAPMIN=fishingGapMinutes )
    
    # get the updated results
    catch2 <- sqlFetch( ch, "TempSumUnmarkedByTrap_Run_Final" )
    
    catch <- catch2[catch2$SampleMinutes <= fishingGapMinutes,]   # this becomes the master catch df
    
    close(ch)
    
  } else {
    
    # no long gaps to worry about.  so just keep going with the version of catch that we already
    # pulled in from the mdb.
    catch$oldtrapPositionID <- catch$trapPositionID    # these two are the same.  include for compatibility.
    
  }
  
  
  
  
  
  
  
  db <- get( "db.file", env=.GlobalEnv )
  ch <- odbcConnectAccess(db)
  includecatchID <- sqlFetch(ch, "TempSamplingSummary")             # jason add to get variable includeCatchID
  F.sql.error.check(catch)
  
  close(ch)
  
  if(nvisits > 0 & nrow(catch) == 0){
    warning("Your criteria returned no catch records.  Check to make sure valid Fishing occurred within your date range.")
    stop
  }
  
  #   ******************************************************************
  #   Assign time zone (probably does not matter)
  time.zone <- get( "time.zone", env=.GlobalEnv )
  attr(catch$StartTime, "tzone") <- time.zone
  attr(catch$EndTime, "tzone") <- time.zone
  
  #  jason add all this get includeCatchID:  Assign time zone (definitely does matter -- otherwise it goes to MST)
  time.zone <- get( "time.zone", env=.GlobalEnv )
  # includecatchID$StartTime <- includecatchID$timeSampleStarted
  includecatchID$EndTime <- includecatchID$timeSampleEnded
  includecatchID$ProjID <- includecatchID$projectDescriptionID
  includecatchID$timeSampleStarted <- includecatchID$timeSampleEnded <- includecatchID$projectDescriptionID <- includecatchID$trapVisitID <- includecatchID$sampleGearID <- NULL
  # attr(includecatchID$StartTime, "tzone") <- time.zone
  attr(includecatchID$EndTime, "tzone") <- time.zone
  includecatchID <- includecatchID[,c('trapPositionID','EndTime','ProjID','includeCatchID')]
  
  # sampleGearID ProjID trapPositionID trapVisitID
  catch <- merge(catch,includecatchID,by=c('trapPositionID','EndTime','ProjID'),all.x=TRUE)
  
  
  
  #   ********************************************************************
  #   At this point, catch has all visits in it, even if no fish were caught.
  #   It also has non-fishing intervals.  This is how you identify these intervals:
  #       1. zero catch = catch$Unmarked == 0  ($FinalRun and $LifeStage are both "Unassigned" for these lines)
  #       2. not fishing = catch$TrapStatus == "Not fishing"  (equivalently, $trapVisitID is missing for these lines.  Only time its missing.)
  #
  #   Pull apart the visits from the catch, because plus count expansion only applys to positive catches.
  #   Recall, catch currently has multiple lines per trapVisit delineating fish with different fork lengths.
  
  visit.ind <- !duplicated( catch$trapVisitID ) | (catch$TrapStatus == "Not fishing")
  visits <- catch[visit.ind,!(names(catch) %in% c("Unmarked", "FinalRun", "lifeStage", "forkLength", "RandomSelection"))]
  
  #   ********************************************************************
  #   Subset the catches to just positives.  Toss the 0 catches and non-fishing visits.
  catch <- catch[ (catch$Unmarked > 0) & (catch$TrapStatus == "Fishing"), ]
  
  
  
  
  # get summary counts of catch run vs. lifetstage for internal checking.
  totalFish <<- sum(catch$Unmarked)
  
  # jason adds the if here 1/12/2016, since for some reports, we could have 0 valid catch, but non-zero invalid catch.
  # aggregate doesn't work on zero rows, thus requiring the if-clause.
  if(nrow(catch) > 0){
    totalRun <<- aggregate(catch$Unmarked, list(FinalRun=catch$FinalRun), FUN=sum)
    totalLifeStage <<- aggregate(catch$Unmarked, list(LifeStage=catch$lifeStage), FUN=sum)
    totalRunXLifeStage <<- aggregate(catch$Unmarked, list(LifeStage=catch$lifeStage,FinalRun=catch$FinalRun), FUN=sum)
  }
  
  catch$Unassd <- catch$lifeStage # jason add to ID the unassigned lifeStage -- necessary to separate measured vs caught.
  #   ********************************************************************
  catchSave <<- catch
  nHalfCone <- nrow(catch[catch$halfConeID == 1,])
  if(nHalfCone > 0){   # do a different plus-count algorithm in this case.  1/14/2016.
    
    preCatch <- catch
    
    # expand half-cone operations to full-cone.  this needs to happen prior to plus-counting.
    catch$preUnmarked <- catch$Unmarked
    catch$Unmarked <- ifelse(catch$halfConeID == 1,halfConeMulti*catch$preUnmarked,catch$preUnmarked)
    catch$preUnmarked <- NULL    # served its purpose -- also, plus-counting duplicates these numbers
    
    #   Expand the Plus counts
    preCatch2 <- F.expand.plus.counts( preCatch )
    catch2 <- F.expand.plus.counts( catch )
    
    names(preCatch2)[names(preCatch2) == 'Unmarked'] <- 'preUnmarked'    # now, put this variable back to preUnmarked, to prepare for merge
    test <- merge(catch2,preCatch2[,c('trapVisitID','FinalRun','lifeStage','forkLength','RandomSelection','Unassd','preUnmarked')],by=c('trapVisitID','FinalRun','lifeStage','forkLength','RandomSelection','Unassd'),all.x=TRUE)
    
    possiblyOff <- test[is.na(test$preUnmarked),]   # possible trouble spots
    test$preUnmarked[is.na(test$preUnmarked)] <- 0   # unassigned fish that were not assigned to a certain category before, but were after -or- fish that were assigned a diff lifestage and finalrun between preCatch and catch
    
    test$halfConeAssignedCatch   <- ifelse(test$halfConeID == 1 & test$Unassd != 'Unassigned',test$Unmarked - test$preUnmarked,0)    # halfConeAdj have to originate from a halfCone trapping instance.
    test$halfConeUnassignedCatch <- ifelse(test$halfConeID == 1 & test$Unassd == 'Unassigned',test$Unmarked - test$preUnmarked,0)    # halfConeAdj have to originate from a halfCone trapping instance.
    
    test$assignedCatch   <- ifelse(test$Unassd != 'Unassigned',test$Unmarked - test$halfConeAssignedCatch,0)
    test$unassignedCatch <- ifelse(test$Unassd == 'Unassigned',test$Unmarked - test$halfConeUnassignedCatch,0)
    
    # deal with awkwardness of doing the plus-count routine two times with slightly different data (due to plus-counting)
    #                                    ,after 'times2' - before 'times2'
    test$off <- ifelse(test$halfConeID==2,test$Unmarked - test$preUnmarked,test$Unmarked - test$halfConeAssignedCatch - test$halfConeUnassignedCatch - test$preUnmarked)
    #                                    , should be zero
    
    test$preUnmarked <- ifelse(test$halfConeID == 2 & test$off != 0,test$Unmarked,test$preUnmarked)
    test$off2 <- ifelse(test$halfConeID==2,test$Unmarked - test$preUnmarked,test$Unmarked - test$halfConeAssignedCatch - test$halfConeUnassignedCatch - test$preUnmarked)
    
    #   look <- test[,c('trapVisitID','FinalRun','lifeStage','forkLength','RandomSelection','Unassd','trapPositionID','SampleDate','TrapStatus','TrapPosition','halfConeID','Unmarked','preUnmarked','halfConeAssignedCatch','halfConeUnassignedCatch','assignedCatch','unassignedCatch')]
    #   look[as.Date(look$SampleDate) == '2014-02-28' & look$TrapPosition == 'Gate 8',]
    
    test$modUnassignedCatch <- test$halfConeUnassignedCatch + test$unassignedCatch
    test$modAssignedCatch <- test$halfConeAssignedCatch + test$assignedCatch
    
    test$off <- test$off2 <- NULL
    
    catch <- test
    
  } else {
    
    # data query where all are full catch -- just set the fancy variables to zero.
    catch <- F.expand.plus.counts( catch )
    catch$preUnmarked <- catch$Unmarked
    if(nrow(catch) > 0){catch$halfConeAssignedCatch <- 0}
    if(nrow(catch) > 0){catch$halfConeUnassignedCatch <- 0}
    catch$assignedCatch   <- ifelse(catch$Unassd != 'Unassigned',catch$Unmarked - catch$halfConeAssignedCatch,0)
    catch$unassignedCatch <- ifelse(catch$Unassd == 'Unassigned',catch$Unmarked - catch$halfConeUnassignedCatch,0)
    catch$modUnassignedCatch <- catch$halfConeUnassignedCatch + catch$unassignedCatch
    catch$modAssignedCatch <- catch$halfConeAssignedCatch + catch$assignedCatch
  }
  
  #   Reassign factor levels because they may have changed.  I.e., we may have eliminated "Unassigned"
  catch$FinalRun <- as.character( catch$FinalRun )
  catch$lifeStage <- as.character( catch$lifeStage )
  #catch$lifeStage <- as.character( catch$Unassd ) jason - possibly delete
  
  #   ********************************************************************
  #   Assign batch dates -- jason adds the ifs 1/16/2016 to allow zero row visits and catch dfs to pass through
  if(nrow(visits) > 0){visits <- F.assign.batch.date( visits )}
  if(nrow(catch) > 0){catch <- F.assign.batch.date( catch )}
  
  #   Assign attributes
  attr(catch, "siteID" ) <- site
  attr(catch, "site.name") <- catch$siteName[1]
  #attr(catch, "site.abbr") <- catch$siteAbbreviation[1]
  #attr(catch, "runID") <- run
  #attr(catch, "run.name") <- run.name
  #attr(catch, "run.season") <- run.season
  #attr(catch, "site.stream") <- site.stream
  attr(catch, "subsites") <- unique(catch$trapPositionID)
  #attr(catch, "taxonID" ) <- taxon.string
  #attr(catch, "species.name") <- sp.commonName
  #
  cat("First 20 records of catch data frame...\n")
  if( nrow(catch) >= 20 ) print( catch[1:20,] ) else print( catch )
  
  #f.banner("F.get.catch.data - Complete")
  
  #   Return two data frames. One containing positive catches.  The other containing visit and fishing information.
  #tmp.df <- list (catch=catch, visit=visits)
  list( catch=catch, visit=visits )
  
}
