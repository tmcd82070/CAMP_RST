#' @export
#'
#' @title F.get.catch.data - Retrieve data frame with catch information
#'  
#' @description Fetch catch data from an Access database. Do some initial computations, like
#' dates.
#' 
#' @param site The identification number of the site for which estimates are
#'   required.
#' @param taxon The species identifier indicating the type of fish of interest. 
#'   This is always \code{161980}; i.e., Chinook Salmon.
#' @param min.date The start date for data to include. This is a text string in 
#'   the format \code{\%Y-\%m-\%d}, or \code{YYYY-MM-DD}.
#' @param max.date The end date for data to include.  Same format as 
#'   \code{min.date}.
#' @param autoLS A logical indicating whether or not lifestage assignment should
#'   be decided by the computer via a mixture distribution/clustering analysis.
#' @param nLS A numeric communicating the number of new lifestages to assign. 
#'   Values can be \code{2} or \code{3}.
#' @param weightUse A logical indicating if weight data are to be incorporated 
#'   in the assigning of lifestage.  \code{useWeight = NULL}, ignored if
#'   \code{autoLS} is \code{FALSE}, \code{NULL} leads to the program deciding if
#' weight should be used, \code{FALSE} lead to the program not using weight to 
#' assign lifestage
#' 
#' @return A data frame summarizing catch for the site of interest for all traps
#'   between the dates indicated.  Data include biologist- or computer-assigned 
#'   \code{lifeStage}, \code{FinalRun}, and \code{forkLength}.
#'   
#' @details Function \code{F.get.catch.data} fetches all appropriate catch data 
#'   from an Access database, and then processes it for further use.  The 
#'   processing includes several steps. Currently, although variable 
#'   \code{includeCatchID} is separately queried, it is not used in processing 
#'   after the initial catch query.
#'   
#'   Each record contained in the resulting data frame itemizes fork length,
#'   lifestage, and final run, via variables \code{forkLength},
#'   \code{lifeStage}, and \code{FinalRun}, respectively, for each unique combination of 
#'   \code{trapVisitID} and \code{trapPositionID}.
#'   
#'   Counts of captured fish are recorded via variable \code{Unmarked}, with
#'   zero catch containing a \code{0}.  Zero records additionally have variables
#'   \code{lifeStage} and \code{FinalRun} equal to \code{Unassigned}.
#' 
#' @section Lifestage: 
#' Users have the options of reassigning lifestage away from 
#'   the assignments provided by field biologists at the time of capture.
#'   Options for reassignment number several, and are detailed (here?).
#'   
#' @section Gaps in Fishing:  
#' Sometimes, during the normal course of fishing, a 
#'   trap, or \code{trapPositionID}, stops fishing for an extended period of 
#'   time in the middle of the time frame specified by \code{min.date} and 
#'   \code{max.date}.  During this so-called "gap in fishing," subsequent catch 
#'   fitting methodologies have no data via which to estimate catch fit. Thus, 
#'   unexpected behavior may occur, especially if catch was trending 
#'   upwards/downwards immediately before/after a gap in fishing.  To prevent 
#'   statistical methodologies from estimating catch during these periods, catch
#'   imputation procedures are "turned off" by reassigning the 
#'   \code{trapPositionID} of the offending trap after the gap to a different 
#'   \code{trapPositionID}.  In this way, catch is independently estimated for 
#'   each disconnected trapping period, with no estimation occurring during the 
#'   gap.  Gaps must be greater than or equal to the value set by global 
#'   variable \code{fishingGapMinutes} for reassignment to occur, which is 
#'   currently set at 7 days (or 10,080 minutes).
#'   
#'   Any one trap, given a \code{min.date} and \code{max.date}, may have more
#'   than one gap in fishing.  Generally, the number of resulting reassigned 
#'   \code{trapPositionID}s equals one more than the number of gaps. Reassigned 
#'   traps can be identified by a decimal appendage after the original 
#'   \code{trapPositionID}, although the first trapping instance, i.e., before
#'   the first (and possibly only) gap in fishing, retains its original
#'   non-decimal \code{trapPositionID}.
#'   
#' @section Half-cone Adjustment:  
#' On some rivers, the use of half-cone 
#'   adjustments is commonplace.  Practically, the use of a half-cone involves 
#'   covering half of a trap opening, so as to reduce the amount of water that 
#'   flows into it.  This also necessarily reduces the amount of captured fish 
#'   as well. In order to accurately estimate temporal catch trends, statistical
#'   methodologies need to account for this expected reduction.  However, a 
#'   season could include trapping instances involving both full-cone and 
#'   half-cone operations.  To account for this possibility, trapping instances 
#'   utilizing half-cones via variable \code{halfConeID} have their catch 
#'   multiplied by the value of the global variable \code{halfConeMulti}, which 
#'   is currently set at 2.
#'   
#' @section Half-cone Operations \& Plus Counts:  
#' Generally, during a trapping
#'   instance, a small sample is selected from what may be many thousands of
#'   fish.  The resulting sampling distribution, in terms of lifestage and run,
#'   is then applied to the remaining fish not randomly sampled.  The resulting
#'   assigned proportions of unsampled fish form "plus counts."  Functions 
#'   \code{expand.plus.counts}, \code{assign.1dim}, and \code{assign.2dim} 
#'   detail the plus-count algorithm.  Plus-counting requires special 
#'   consideration in light of half-cone adjustments.
#'   
#'   Generally, the estimation of plus counts requires a sample.  Thus, prior to
#'   its implementation, any half-cone adjustments must already be applied. 
#'   However, the plus-count algorithm often considers the sampling distribution
#'   of fish from trapping instances temporally neighboring that of the trapping
#'   instance of interest.  Inevitably, a before and/or after trapping instance 
#'   may have been a full-cone operation, in contrast to the half-cone operation
#'   of the trapping instance of focus, or vice versa.  Thus, resulting 
#'   sampling distributions can become skewed, i.e., the amount by which a
#'   half-cone trapping instance must have its fish counts expanded is not
#'   necessarily an exact multiple of 2.
#'   
#'   To combat this phenonmenon, trapping instances with half-cone operations
#'   are not simply multiplied by the value of the global variable 
#'   \code{halfConeMulti}.  Instead, the plus-count routine is applied twice,
#'   both with and without the \code{halfConeMulti} adjustment applied.  Then,
#'   for each trapping instance, the difference in the count of fish is then
#'   recorded as the "half-cone adjustment" for that particular lifestage, final
#'   run, and forklength combination.  In this way, half-cone adjustments are obtained,
#'   while taking into consideration the possibility that in some instances,
#'   simple application of the \code{halfConeMulti} variables is not advised.
#'   
#' @section Not Fishing:  
#' Similar to "gaps in fishing" are periods of "Not 
#'   fishing."  An instance of Not fishing is a period during which a trap does 
#'   not operate for more than 30 minutes, but less than 7 days.  Instances of 
#'   Not fishing are included as records within the data frame returned by 
#'   function \code{F.get.catch.data}, and can be identified by variable
#'   \code{TrapStatus}, which is set equal to \code{Not fishing}.  Equivalently,
#'   variable \code{trapVisitID} is missing.  (does connie add in sampling less than 30 minutes to a trapping period of good fishing?)
#'   
#' @seealso \code{getCatchDataWeight.R}, \code{expand.plus.counts}, \code{assign.1dim}, and \code{assign.2dim} these also: \code{assignLifeStage.R},
#'   \code{assignLSCompare.R}? 
#'   
#' @examples  
#' 
#' # we would need to query.  what do we want to do?

F.get.catch.data <- function( site, taxon, min.date, max.date,autoLS=FALSE,nLS=NULL,weightUse=NULL ){
  
  # site <- 
  # taxon <- 161980
  # min.date <- "2010-01-01"
  # max.date <- "2010-06-30"
  # autoLS <- FALSE
  # nLS <- NULL
  # weightUse <- NULL
  
  
  #   ---- STEP 1:  Get original raw catch data via querying. 
  if(autoLS){
    
    #   ---- Obtain catch data with weights.
    catch <- getCatchDataWeight(taxon,site,min.date,max.date)
    
  } else {  
    
    #   ---- Obtain catch data without weights.
    nvisits <- F.buildReportCriteria( site, min.date, max.date )
    
    if( nvisits == 0 ){
      warning("Your criteria returned no trapVisit table records.")
      return()
    }
    
    #   ---- Open ODBC channel.
    db <- get( "db.file", env=.GlobalEnv )
    ch <- odbcConnectAccess(db)
    
    #   ---- Develop the hours fished and TempSamplingSummary table.
    F.run.sqlFile( ch, "QrySamplePeriod.sql", R.TAXON=taxon )
    
    #   ---- Generate times when traps were not fishing.
    F.run.sqlFile( ch, "QryNotFishing.sql" )
    
    #   ---- Generates unmarked fish by run and life stage.  
    F.run.sqlFile( ch, "QryUnmarkedByRunLifestage.sql", R.TAXON=taxon )
    
    #   ---- Now, fetch the result.  
    catch <- sqlFetch( ch, "TempSumUnmarkedByTrap_Run_Final" )
    close(ch)
    
    if(nvisits > 0 & nrow(catch) == 0){
      warning("Your criteria returned no catch records.  Check to make sure valid Fishing occurred within your date range.")
      stop
    }
  }
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  #   ---- STEP 3:  Look for gaps in fishing, as defined by global
  #   ----          variable fishingGapMinutes.  If any are found, 
  #   ----          reassign trapPositionIDs.  
  theLongCatches <- catch[!is.na(catch$SampleMinutes) & catch$SampleMinutes > fishingGapMinutes & catch$TrapStatus == "Not fishing",c('SampleDate','StartTime','EndTime','SampleMinutes','TrapStatus','siteID','siteName','trapPositionID','TrapPosition')]
  nLongCatches <- nrow(theLongCatches)
  if(nLongCatches > 0){
    
    warning("Long gaps were found in the data so the LongGapLoop series will be run.")
    
    if( autoLS ){
      
      #   ---- The Gaps series doesn't bring back weights.  So we have to assemble the data
      #   ---- we have from its constituent pieces.  The weight dataframe catch will in theory
      #   ---- have more rows than catch2, due to particular forkLengths being broken out by
      #   ---- a distribution of weights.  So, do a left join to identify those trapPositionIDs
      #   ---- that have had a decimal appended.  
      
      #   ---- For QryFishingGaps.sql to work propertly, we need emulate the conditions under
      #   ---- which it was developed.  
      
      #   ---- Obtain catch data without weights.
      nvisits <- F.buildReportCriteria( site, min.date, max.date )
      
      if( nvisits == 0 ){
        warning("Your criteria returned no trapVisit table records.")
        return()
      }
      
      #   ---- Open ODBC channel.
      db <- get( "db.file", env=.GlobalEnv )
      ch <- odbcConnectAccess(db)
      
      save.image(file = "C:/Users/jmitchell/Desktop/camp.RData")
      
      
      #   ---- Develop the hours fished and TempSamplingSummary table.
      F.run.sqlFile( ch, "QrySamplePeriod.sql", R.TAXON=taxon )
      F.run.sqlFile( ch, "QryNotFishing.sql" )
      F.run.sqlFile( ch, "QryUnmarkedByRunLifestage.sql", R.TAXON=taxon )
      close(ch)
      
      db <- get( "db.file", env=.GlobalEnv )
      ch <- odbcConnectAccess(db)
      F.run.sqlFile( ch, "QryFishingGaps.sql", R.FISHGAPMIN=fishingGapMinutes )
      
      #   ---- Fetch the updated results, which now identify the gaps.  We do all of this just to
      #   ---- identify those.  
      catchWgtGaps <- sqlFetch( ch, "TempSumUnmarkedByTrap_Run_Final" )
      test <- merge(catch,catchWgtGaps[catchWgtGaps$trapVisitID,catchWgtGaps$trapPositionID,catchWgtGaps$oldtrapPositionID,],by=c('trapVisitID'),all.x=TRUE)
      
    } else {
      
      db <- get( "db.file", env=.GlobalEnv )
      ch <- odbcConnectAccess(db)
      
      #   ---- SQL code to find the gaps, and modify trapPositionIDs accordingly.
      F.run.sqlFile( ch, "QryFishingGaps.sql", R.FISHGAPMIN=fishingGapMinutes )
      
      #   ---- Fetch the updated results.  
      catch2 <- sqlFetch( ch, "TempSumUnmarkedByTrap_Run_Final" )
      
      catch <- catch2[catch2$SampleMinutes <= fishingGapMinutes,]   
    }

    
    close(ch)
    
  } else {
    
    #   ---- No long gaps to worry about.  Use catch we already have, 
    #   ---- while ensuring data frame consistency.
    catch$oldtrapPositionID <- catch$trapPositionID
  }
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  #   ---- STEP 2:  Reassign lifestage, if requested.     
  if(autoLS){
    cat('Starting mixture distribution estimation to assign life stage. \n')
    ##write.csv(catch,paste0(output.file,site,'.csv'),row.names=FALSE)
    cat('\n')
    cat('\n')
    cat('\n')
    cat('\n')
    cat('\n')
    cat('\n')
    
    #   ---- Swap out the old catch with the new catch.
    catch <- assignLifeStage(DATA=catch,groupN=nLS,USEWeight=weightUse)  
    
    # for debugging
    jCatch <<- catch
    #catch <- jCatch
    
    cat('\n')
    cat('\n')
    cat('\n')
    cat('The mixture distribution estimation to assign life stage is over. \n')
    cat('<^><^><^><^><^><^><^><^><^><^><^><^><^><^><^><^><^><^><^><^><^><^><^><^><^><^> \n')
    ## for debugging
    ##return(catch)
    ##stop('That is good enough')
  }
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  #   ---- STEP 4:  Get includeCatchID variable.  This is an
  #   ----          historical addition, based on an old request.
  #   ----          The catch sequence works with this variable 
  #   ----          included, but unknown behavior if it were to 
  #   ----          be removed.  So, this section stays for now. 
  
  #   ---- Fetch variable includeCatchID.  
  db <- get( "db.file", env=.GlobalEnv )
  ch <- odbcConnectAccess(db)
  includecatchID <- sqlFetch(ch, "TempSamplingSummary")             
  F.sql.error.check(catch)
  close(ch)
  
  #   ---- Assign time zone (probably does not matter).
  time.zone <- get( "time.zone", env=.GlobalEnv )
  attr(catch$StartTime, "tzone") <- time.zone
  attr(catch$EndTime, "tzone") <- time.zone
  
  #   ---- Add in includeCatchID:  Assign time zone (definitely does matter -- otherwise it goes to MST).
  time.zone <- get( "time.zone", env=.GlobalEnv )
  includecatchID$EndTime <- includecatchID$timeSampleEnded
  includecatchID$ProjID <- includecatchID$projectDescriptionID
  includecatchID$timeSampleStarted <- includecatchID$timeSampleEnded <- includecatchID$projectDescriptionID <- includecatchID$trapVisitID <- includecatchID$sampleGearID <- NULL
  attr(includecatchID$EndTime, "tzone") <- time.zone
  includecatchID <- includecatchID[,c('trapPositionID','EndTime','ProjID','includeCatchID')]
  catch <- merge(catch,includecatchID,by=c('trapPositionID','EndTime','ProjID'),all.x=TRUE)
  
  
  
  #   ---- STEP 5:  Clean up visits and catch dataframes.  
  
  #   ---- Reduce visits by removing duplicates in trapVisitID and throwing 
  #   ---- out the"Not fishing." 
  visit.ind <- !duplicated( catch$trapVisitID ) | (catch$TrapStatus == "Not fishing")
  visits <- catch[visit.ind,!(names(catch) %in% c("Unmarked", "FinalRun", "lifeStage", "forkLength", "RandomSelection"))]
  
  #   ---- Reduce catches to just positives.  Toss the 0 catches and non-fishing visits.
  catch <- catch[ (catch$Unmarked > 0) & (catch$TrapStatus == "Fishing"), ]
  
  
  #   ---- STEP 6:  Expand for half-cone adjustments, and calculate plus counts.  
  
  #   ---- Get summary counts of catch run vs. lifetstage for internal checking.
  totalFish <<- sum(catch$Unmarked)
  
  #   ---- Allow for 0 valid catch, but non-zero invalid catch.
  #   ---- Use if-clause since aggregate doesn't work on zero rows.
  if(nrow(catch) > 0){
    totalRun <<- aggregate(catch$Unmarked, list(FinalRun=catch$FinalRun), FUN=sum)
    totalLifeStage <<- aggregate(catch$Unmarked, list(LifeStage=catch$lifeStage), FUN=sum)
    totalRunXLifeStage <<- aggregate(catch$Unmarked, list(LifeStage=catch$lifeStage,FinalRun=catch$FinalRun), FUN=sum)
  }
  
  #   ---- ID the unassigned lifeStage - necessary to separate measured vs caught.
  catch$Unassd <- catch$lifeStage 
  
  #   ---- See how many halfcone records there are.  
  nHalfCone <- nrow(catch[catch$halfConeID == 1,])
  if(nHalfCone > 0){  
    
    preCatch <- catch
    
    #   ---- Expand half-cone operations to full-cone.  This needs to happen prior to plus-counting.
    catch$preUnmarked <- catch$Unmarked
    catch$Unmarked <- ifelse(catch$halfConeID == 1,halfConeMulti*catch$preUnmarked,catch$preUnmarked)
    catch$preUnmarked <- NULL    # Served its purpose - also, plus-counting duplicates these numbers.
    
    #   ---- Expand the Plus counts.  
    preCatch2 <- F.expand.plus.counts( preCatch )
    catch2 <- F.expand.plus.counts( catch )
    
    
    names(preCatch2)[names(preCatch2) == 'Unmarked'] <- 'preUnmarked'    # Put this variable back to preUnmarked, to prepare for merge.
    test <- merge(catch2,preCatch2[,c('trapVisitID','FinalRun','lifeStage','forkLength','RandomSelection','Unassd','preUnmarked')],by=c('trapVisitID','FinalRun','lifeStage','forkLength','RandomSelection','Unassd'),all.x=TRUE)
    
    #   ---- Identify the problem records.  
    possiblyOff <- test[is.na(test$preUnmarked),]    # Possible trouble spots.
    test$preUnmarked[is.na(test$preUnmarked)] <- 0   # Unassigned fish that were not assigned to a certain category before, 
    # but were after -or- fish that were assigned a diff lifestage and 
    # finalrun between preCatch and catch.
    
    #   ---- Manipulations to deal with the problem records.  
    test$halfConeAssignedCatch   <- ifelse(test$halfConeID == 1 & test$Unassd != 'Unassigned',test$Unmarked - test$preUnmarked,0)    # halfConeAdj have to originate from a halfCone trapping instance.
    test$halfConeUnassignedCatch <- ifelse(test$halfConeID == 1 & test$Unassd == 'Unassigned',test$Unmarked - test$preUnmarked,0)    # halfConeAdj have to originate from a halfCone trapping instance.
    
    test$assignedCatch   <- ifelse(test$Unassd != 'Unassigned',test$Unmarked - test$halfConeAssignedCatch,0)
    test$unassignedCatch <- ifelse(test$Unassd == 'Unassigned',test$Unmarked - test$halfConeUnassignedCatch,0)
    
    #   ---- Deal with awkwardness of doing the plus-count routine two times with slightly different data (due to plus-counting).
    #                                    ,after 'times2' - before 'times2'
    test$off <- ifelse(test$halfConeID==2,test$Unmarked - test$preUnmarked,test$Unmarked - test$halfConeAssignedCatch - test$halfConeUnassignedCatch - test$preUnmarked)
    #                                    , should be zero
    
    test$preUnmarked <- ifelse(test$halfConeID == 2 & test$off != 0,test$Unmarked,test$preUnmarked)
    test$off2 <- ifelse(test$halfConeID==2,test$Unmarked - test$preUnmarked,test$Unmarked - test$halfConeAssignedCatch - test$halfConeUnassignedCatch - test$preUnmarked)
    
    #   ---- Helper code to look at results of manipulations.  This works for the RBDD.  
    #   look <- test[,c('trapVisitID','FinalRun','lifeStage','forkLength','RandomSelection','Unassd','trapPositionID','SampleDate','TrapStatus','TrapPosition','halfConeID','Unmarked','preUnmarked','halfConeAssignedCatch','halfConeUnassignedCatch','assignedCatch','unassignedCatch')]
    #   look[as.Date(look$SampleDate) == '2014-02-28' & look$TrapPosition == 'Gate 8',]
    
    #   ---- We're done!  Summarize the new catch, taking into account plusCounts.
    test$modUnassignedCatch <- test$halfConeUnassignedCatch + test$unassignedCatch
    test$modAssignedCatch <- test$halfConeAssignedCatch + test$assignedCatch
    
    test$off <- test$off2 <- NULL
    
    catch <- test
    
  } else {
    
    #   --- Data query where all are full catch - just set the fancy variables to zero.
    catch <- F.expand.plus.counts( catch )
    catch$preUnmarked <- catch$Unmarked
    if(nrow(catch) > 0){catch$halfConeAssignedCatch <- 0}
    if(nrow(catch) > 0){catch$halfConeUnassignedCatch <- 0}
    catch$assignedCatch   <- ifelse(catch$Unassd != 'Unassigned',catch$Unmarked - catch$halfConeAssignedCatch,0)
    catch$unassignedCatch <- ifelse(catch$Unassd == 'Unassigned',catch$Unmarked - catch$halfConeUnassignedCatch,0)
    catch$modUnassignedCatch <- catch$halfConeUnassignedCatch + catch$unassignedCatch
    catch$modAssignedCatch <- catch$halfConeAssignedCatch + catch$assignedCatch
  }
  
  #   ---- STEP 7:  Final clean-up.   
  #   ---- Reassign factor levels because they may have changed.  I.e., we may have eliminated "Unassigned."
  catch$FinalRun <- as.character( catch$FinalRun )
  catch$lifeStage <- as.character( catch$lifeStage )
  
  #   ---- Assign batch dates.  The ifs allow zero-row visits and catch dataframes to pass.
  if(nrow(visits) > 0){visits <- F.assign.batch.date( visits )}
  if(nrow(catch) > 0){catch <- F.assign.batch.date( catch )}
  
  #   ---- Assign attributes.  
  attr(catch, "siteID" ) <- site
  attr(catch, "site.name") <- catch$siteName[1]
  attr(catch, "subsites") <- unique(catch$trapPositionID)
  
  cat("First 20 records of catch data frame...\n")
  if( nrow(catch) >= 20 ) print( catch[1:20,] ) else print( catch )
  
  #   ---- Return two data frames via a list.  One contains positive catches, while
  #   ---- the other contains visit and fishing information.
  list( catch=catch, visit=visits )
  
}