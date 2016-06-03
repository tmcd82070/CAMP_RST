#' @export
#' 
#' @title F.est.passage - Estimate passage
#' 
#' @description Compute passage estimates, given catch and efficiency trial data.  
#' 
#' @param catch.df A data frame with one row per \code{trapvisitID} for a
#'   particular \code{FinalRun} and \code{lifeStage}.  That is, \code{catch.df}
#'   has only one (of what could be many) run-by-life-stage combinations.
#' @param release.df A data frame resulting from a call to function
#'   \code{F.get.release.data}.  Contains efficiency data.
#' @param summarize.by A text string indicating the temporal unit over which
#'   daily estimated catch is to be summarized.  Can be one of \code{day}, 
#'   \code{week}, \code{month}, \code{year}.
#' @param file.root  A text string indicating a prefix to append to all output.
#' @param ci A logical indicating if 95% bootstrapped confidence intervals
#'   should be estimated along with passage estimates.
#'   
#' @return A data frame containing daily passage estimates and associated
#'   standard errors.
#'   
#' @details Estimation of passage is comprised of two main steps.  The first 
#'   fetches and formats all the necessary data.  The second performs 
#'   statistical analysis on those processed data.  Function 
#'   \code{F.est.passage} is the workhorse function for all statistical 
#'   analysis.  As such, it calls functions responsible for catch modeling
#'   (\code{F.est.catch}), efficiency modeling (\code{F.est.efficiency}), and
#'   bootstrapping (\code{F.bootstrap.passage}).
#'   
#'   Function calls resulting in non-zero catch, but zero efficiency, due to no
#'   valid efficiency trials, result in warnings of zero efficiency.  The 
#'   function will continue, but all passage estimates will be \code{NA}.
#'   
#'   Function \code{F.est.passage} brings together catch and efficiency data. 
#'   Called the 'grand merge,' resulting data frame \code{grand.df} forms the 
#'   basis of all passage estimation.  Merging takes places on unique
#'   combinations of \code{trapPositionID} and \code{batchDate}.  Trap matches
#'   respect decimal suffixes appended due to gaps in fishing.
#'   
#'   In processing prior to the creation of the \code{grand.df}, the dates 
#'   outside the first and last date are dropped from each trap. In reality 
#'   however, the season for each trap is identified as non missing catch.  In 
#'   other words, the grand merge inserts every date for all traps because the 
#'   underlying efficiency data frame has all dates.
#'   
#'   Function \code{F.bootstrap.passage} summarizes the daily passage estimates 
#'   housed in \code{grand.df} to the temporal units specified via 
#'   \code{summarize.by}, and then compiles all statistics for eventual
#'   reporting.  Statistics include weighted mean forklength, standard deviation
#'   of forklength, and fish n.
#' 
#' @section Fish Accounting:  
#' Passage estimation results in the partitioning of 
#'   fish into different groups. For example, a fish could be assigned/not 
#'   assigned, measured/not measured, half-cone/full-cone, plus-count, imputed, 
#'   or inflated (define these?). Function \code{F.est.passage} organizes all of
#'   these different types of fish following their initial partitioning in
#'   function \code{F.get.catch.data}. Fish accounting on a daily basis ensures 
#'   that the counts of these different types of fish collapse back to their 
#'   original totals following analytic processing.  Said another way, fish 
#'   accounting ensures that no fish are mysteriously gained or lost during the 
#'   passage estimation process.
#'   
#'   Three types of daily checks are performed for each individual trap.
#'   
#'   Do we need to define these? 
#'   
#'   \enumerate{
#'     \item{eqn{totalCatch =  assignedCatch + unassignedCatch + imputedCatch}}
#'     \item{eqn{inflatedCatch =  assignedCatch + unassignedCatch}}
#'     \item{eqn{totalCatch =  inflatedCatch + imputedCatch}}
#'   }
#'   
#'   
#' 
#' @seealso \code{F.get.release.data}, \code{F.get.catch.data}, \code{F.est.passage}
#' 
#' @examples 
#' # query mdb?
F.est.passage <- function( catch.df, release.df, summarize.by, file.root, ci ){

  # catch.df <- catch.df.ls
  # release.df <- release.df
  # summarize.by <- by
  # file.root <- out.fn.root
  # ci <- ci
  
  #   ---- Obtain text description of trap positions for use in output. 
  catch.df.sites <- unique(catch.df[,c('trapPositionID','TrapPosition')])
  colnames(catch.df.sites) <- c('subSiteID','subSiteName') 
  
  #   ---- Obtain Julian weeks once and for all and place in Global environment for ease. 
  if( summarize.by == "week" ){
    db <- get( "db.file", env=.GlobalEnv )
    ch <- odbcConnectAccess(db)
    the.dates <<- sqlFetch( ch, "Dates" )
    close(ch)
  }
  
  time.zone <- get("time.zone", env=.GlobalEnv )
  
  f.banner <- function( x ){
      cat("\n")
      cat(paste(rep("=",50), collapse=""));
      cat(x);
      cat(paste(rep("=",50), collapse=""));
      cat("\n")
  }
  
  f.banner(" F.est.passage - START ")
  
  #   ---- Keep track of produced files.
  out.fn.list <- NULL
  
  #   ---- Retrieve the progress bar.
  usepb <- exists( "progbar", where=.GlobalEnv )

  #   ---- Need to collapse over lifeStage, in light of GitHub Issue #73.  Ideally, this would occur in the very
  #   ---- beginning, but Unassigned counts are subsumed in run + lifeStage combinations in summarize_fish_visit.
  #   ---- So collapse over lifeStage here.  Need to do this before we merge immediately below; otherwise, they break.
  
  #   ---- Helper function to collapse fish counts.
  collapseEm <- function(var){
    
    # var <- 'halfConeUnassignedCatch'
    
    temp <- data.frame(temp=tapply(catch.df[!is.na(catch.df[,var]),][,var],list(catch.df[!is.na(catch.df[,var]),]$trapVisitID),FUN=sum))
    colnames(temp)[colnames(temp) == 'temp'] <- var
    temp
  }
  
  #   ---- If we're running over runs, collapse counts over lifeStage.
  if(passReport == 'ALLRuns'){
    v1  <- collapseEm('n.tot')
    v2  <- collapseEm('n.Orig')
    v3  <- collapseEm('n.Unassd')
    v4  <- collapseEm('halfConeAssignedCatch')
    v5  <- collapseEm('halfConeUnassignedCatch')
    v6  <- collapseEm('assignedCatch')
    v7  <- collapseEm('unassignedCatch')
    v8  <- collapseEm('modAssignedCatch')
    v9  <- collapseEm('modUnassignedCatch')

    vv <- cbind(v1,v2,v3,v4,v5,v6,v7,v8,v9)
  
    df3b <- vv
    df3b$trapVisitID <- rownames(df3b)
    df3c <- unique(catch.df[,c("trapVisitID","trapPositionID","EndTime","ProjID","batchDate","StartTime","SampleMinutes","TrapStatus","siteID","siteName","oldtrapPositionID","TrapPosition","sampleGearID","sampleGear","halfConeID","HalfCone","includeCatchID","FinalRun","lifeStage")])
    df3d <- merge(df3c,df3b,by=c('trapVisitID'),all.x=TRUE)
    df3d <- df3d[order(df3d$EndTime),]
  
    #   ---- Keep the original, because it has mean and sd info over all the fish.  
    #   ---- We're not amending those data to fix the collapsing over lifestage issue here.
    catch.df.old <- catch.df    
    catch.df <- df3d
  }

  #   ---- Data frame catch.df has the raw unmarked counts of catch.  Note that rows with missing data for
  #   ---- certain days, i.e., for which imputation occurs, also appear as line items here.  So, to get catch, 
  #   ---- for different trapPositionIDs/subSiteIDs, summarise and add togeter (because some days have more 
  #   ---- than one record).  This brings back more dates than ultimately wanted; let merge below (after 
  #   ---- grand.df) take care of which to keep.
  jason.catch2.df <- catch.df[,c('trapVisitID','batchDate','trapPositionID','n.Orig')]
  jason.catch3.df <- data.frame(with(jason.catch2.df,tapply(n.Orig, list(batchDate,trapPositionID), sum, na.rm=T )))
  jason.catch4.df <- na.omit(reshape(jason.catch3.df,idvar='batchDate',ids=row.names(jason.catch3.df),times=names(jason.catch3.df),timevar='trapPositionID',varying=list(names(jason.catch3.df)),direction='long'))
  colnames(jason.catch4.df)[2] <- 'rawCatch'
  jason.catch4.df$trapPositionID <- as.character(substr(jason.catch4.df$trapPositionID,2,nchar(jason.catch4.df$trapPositionID)))
  jason.catch4.df$batchDate <- as.POSIXct(jason.catch4.df$batchDate,time.zone)
  
  #   ---- Do the same as above, but with n.tot.  Sloppy to do this twice like this, but it works.
  jason.totCatch2.df <- catch.df[,c('trapVisitID','batchDate','trapPositionID','n.tot')]
  jason.totCatch3.df <- data.frame(with(jason.totCatch2.df,tapply(n.tot, list(batchDate,trapPositionID), sum, na.rm=T )))
  jason.totCatch4.df <- na.omit(reshape(jason.totCatch3.df,idvar='batchDate',ids=row.names(jason.totCatch3.df),times=names(jason.totCatch3.df),timevar='trapPositionID',varying=list(names(jason.totCatch3.df)),direction='long'))
  colnames(jason.totCatch4.df)[2] <- 'n.tot'
  jason.totCatch4.df$trapPositionID <- as.character(substr(jason.totCatch4.df$trapPositionID,2,nchar(jason.totCatch4.df$trapPositionID)))
  jason.totCatch4.df$batchDate <- as.POSIXct(jason.totCatch4.df$batchDate,time.zone)

  #   ---- Estimate capture for every day of season.  Return value is
  #   ---- a data frame with columns $batchDate and $catch.
  #   ---- By default, this produces a catch graph in a png.  Turn 
  #   ---- this off with plot=FALSE in call. Resulting list 
  #   ---- catch.and.fits has components $catch, $fits, $X.miss, $gaps, 
  #   ---- $bDates.miss, $trapsOperating, true.imp, and allDates.
  catch.and.fits <- F.est.catch( catch.df, plot=TRUE, plot.file=file.root )
  if(usepb){
    progbar <- get( "progbar", pos=.GlobalEnv )
    tmp <- getWinProgressBar(progbar)
    setWinProgressBar(progbar, (2*tmp + 1)/3 )
  }
  
  #   ---- Note this doesn't have imputedCatch.
  catch <- catch.and.fits$catch  

  #   ---- The catch data frame in this list has imputed values 
  #   ---- already overwriting the original numbers.  This happens
  #   ---- in F.catch.model.  This step incorporates the imputed 
  #   ---- values into the analysis for fish accounting and 
  #   ---- eventual accounting.  
  jason.catch.and.fits2.df <- catch.and.fits$true.imp
  jason.catch.and.fits3.df <- data.frame(with(jason.catch.and.fits2.df,tapply(n.tot, list(batchDate,trapPositionID), sum, na.rm=T )))
  jason.catch.and.fits4.df <- na.omit(reshape(jason.catch.and.fits3.df,idvar='batchDate',ids=row.names(jason.catch.and.fits3.df),times=names(jason.catch.and.fits3.df),timevar='trapPositionID',varying=list(names(jason.catch.and.fits3.df)),direction='long'))
  colnames(jason.catch.and.fits4.df)[2] <- 'imputedCatch'
  jason.catch.and.fits4.df$trapPositionID <- as.character(substr(jason.catch.and.fits4.df$trapPositionID,2,nchar(jason.catch.and.fits4.df$trapPositionID)))
  jason.catch.and.fits4.df$batchDate <- as.POSIXct(jason.catch.and.fits4.df$batchDate,time.zone)
  
  out.fn.list <- c(out.fn.list, attr(catch.and.fits, "out.fn.list"))
  
  #   ---- Data frame release.df has info on adjusted beginning
  #   ---- and end fishing days, for each trap.  Note that 
  #   ---- release.df doesn't have decimal expansion, due to 
  #   ---- incorporation of gap in fishing.  
  allDates <- catch.and.fits$allDates
  allDates$trap <- round(allDates$trap,0)     
  release.df <- merge(release.df,allDates,by.x=c('trapPositionID'),by.y=c('trap'),all.x=TRUE)
  
  f.banner("Efficiency estimation ")
  
  #   ---- Get all the batchDates for use in efficiency.
  bd <- strptime(sort( seq(as.Date(min(na.omit(release.df$ReleaseDate),na.omit(release.df$origBeg.date),unique(catch$batchDate))),as.Date(max(na.omit(release.df$ReleaseDate),na.omit(release.df$origEnd.date),unique(catch$batchDate))),"days")),format="%F",tz=time.zone)

  #   ---- Estimate capture for every day of season.  
  eff.and.fits <- F.est.efficiency( release.df, bd, method=3, df=3, plot=TRUE, plot.file=file.root )
  if(usepb){
    tmp <- getWinProgressBar(progbar)
    setWinProgressBar(progbar, (2*tmp + 1)/3 )
  }
  efficiency <- eff.and.fits$eff

  out.fn.list <- c(out.fn.list, attr(eff.and.fits, "out.fn.list"))
  
  #   ---- Something is wrong with efficiency data. Make an empty efficiency data frame
  if( all(is.na(efficiency[1,])) ){
    efficiency <- data.frame( trapPositionID=catch$trapPositionID, batchDate=catch$batchDate, efficiency=rep(NA, nrow(catch)))
    warning("Zero efficiency")
  }

  #   ---- Could do...
  # n <- data.base( catch, efficiency=efficiency$efficiency, gam.estimated.eff=efficiency$gam.estimated )
  #   ---- ...to produce a data frame of values that go into estimator, one line per batchDate.

  #   ---- Now, estimate passage.  It shouldn't happen that efficiency <= 0, 
  #   ---- but just in case.  This also gives us a way to exclude days -- 
  #   ---- just set efficiency <= 0.
  if( any(ind <- !is.na(efficiency$efficiency) & (efficiency$efficiency <= 0)) ){    
    efficiency$efficiency[ind] <- NA
  }

  #   ---- First merge catch and efficiency data frames
  catch$batchDay <- format(catch$batchDate, "%Y-%m-%d")
  catch$trapPositionID <- as.character(catch$trapPositionID)
  efficiency$batchDay <- format(efficiency$batchDate, "%Y-%m-%d")
  efficiency$trapPositionID <- as.character(efficiency$trapPositionID)
  
  #   ---- Drop POSIX date from efficiency.
  efficiency <- efficiency[,names(efficiency) != "batchDate"]  

  cat("First 20 rows of CATCH...\n")
  print(catch[1:20,])
  cat("First 20 rows of EFFICIENCY...\n")
  print(efficiency[1:20,])

  #   ---- To ensure that trapPositionIDs with decimals find their efficiency trial trap match,
  #   ---- ensure we have the old IDs -- otherwise, these won't ever be found.
  catch$oldTrapPositionID <- as.character(round(as.numeric(catch$trapPositionID),0))
  names(efficiency)[names(efficiency) == 'trapPositionID'] <- 'oldTrapPositionID'
  
  #   ---- The Grand Merge.  Merge catch info with efficiency info.
  grand.df <- merge( catch, efficiency, by=c("oldTrapPositionID", "batchDay"), all=T)
  
  #   ---- Get rid of helper variable oldTrapPositionID;  it has served its purpose.  
  grand.df$oldTrapPositionID <- NULL
  
  #   ---- For each trap, drop the dates that are outside its min. and max.date.  
  #   ---- The season for each trap is identified as non missing catch.  I.e., the 
  #   ---- grand merge puts in every date because efficiency data frame has all dates.
  grand.df <- grand.df[!is.na(grand.df$catch), ]
  
  #   ---- Bring in raw catch (measured).
  grand.df.rawCatch <- merge(grand.df,jason.catch4.df,by=c('trapPositionID','batchDate'),all.x=TRUE)       
  
  #   ---- Bring in inflated catch (measured + plus counts).  
  grand.df.rawCatch.Inflated <- merge(grand.df.rawCatch,jason.totCatch4.df,by=c('trapPositionID','batchDate'),all.x=TRUE)
  
  #   ---- Bring in imputed catch.  
  grand.df.rawCatch.Imputed <- merge(grand.df.rawCatch.Inflated ,jason.catch.and.fits4.df,by=c('trapPositionID','batchDate'),all.x=TRUE) 
  grand.df <- grand.df.rawCatch.Imputed

  #   ---- Somewhere, there are comments that state that catches of NA mean zero.  So, 
  #   ---- replace NA in each of rawCatch and ImputedCatch with zero.
  grand.df$imputedCatch <- ifelse(is.na(grand.df$imputedCatch), 0, round(grand.df$imputedCatch,1))
  grand.df$rawCatch <- ifelse(is.na(grand.df$rawCatch), 0, grand.df$rawCatch)
  grand.df$n.tot <- ifelse(is.na(grand.df$n.tot), 0, grand.df$n.tot)  # the preTotalCatch
  grand.df$totalEstimatedCatch <- round(grand.df$n.tot + grand.df$imputedCatch,1)
  grand.df$rawCatch <- grand.df$catch <- NULL

  #   ---- Fish accounting.  
  #   ---- Check and make sure that assignedCatch + unassignedCatch + imputedCatch = totalCatch.
  #   ---- Check and make sure that assignedCatch + unassignedCatch = inflatedCatch.
  #   ---- Check and make sure that inflatedCatch + imputedCatch = totalCatch.
  grand.df$sum1 <- grand.df$modAssignedCatch + grand.df$modUnassignedCatch + grand.df$imputedCatch
  grand.df$sum2 <- grand.df$modAssignedCatch + grand.df$modUnassignedCatch
  grand.df$sum3 <- grand.df$halfConeAssignedCatch + grand.df$halfConeUnassignedCatch + grand.df$assignedCatch + grand.df$unassignedCatch + grand.df$imputedCatch
  grand.df$check1 <- ifelse(grand.df$sum1 == grand.df$totalEstimatedCatch,TRUE,FALSE)
  grand.df$check2 <- ifelse(grand.df$sum2 == grand.df$n.tot,TRUE,FALSE)
  grand.df$check3 <- ifelse(grand.df$sum3 == grand.df$totalEstimatedCatch,TRUE,FALSE)

  if(sum(grand.df$check1 + grand.df$check2 + grand.df$check3) != nrow(grand.df)*3){
    stop('Issue with summation of assignedCatch, unassignedCatch, inflatedCatch, imputedCatch, and/or totalCatch.  Investigate est_passage.R, around line 176.')
  } else {
    cat('No issue with summation of halfConeAssignedCatch, halfConeUnassignedCatch, assignedCatch, unassignedCatch, modAssignedCatch, modUnassignedCatch, imputedCatch, and/or totalEstimatedCatch.  Continuing...\n')
  }

  #   ---- The passage estimator.
  grand.df$passage <- rep(NA, nrow(grand.df))
  grand.df$passage <- grand.df$totalEstimatedCatch / grand.df$efficiency   #ifelse(!is.na(grand.df$efficiency),grand.df$totalEstimatedCatch / grand.df$efficiency,0)

  if( !is.na(file.root) ){
    
    #   ---- Do this so can change names (headers) in csv file; i.e., drop 2 columns.
    tmp.df <- grand.df[, !(names(grand.df) %in% c("nReleased", "nCaught", "batchDay")) ] 
    names(tmp.df)[ names(tmp.df) == "imputed.catch" ] <- "propImputedCatch"
    names(tmp.df)[ names(tmp.df) == "imputed.eff" ] <- "propImputedEff"
    
    #   ---- Convert to numbers, 0 or 1.
    tmp.df$propImputedEff <- as.numeric(tmp.df$propImputedEff)  
    tmp.df$passage <- round(tmp.df$passage)  
    tmp.df$totalCatch <- round(tmp.df$totalEstimatedCatch,1)
    tmp.df$efficiency <- round(tmp.df$efficiency, 4)
    tmp.df$halfConeAdj <- tmp.df$halfConeAssignedCatch + tmp.df$halfConeUnassignedCatch

    #   ---- Merge in subsiteNames.
    ssiteNames <- catch.df.sites           
    tmp.df <- merge( ssiteNames, tmp.df, by.x="subSiteID", by.y="trapPositionID", all.y=T )
    out.fn <- paste(file.root, "_baseTable.csv", sep="")
    tmp.df$TrapPosition <- tmp.df$TrapPositionID <- NULL

    #   ---- Rearrange columns.
    tmp.df <- tmp.df[c('subSiteID','subSiteName','batchDate','assignedCatch','unassignedCatch','halfConeAdj','imputedCatch','totalEstimatedCatch','propImputedCatch','efficiency','propImputedEff','passage')]
    tmp.df <- tmp.df[order(tmp.df$subSiteID,tmp.df$batchDate),] 

    write.table( tmp.df, file=out.fn, sep=",", row.names=FALSE, col.names=TRUE)
    out.fn.list <- c(out.fn.list, out.fn)
  }

  # ====== Passage estimates are done by day.  Compute variance and summarize ====================================================================================================
  f.banner(paste(" Bootstrapping, if called for, and summarizing by", summarize.by))

  #   ---- Summarization (to weeks, years, etc.) needs to happen in the bootstrapping routine.
  #   ---- Even if bootstraps are not called for, F.bootstrap averages over traps (if multiple 
  #   ---- present) and summarizes by 'summarize.by'.
  n <- F.bootstrap.passage( grand.df, catch.and.fits$fits, catch.and.fits$X.miss, catch.and.fits$gaps,
                catch.and.fits$bDates.miss, eff.and.fits$fits, eff.and.fits$X, eff.and.fits$ind.inside,
                eff.and.fits$X.dates, summarize.by, 100, ci )
  
  if(usepb){
    tmp <- getWinProgressBar(progbar)
    setWinProgressBar(progbar, tmp + (1-tmp)*.9 )
  }

  #   ---- Grab the correct catch.df for use in summarizing.
  if(passReport == 'ALLRuns'){
    index.aux <- F.summarize.index( catch.df.old$batchDate, summarize.by )
  } else {  # by lifeStage
    index.aux <- F.summarize.index( catch.df$batchDate, summarize.by )
  }

  #   ---- Force summarize.index and bootstrap passage to have the same year. 
  if(summarize.by == 'year'){
    n[1,1] <- index.aux[[1]][1]
  }

  #   ---- Grab the correct catch.df for use in summarizing.
  if(passReport == 'ALLRuns'){

    #   ---- Calculate numerator (weighted) mean forklength.
    num <- catch.df.old$mean.fl.Orig * catch.df.old$n.Orig
    num <- tapply( num, index.aux, sum, na.rm=T )
  
    #   ---- Calcualte numerator standard deviation of forklength.
    #   ---- This is sum of squares without the summing just yet.
    num.sd <- (catch.df.old$sd.fl.Orig * catch.df.old$sd.fl.Orig) * (catch.df.old$n.Orig  - 1)    
    num.sd <- tapply( num.sd, index.aux, sum, na.rm=T )

    #   ---- Calculate n.  
    den <- tapply( catch.df.old$n.Orig, index.aux, sum, na.rm=T)
    
  #   ---- Estimate by lifeStage.
  } else {  
  
    #   ---- Calculate numerator (weighted) mean forklength.  
    num <- catch.df$mean.fl.Orig * catch.df$n.Orig
    num <- tapply( num, index.aux, sum, na.rm=T )

    #   ---- Calculate numerator standard deviation of forklength.  
    #   ---- This is sum of squares without the summing just yet.    
    num.sd <- (catch.df$sd.fl.Orig * catch.df$sd.fl.Orig) * (catch.df$n.Orig  - 1)    
    num.sd <- tapply( num.sd, index.aux, sum, na.rm=T )

    #   ---- Calculate n.
    den <- tapply( catch.df$n.Orig, index.aux, sum, na.rm=T)
  }

  #   ---- Mean and standard deviation computations.  
  aux.fl <- ifelse( den > 0, num / den, NA )
  aux.sd <- ifelse( den > 1, sqrt(num.sd / (den-1)), NA )

  #   ---- Grab the correct catch.df for use in summarizing.
  if(passReport == 'ALLRuns'){
    
    #   ---- Reduce data frame to select first of each and change to batchdate.
    catch.df.reduced <- aggregate(catch.df.old,by=list(ID=catch.df.old$batchDate),head,1)
  #   ---- By lifeStage.  
  } else {      
    #   ---- Reduce data frame.  Possibly due to multiple records over lifestage in run estimates. 
    catch.df.reduced <- aggregate(catch.df,by=list(ID=catch.df$batchDate),head,1)
  }

  catch.df.Fishing <- catch.df
  catch.df.Fishing$SampleMinutes <- ifelse(catch.df.Fishing$TrapStatus == 'Not fishing',0,catch.df.Fishing$SampleMinutes)
  catch.df.Fishing <- unique(catch.df.Fishing[,c('SampleMinutes','batchDate','trapPositionID')])
  num <-  aggregate(catch.df.Fishing$SampleMinutes,by=list(ID=catch.df.Fishing$batchDate),sum)[,2]

  #   ---- Variable batchDate defaults to Mountain Time.  Fix that.
  tzn <- get("time.zone", .GlobalEnv )                                                   
  catch.df.reduced$batchDate <- as.POSIXct( strptime( format(catch.df.reduced$batchDate, "%Y-%m-%d"), "%Y-%m-%d", tz=tzn),tz=tzn)

  #   ---- Index in reduced data frame.  
  index.aux <- F.summarize.index(catch.df.reduced$batchDate,summarize.by)               

  #   ---- Hours actually sampled during the 'index' period.
  aux.hrs <- tapply( num, index.aux, sum, na.rm=T )/60                                  

  #   ---- Make big data frame of statistics.  
  aux<-data.frame( s.by=dimnames(aux.fl)[[1]],
    nForkLenMM=c(den),
    meanForkLenMM=c(aux.fl),
    sdForkLenMM=c(aux.sd),
    sampleLengthHrs=c(aux.hrs),
    stringsAsFactors=F, row.names=NULL )

  #   ---- Merge 'n' and 'aux' information together. 
  n <- merge(n,aux, by="s.by", all.x=T)
  n$sampleLengthDays <- n$sampleLengthHrs / 24

  #   ---- Possibly only works west of GMT (North America).  East of GMT, 
  #   ---- it may be 12 hours off.  Untested east of GMT.  
  tz.offset <- as.numeric(as.POSIXct(0, origin="1970-01-01", tz=time.zone))
  n$date <- as.POSIXct( n$date-tz.offset, origin="1970-01-01", tz=time.zone )  

  #   ---- Put the final data frame together.  
  names(n)[names(n) == "s.by"] <- summarize.by

  attr(n, "taxonID" ) <- attr(catch.df,"taxonID")
  attr(n, "species.name") <- attr(catch.df, "species.name")
  attr(n, "siteID" ) <- attr(catch.df,"siteID")
  attr(n, "site.name") <- attr(catch.df, "site.name")
  attr(n, "site.abbr") <- attr(catch.df, "site.abbr")
  attr(n, "runID") <- attr(catch.df, "runID")
  attr(n, "run.name") <- attr(catch.df, "run.name")
  attr(n, "year") <- attr(catch.df, "year")
  attr(n, "run.season") <- attr(catch.df, "run.season")
  attr(n, "summarized.by") <- summarize.by
  attr(n, "out.fn.list") <- out.fn.list
  attr(n, "trapsOperating") <- catch.and.fits$trapsOperating

  f.banner(" F.est.passage - COMPLETE ")

  n

}