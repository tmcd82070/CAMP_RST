#' @export
#' 
#' @title F.run.passage
#'   
#' @description Estimate production by life stage and run for all days within a
#'   date range.
#'   
#' @param site The identification number of the site for which estimates are 
#'   required.
#'   
#' @param taxon The species identifier indicating the type of fish of interest. 
#'   This is always \code{161980}; i.e., Chinook Salmon.
#'   
#' @param run The text seasonal identifier.  This is a one of \code{"Spring"},
#'   \code{"Fall"}, \code{"Late Fall"}, or \code{"Winter"}.
#'   
#' @param min.date The start date for data to include. This is a text string in 
#'   the format \code{\%Y-\%m-\%d}, or \code{YYYY-MM-DD}.
#'   
#' @param max.date The end date for data to include.  Same format as 
#'   \code{min.date}.
#'   
#' @param by A text string indicating the temporal unit over which daily 
#'   estimated catch is to be summarized.  Can be one of \code{day}, 
#'   \code{week}, \code{month}, \code{year}.
#'   
#' @param output.file A text string indicating a prefix to append to all output.
#' 
#' @param ci A logical indicating if 95\% bootstrapped confidence intervals 
#'   should be estimated along with passage estimates..  Default is \code{TRUE}.  
#'   
#' @param enhmodel A logical indicating if enhanced efficiency models should 
#'   be used to estimate trap efficiencies.  Default is \code{TRUE}.  
#'   
#' @return A \code{csv} table of passage estimates over the specified date 
#'   range, with runs across the columns.  A \code{csv} of daily passage 
#'   estimates for all traps operating at least one day, and catching at least 
#'   one fish, for all days within the specified date range. An additional 
#'   \code{csv} summarizing passage via the temporal unit specified via 
#'   \code{by}.  A \code{png} of catch versus time, for all inclusive traps. A
#'   \code{png} of daily efficiency estimates, and accompanying \code{csv} for 
#'   all traps operating at least one day, and catching at least one fish, for 
#'   all days within the specified time period.  Finally, for each run, a bar
#'   chart of passage summarizing catch over the time period specified via
#'   \code{by}.
#'   
#' @details Function \code{F.run.passage} is the main workhorse function for 
#'   estimating passage with respect to each of run and life stage.  As such, it
#'   calls several separate functions, some of which contain queries designed to
#'   run against an Access database.
#'   
#'   Generally, queries against a database comprise two main efforts.  The first
#'   involves a query for efficiency trial data, generally called "release" 
#'   data, and conducted via function \code{F.get.release.data}, while the 
#'   second queries for catch data via function \code{F.get.catch.data}.
#'   
#'   Once catch data are obtained, fish are partitioned as to whether or not 
#'   they were assigned and caught during a half-cone operation.  Function 
#'   \code{F.est.passage} wraps the functions that conduct the actual passage 
#'   estimation, which involves statistical fits of each of catch and efficiency
#'   over time.
#'   
#'   All calls to function \code{F.run.passage} result in daily passage 
#'   estimates, and courser temporal estimates, based on the value specified via
#'   \code{by}.  Regardless of the temporal partitioning, estimates are always 
#'   additionally summarized by year.  Function runs with \code{by} specified 
#'   as \code{year} output only one set of annual estimates.
#'   
#'   The difference between the specified \code{max.date} and \code{min.date}
#'   must be less than or equal to 366 days, as calculated via function
#'   \code{difftime}.
#'   
#'   Selection of \code{week} for input variable \code{by} results in weeks 
#'   displayed as customized Julian weeks, where weeks number 1-53.  The 
#'   specific mapping of days to weeks can be found within the "\code{Dates}" 
#'   table of any associated Access database.
#'   
#' @seealso \code{F.get.release.data}, \code{F.get.catch.data}
#'   
#' @examples  
#' \dontrun{
#' #   ---- Estimate passage on the American for the Fall run.
#' site <- 6000
#' taxon <- 161980
#' min.date <- "2010-12-07"
#' max.date <- "2011-06-02"
#' by <- "day"
#' output.file <- "Feather"
#' ci <- TRUE
#' }
F.run.passage <- function( site, taxon, min.date, max.date, by, output.file, ci=TRUE, enhmodel=TRUE ){
  
  # site <- 12345
  # taxon <- 161980
  # min.date <- "2005-01-01"
  # max.date <- "2005-06-30"
  # by <- "week"
  # output.file <- NA
  # ci <- TRUE
  # enhmodel <- TRUE
  
  #   ---- Make sure we have all temp tables.
  tableChecker()
  
  #   ---- Obtain necessary variables from the global environment.  
  fishingGapMinutes <- get("fishingGapMinutes",envir=.GlobalEnv)
  passageRounder <- get("passageRounder",envir=.GlobalEnv)
  
  #   Check that times are less than 1 year apart
  strt.dt <- as.POSIXct( min.date, format="%Y-%m-%d" )
  end.dt <- as.POSIXct( max.date, format="%Y-%m-%d" )
  run.season <- data.frame( start=strt.dt, end=end.dt )
  dt.len <- difftime(end.dt, strt.dt, units="days")
  if( dt.len > 366 )  stop("Cannot specify more than 365 days in F.passage. Check min.date and max.date.")
  
  #   ---- Check that taxon is Chinook salmon.  
  if( taxon != 161980 ) stop("Cannot specify any species other than Chinook salmon, code 161980.")
  
  #   ---- Identify the type of passage report we're doing
  # Utilize this construction to avoid NOTEs about assigning variables to the 
  # .GlobalEnv when running devtools::check().  
  pos <- 1
  envir <- as.environment(pos)
  assign("passReport","ALLRuns",envir=envir)
  passReport <- get("passReport",envir=.GlobalEnv)
  
  #   ---- Start a progress bar
  progbar <<- winProgressBar("Production estimate for ALL runs",
                             label=paste0("Fetching catch data, while using a ",round(fishingGapMinutes / 24 / 60,2),"-day fishing gap."),
                             width=1000)
  
  #   ---- Fetch the catch and visit data
  tmp.df   <- F.get.catch.data( site, taxon, min.date, max.date, output.file  )
  
  catch.df <- tmp.df$catch   # All positive catches, all FinalRun and lifeStages, inflated for plus counts.  Zero catches (visits without catch) are NOT here.
  visit.df <- tmp.df$visit   # the unique trap visits.  This will be used in a merge to get 0's later
  
  catch.dfX <- catch.df      # save for a small step below.  several dfs get named catch.df, so need to call this something else.
  
  if( is.null(catch.df) ){
    stop( paste0( "No catch records between ", min.date, " and ", max.date, ". Check dates and taxon."))
  }
  
  #   ---- Summarize catch data by trapVisitID X FinalRun X lifeStage. Upon return, catch.df has one line per combination of these variables
  catch.df0 <- F.summarize.fish.visit( catch.df, 'unassigned' )   # jason - 5/20/2015 - we summarize over lifeStage, wrt to unassigned.   10/2/2015 - i think by 'unassigned,' i really mean 'unmeasured'???
  catch.df1 <- F.summarize.fish.visit( catch.df, 'inflated' )     # jason - 4/14/2015 - we summarize over lifeStage, w/o regard to unassigned.  this is what has always been done.
  catch.df2 <- F.summarize.fish.visit( catch.df, 'assigned' )     # jason - 4/14/2015 - we summarize over assigned.  this is new, and necessary to break out by MEASURED, instead of CAUGHT.
  
  catch.df3 <- F.summarize.fish.visit( catch.df, 'halfConeAssignedCatch' )     # jason - 1/14/2016
  catch.df4 <- F.summarize.fish.visit( catch.df, 'halfConeUnassignedCatch' )   # jason - 1/14/2016
  catch.df5 <- F.summarize.fish.visit( catch.df, 'assignedCatch' )             # jason - 1/14/2016
  catch.df6 <- F.summarize.fish.visit( catch.df, 'unassignedCatch' )           # jason - 1/14/2016
  catch.df7 <- F.summarize.fish.visit( catch.df, 'modAssignedCatch' )          # jason - 1/14/2016
  catch.df8 <- F.summarize.fish.visit( catch.df, 'modUnassignedCatch' )        # jason - 1/14/2016
  
  #   ---- I calculate mean forklength here and attach via an attribute on visit.df.  This way, it gets into function 
  #   ---- F.get.release.data.enh.  Note I make no consideration of FinalRun, or anything else.  I get rid of plus 
  #   ---- count fish, and instances where forkLength wasn't measured.  Note that I do not restrict to RandomSelection == 
  #   ---- 'yes'.  Many times, if there are few fish in the trap, they'll just measure everything, and record a 
  #   ---- RandomSelection == 'no'.  
  catch.df2B <- catch.df[catch.df$Unassd != "Unassigned" & !is.na(catch.df$forkLength),]
  
  #   ---- Get the weighted-mean forkLength, weighting on the number of that length of fish caught.  Return a vector
  #   ---- of numeric values in millimeters, with entry names reflecting trapVisitIDs.  Also get the N for weighting. 
  flVec <- sapply(split(catch.df2B, catch.df2B$trapVisitID), function(x) weighted.mean(x$forkLength, w = x$Unmarked)) 
  flDF <- data.frame(trapVisitID=names(flVec),wmForkLength=flVec,stringsAsFactors=FALSE)
  nVec <- aggregate(catch.df2B$Unmarked,list(trapVisitID=catch.df2B$trapVisitID),sum)
  names(nVec)[names(nVec) == "x"] <- "nForkLength"
  tmp <- merge(flDF,nVec,by=c("trapVisitID"),all.x=TRUE)
  tmp <- tmp[order(as.integer(tmp$trapVisitID)),]
  attr(visit.df,"fl") <- tmp
  
  #   ---- Fetch efficiency data
  setWinProgressBar( progbar, 0.1 , label="Fetching efficiency data" )
  release.df <- F.get.release.data( site, taxon, min.date, max.date, visit.df )
  
  #   ---- For enh eff models, it is okay if we have zero rows in release.df.  But make a fake release.df so all 
  #   ---- the objects that depend on it have something to grab. 
  if(is.null(release.df)){
    if(enhmodel == TRUE){
      release.df <- makeFake_release.df(site,min.date,max.date,visit.df)
      if(is.null(release.df)){
        stop(paste0("No efficiency trials between ",min.date, " and ",max.date," in the current year, nor historically for this month and day. Check dates.\n"))
      }
    } else {
      stop( paste( "No efficiency trials between", min.date, "and", max.date, ". Check dates.\n"))
    }
  } else if(length(unique(visit.df$trapPositionID)[!(unique(visit.df$trapPositionID) %in% unique(release.df$trapPositionID))]) > 0){
    
    visit_but_no_release_traps <- unique(visit.df$trapPositionID)[!(unique(visit.df$trapPositionID) %in% unique(release.df$trapPositionID))]
    cat(paste0("I'm going to add in fake releases for trap(s) ",paste0(visit_but_no_release_traps,collapse=", "),".\n"))
    
    #   ---- Add in thisIsFake to what we do have already.  
    release.df$thisIsFake <- 0
    
    #   ---- If we're here, we have a visit for a trap that lacks efficiency trials, but a visit for a different 
    #   ---- trap that does have efficiency trials.  This means release.df is not null.  So we find the trap with 
    #   ---- a visit but no efficiency trial and make a fake trial.  
    for(trap in visit_but_no_release_traps){
      
      #   ---- I restrict visit.df to trap here to ensure that makeFake returns one record for the trap of interest.  
      #   ---- Only add a fake record if we need to.  
      release.df.fake <- makeFake_release.df(site,min.date,max.date,visit.df[visit.df$trapPositionID == trap,])
      if(!is.null(release.df.fake)){
        release.df <- rbind(release.df,release.df.fake)
      }
    }
  } else {
    release.df$thisIsFake <- rep(0,nrow(release.df))
  }
  
  # if( nrow(release.df) == 0 ){
  #   stop( paste( "No efficiency trials between", min.date, "and", max.date, ". Check dates."))
  # }
  
  #   ---- Compute the unique runs we need to do
  runs <- unique(c(catch.df1$FinalRun,catch.df2$FinalRun))    # get all instances over the two df.  jason change 4/17/2015 5/21/2015: don't think we need to worry about catch.df0.
  runs <- runs[ !is.na(runs) ]
  cat("\nRuns found between", min.date, "and", max.date, ":\n")
  print(runs)
  
  #   ---- Print the number of non-fishing periods
  cat( paste("\nNumber of non-fishing intervals at all traps:", sum(visit.df$TrapStatus == "Not fishing"), "\n\n"))
  
  #   ********
  #   Loop over runs
  ans <- lci <- uci <- matrix(0, 1, length(runs))#matrix(0, length(lstages), length(runs))
  dimnames(ans)<-list('All',runs)#list(lstages, runs)
  
  out.fn.roots <- NULL
  for( j in 1:length(runs) ){
    
    assign("run.name",runs[j],envir=envir)
    run.name <- get("run.name",envir=.GlobalEnv)
    
    # jason puts together the catches based on total, unassigned, assigned.
    assd <- catch.df2[catch.df2$Unassd != 'Unassigned' & catch.df2$FinalRun == run.name,c('trapVisitID','lifeStage','n.tot','mean.fl','sd.fl')]
    colnames(assd) <- c('trapVisitID','lifeStage','n.Orig','mean.fl.Orig','sd.fl.Orig')
    catch.dfA <- merge(catch.df1,assd,by=c('trapVisitID','lifeStage'),all.x=TRUE)
    unassd <- catch.df0[catch.df0$FinalRun == run.name,c('trapVisitID','lifeStage','n.tot')]
    colnames(unassd) <- c('trapVisitID','lifeStage','n.Unassd')
    
    # jason adds 6/7/2015 to throw out unassd counts from different runs that were creeping in.
    catch.small <- catch.dfX[catch.dfX$Unassd == 'Unassigned' & catch.dfX$FinalRun == run.name,c('trapVisitID','lifeStage','Unmarked','Unassd')]
    if(nrow(catch.small) > 0){
      catch.small.tot <- aggregate(catch.small$Unmarked,list(trapVisitID=catch.small$trapVisitID,lifeStage=catch.small$lifeStage),sum)
      names(catch.small.tot)[names(catch.small.tot) == 'x'] <- 'Unmarked'
      preunassd <- merge(unassd,catch.small.tot,by=c('trapVisitID','lifeStage'),all.x=TRUE)
      
      unassd <- preunassd[preunassd$n.Unassd == preunassd$Unmarked,]
      unassd$Unmarked <-  NULL
    }
    catch.df <- merge(catch.dfA,unassd,by=c('trapVisitID','lifeStage'),all.x=TRUE)
    
    # jason brings halfcone counts along for the ride 1/14/2016 -- only for run_passage, and not run lifestage?
    names(catch.df3)[names(catch.df3) == 'n.tot'] <- 'halfConeAssignedCatch'
    names(catch.df4)[names(catch.df4) == 'n.tot'] <- 'halfConeUnassignedCatch'
    names(catch.df5)[names(catch.df5) == 'n.tot'] <- 'assignedCatch'
    names(catch.df6)[names(catch.df6) == 'n.tot'] <- 'unassignedCatch'
    names(catch.df7)[names(catch.df7) == 'n.tot'] <- 'modAssignedCatch'
    names(catch.df8)[names(catch.df8) == 'n.tot'] <- 'modUnassignedCatch'
    
    catch.df <- merge(catch.df,catch.df3[,c('trapVisitID','lifeStage','FinalRun','halfConeAssignedCatch')],by=c('trapVisitID','lifeStage','FinalRun'),all.x=TRUE)
    catch.df <- merge(catch.df,catch.df4[,c('trapVisitID','lifeStage','FinalRun','halfConeUnassignedCatch')],by=c('trapVisitID','lifeStage','FinalRun'),all.x=TRUE)
    catch.df <- merge(catch.df,catch.df5[,c('trapVisitID','lifeStage','FinalRun','assignedCatch')],by=c('trapVisitID','lifeStage','FinalRun'),all.x=TRUE)
    catch.df <- merge(catch.df,catch.df6[,c('trapVisitID','lifeStage','FinalRun','unassignedCatch')],by=c('trapVisitID','lifeStage','FinalRun'),all.x=TRUE)
    catch.df <- merge(catch.df,catch.df7[,c('trapVisitID','lifeStage','FinalRun','modAssignedCatch')],by=c('trapVisitID','lifeStage','FinalRun'),all.x=TRUE)
    catch.df <- merge(catch.df,catch.df8[,c('trapVisitID','lifeStage','FinalRun','modUnassignedCatch')],by=c('trapVisitID','lifeStage','FinalRun'),all.x=TRUE)
    
    #theSumsBefore <<- accounting(catch.df,"byRun")
    
    catch.df <- catch.df[order(catch.df$trapPositionID,catch.df$batchDate),]
    
    cat(paste(rep("*",80), collapse=""))
    tmp.mess <- paste("Processing ", run.name)
    cat(paste("\n", tmp.mess, "\n"))
    cat(paste(rep("*",80), collapse=""))
    cat("\n\n")
    
    progbar <- winProgressBar( tmp.mess, label="Run processing", width=1000 )
    barinc <- 1 / (length(runs) * 6)
    assign( "progbar", progbar, pos=envir )
    
    indRun <- (catch.df$FinalRun == run.name ) & !is.na(catch.df$FinalRun)   # Don't need is.na clause.  FinalRun is never missing here.
    
    #   ---- If we caught this run, compute passage estimate.
    if( any( indRun ) ){   # 2/25/2016.  jason observes that this should probably check if we have at least one caught fish --- and not all zeros.
      
      # old - catch.df.ls <- catch.df[ indRun & indLS, c("trapVisitID", "FinalRun", "lifeStage", 'n.Orig','mean.fl.Orig','sd.fl.Orig',"n.tot", "mean.fl", "sd.fl","n.Unassd")]
      catch.df.ls <- catch.df[ indRun, c("trapVisitID", "FinalRun", "lifeStage",'n.Orig','mean.fl.Orig','sd.fl.Orig',"n.tot", "mean.fl", "sd.fl","n.Unassd",'halfConeAssignedCatch','halfConeUnassignedCatch','assignedCatch','unassignedCatch','modAssignedCatch','modUnassignedCatch')]
      
      #   ---- Merge in the visits to get zeros
      catch.df.ls <- merge( visit.df, catch.df.ls, by="trapVisitID", all.x=T )
      setWinProgressBar( progbar, getWinProgressBar(progbar)+barinc )
      
      #   ---- Update the constant variables.  Missing n.tot when trap was fishing should be 0.
      catch.df.ls$FinalRun[ is.na(catch.df.ls$FinalRun) ] <- run.name
      catch.df.ls$lifeStage <- "All"               # emulate passage behavior here
      catch.df.ls$n.tot[ is.na(catch.df.ls$n.tot) & (catch.df.ls$TrapStatus == "Fishing") ] <- 0
      catch.df.ls$n.Orig[ is.na(catch.df.ls$n.Orig) & (catch.df.ls$TrapStatus == "Fishing") ] <- 0
      catch.df.ls$n.Unassd[ is.na(catch.df.ls$n.Unassd) & (catch.df.ls$TrapStatus == "Fishing") ] <- 0
      
      catch.df.ls$halfConeAssignedCatch[ is.na(catch.df.ls$halfConeAssignedCatch) & (catch.df.ls$TrapStatus == "Fishing") ] <- 0
      catch.df.ls$halfConeUnassignedCatch[ is.na(catch.df.ls$halfConeUnassignedCatch) & (catch.df.ls$TrapStatus == "Fishing") ] <- 0
      catch.df.ls$assignedCatch[ is.na(catch.df.ls$assignedCatch) & (catch.df.ls$TrapStatus == "Fishing") ] <- 0
      catch.df.ls$unassignedCatch[ is.na(catch.df.ls$unassignedCatch) & (catch.df.ls$TrapStatus == "Fishing") ] <- 0
      catch.df.ls$modAssignedCatch[ is.na(catch.df.ls$modAssignedCatch) & (catch.df.ls$TrapStatus == "Fishing") ] <- 0
      catch.df.ls$modUnassignedCatch[ is.na(catch.df.ls$modUnassignedCatch) & (catch.df.ls$TrapStatus == "Fishing") ] <- 0
      
      #   ---- Update progress bar
      out.fn.root <- paste0(output.file, run.name)
      setWinProgressBar( progbar, getWinProgressBar(progbar)+barinc )
      
#       # jason add 2/25/2016 -- deal with traps with all zero fish.
#       # see if we have non-zero fish for a trap, given the lifestage and run.
#       theSums <- tapply(catch.df.ls[!is.na(catch.df.ls$n.Orig),]$n.Orig,list(catch.df.ls[!is.na(catch.df.ls$n.Orig),]$trapPositionID),FUN=sum)
#       theZeros <- names(theSums[theSums == 0])
#       catch.df.ls <- catch.df.ls[!(catch.df.ls$trapPositionID %in% theZeros),]
      
      #   ---- Set these attributes so they can be passed along.
      attr(catch.df.ls,"min.date") <- min.date
      attr(catch.df.ls,"max.date") <- max.date
      attr(catch.df.ls,"enhmodel") <- enhmodel
      #   ---- Compute passage
      if(by == 'year'){
        pass <- F.est.passage( catch.df.ls, release.df, "year", out.fn.root, ci )
        passby <- pass
      } else if(by != 'year'){
        pass <- F.est.passage( catch.df.ls, release.df, "year", out.fn.root, ci )
        passby <- F.est.passage( catch.df.ls, release.df, by, out.fn.root, ci )
      }
      
      #   ---- Update progress bar
      setWinProgressBar( progbar, getWinProgressBar(progbar)+barinc )
      out.fn.roots <- c(out.fn.roots, attr(pass, "out.fn.list"))
      
      #   ---- Save
      ans[ 1, j ] <- signif(round(pass$passage,0),passageRounder)
      lci[ 1, j ] <- signif(round(pass$lower.95,0),passageRounder)
      uci[ 1, j ] <- signif(round(pass$upper.95,0),passageRounder)
      setWinProgressBar( progbar, getWinProgressBar(progbar)+barinc )
      
      output.fn <- output.file
      
      #   ---- Write passage table to a file, if called for
      if( !is.na(output.fn) ){
        
        #   Fix up the pass table to pretty the output
        tmp.df <- passby
        
        if(by == 'week'){
          
          #   ---- Obtain Julian dates so days can be mapped to specialized Julian weeks. 
          db <- get( "db.file", envir=.GlobalEnv ) 
          ch <- odbcConnectAccess(db)
          JDates <- sqlFetch( ch, "Dates" )
          close(ch) 
          
          the.dates <- JDates[as.Date(JDates$uniqueDate) >= min.date & as.Date(JDates$uniqueDate) <= max.date,]
          
          the.dates <- the.dates[,c('year','julianWeek','julianWeekLabel')]
          the.dates$week <- paste0(the.dates$year,'-',formatC(the.dates$julianWeek, width=2, flag="0"))
          the.dates <- unique(the.dates)

          #   ---- A join on POSIX dates. 
          tmp.df <- merge(tmp.df,the.dates,by=c('week'),all.x=TRUE)
          tmp.df$week <- paste0(strftime(tmp.df$date,"%Y"),"-",tmp.df$julianWeek,": ",tmp.df$julianWeekLabel)
          tmp.df$year <- tmp.df$julianWeek <- tmp.df$julianWeekLabel <- NULL
          #tmp.df <- subset(tmp.df, select = -c(year,julianWeek,julianWeekLabel) )
        }
        
        tzn <- get("time.zone", .GlobalEnv )
        tmp.df$date <- as.POSIXct( strptime( format(tmp.df$date, "%Y-%m-%d"), "%Y-%m-%d", tz=tzn),tz=tzn)
        
        tmp.df$passage <- round(tmp.df$passage)
        tmp.df$lower.95 <- round(tmp.df$lower.95)
        tmp.df$upper.95 <- round(tmp.df$upper.95)
        tmp.df$meanForkLenMM <- round(tmp.df$meanForkLenMM,1)
        tmp.df$sdForkLenMM <- round(tmp.df$sdForkLenMM,2)
        tmp.df$pct.imputed.catch <- round(tmp.df$pct.imputed.catch, 3)
        tmp.df$sampleLengthHrs <- round(tmp.df$sampleLengthHrs,1)
        tmp.df$sampleLengthDays <- round(tmp.df$sampleLengthDays,2)
        if(enhmodel == TRUE){
          tmp.df$effModel <- "Enhanced"
        } else {
          tmp.df$effModel <- "Mark-Recapture"
        }
        names(tmp.df)[ names(tmp.df) == "pct.imputed.catch" ] <- "propImputedCatch"
        names(tmp.df)[ names(tmp.df) == "lower.95" ] <- "lower95pctCI"
        names(tmp.df)[ names(tmp.df) == "upper.95" ] <- "upper95pctCI"
        names(tmp.df)[ names(tmp.df) == "nForkLenMM" ] <- "numFishMeasured"
        
        if( by == "day" ){
          #   Merge in the trapsOperating column
          tO <- attr(passby, "trapsOperating")
          tmp.df <- merge( tmp.df, tO, by.x="date", by.y="batchDate", all.x=T )
          
          #   For aesthetics, change number fish measured on days in gaps from NA to 0
          tmp.df$numFishMeasured[ is.na(tmp.df$numFishMeasured) & (tmp.df$nTrapsOperating == 0) ] <- 0
        }
        
        #   Open file and write out header.
        out.pass.table <- paste(output.fn, paste0(run.name,"_passage_table.csv"), sep="")
        out.fn.roots <- c(out.fn.roots,out.pass.table)
        
        rs <- paste( format(run.season[1], "%d-%b-%Y"), "to", format(run.season[2], "%d-%b-%Y"))
        nms <- names(tmp.df)[1]
        for( i in 2:length(names(tmp.df))){
          if(by == 'day'){
            nms <- paste(nms, ",", names(tmp.df)[i], sep="")
          } else {
            if(i != 3){                                                # jason add:  put in this condition to make 'date' not print. doug doesnt like it.
              nms <- paste(nms, ",", names(tmp.df)[i], sep="")
            }
          }
        }
        
        if(by == 'day'){
          nms <- gsub('date,', '', nms)     # by == day results in a slightly different format for tmp.df than the other three.
        }
        
        cat(paste("Writing passage estimates to", out.pass.table, "\n"))
        
        sink(out.pass.table)
        cat(paste("Site=,", catch.df$siteName[1], "\n", sep=""))
        cat(paste("Site ID=,", catch.df$siteID[1], "\n", sep=""))
        cat(paste("Species ID=,", taxon, "\n", sep=""))
        cat(paste("Run =,", run.name, "\n", sep=""))
        cat(paste("Lifestage =,", catch.df.ls$lifeStage[1], "\n", sep=""))
        cat(paste("Summarized by=,", by, "\n", sep=""))
        cat(paste("Dates included=,", rs, "\n", sep=""))
        
        cat("\n")
        cat(nms)
        cat("\n")
        sink()
        
        tmp.df$date <- NULL                                              # jason add:  make sure the whole column of date doesnt print.
        
        #   Write out the table
        
        # task 2.4, 1/8/2016:  if passage = 0, force propImputedCatch to be zero.
        tmp.df$propImputedCatch <- ifelse(tmp.df$passage == 0,0,tmp.df$propImputedCatch)
        
        write.table( tmp.df, file=out.pass.table, sep=",", append=TRUE, row.names=FALSE, col.names=FALSE)
        
      }    # close out writing of passage table, if called for
    }   # close out passage estimate, of all types, for this run
    
    #   ---- Plot the final passage estimates
    if( by != "year" ){
      attr(passby,"summarized.by") <- by
      attr(passby, "species.name") <- "Chinook Salmon"
      attr(passby, "site.name") <- catch.df$siteName[1]
      attr(passby, "run.name" ) <- run.name#catch.df$FinalRun[1]
      attr(passby, "lifestage.name" ) <- "All lifestages"
      attr(passby, "min.date" ) <- min.date
      attr(passby, "max.date" ) <- max.date
      
      passby$passage <- round(passby$passage,0)   # task 2.4: 1/8/2016.  make the passage csv and barplot passage png agree on integer fish.
      out.f <- F.plot.passage( passby, out.file=output.fn )
      out.fn.roots <- c(out.fn.roots, out.f)
    }
    
    close(progbar)
  }    # close out everything having to do with the run
  
  cat("Final Run estimates:\n")
  print(ans)
  
  #   ---- compute percentages of each life stage
  ans.pct <- matrix( rowSums( ans ), byrow=T, ncol=ncol(ans), nrow=nrow(ans))
  ans.pct <- ans / ans.pct
  ans.pct[ is.na(ans.pct) ] <- NA
  
  #   ---- Write out the table
  df <- data.frame( dimnames(ans)[[1]], ans.pct[,1], ans[,1], lci[,1], uci[,1], stringsAsFactors=F )
  if( ncol(ans) > 1 ){
    #   We have more than one run
    for( j in 2:ncol(ans) ){
      df <- cbind( df, data.frame( ans.pct[,j], ans[,j], lci[,j], uci[,j], stringsAsFactors=F ))
    }
  }
  names(df) <- c("LifeStage", paste( rep(runs, each=4), rep( c(".propOfPassage",".passage",".lower95pctCI", ".upper95pctCI"), length(runs)), sep=""))
  
  #   ---- Append totals to bottom
  tots <- data.frame( "Total", matrix( colSums(df[,-1]), nrow=1), stringsAsFactors=F)
  names(tots) <- names(df)
  tots[,grep("lower.95", names(tots),fixed=T)] <- NA
  tots[,grep("upper.95", names(tots),fixed=T)] <- NA
  df <- rbind( df, Total=tots )
  df <- df[-1,]    # jason add
  
  if( !is.na(output.file) ){
    out.pass.table <- paste(output.file, "_run_passage_table.csv", sep="")
    rs <- paste( format(run.season[1], "%d-%b-%Y"), "to", format(run.season[2], "%d-%b-%Y"))
    nms <- names(df)[1]
    for( i in 2:length(names(df))) nms <- paste(nms, ",", names(df)[i], sep="")
    
    cat(paste("Writing passage estimates to", out.pass.table, "\n"))
    
    sink(out.pass.table)
    cat(paste("Site=,", catch.df$siteName[1], "\n", sep=""))
    cat(paste("Site ID=,", catch.df$siteID[1], "\n", sep=""))
    cat(paste("Species ID=,", taxon, "\n", sep=""))
    cat(paste("Dates included=,", rs, "\n", sep=""))
    
    cat("\n")
    cat(nms)
    cat("\n")
    sink()
    
    write.table( df, file=out.pass.table, sep=",", append=TRUE, row.names=FALSE, col.names=FALSE)
    out.fn.roots <- c(out.fn.roots, out.pass.table)
    
    ls.pass.df <- df
    
  }
  
  nf <- length(out.fn.roots)
  tableDeleter()
  
  #   ---- Write out message
  cat("SUCCESS - F.run.passage\n\n")
  cat(paste("Working directory:", getwd(), "\n"))
  cat(paste("R data frames saved in file:", "<none>", "\n\n"))
  nf <- length(out.fn.roots)
  cat(paste("Number of files created in working directory = ", nf, "\n"))
  for(i in 1:length(out.fn.roots)){
    cat(paste(out.fn.roots[i], "\n", sep=""))
  }
  cat("\n")
  
  df
}