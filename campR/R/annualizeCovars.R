#' @export
#' 
#' @title annualizeCovars
#'   
#' @description Estimate available covariates annually per site.
#'   
#' @param site The identification number of the site for which estimates are 
#'   required.
#'   
#' @param taxon The species identifier indicating the type of fish of interest. 
#'   This is always \code{161980}; i.e., Chinook Salmon.
#'   
#' @param min.date The start date for data to include. This is a text string in 
#'   the format \code{\%Y-\%m-\%d}, or \code{YYYY-MM-DD}.
#'   
#' @param max.date The end date for data to include.  Same format as 
#'   \code{min.date}.
#'   
#' @param season Typically a year related to the timeframe specified by 
#'   \code{min.date} and \code{max.date}.
#'   
#' @return If successful, an updated data file in the thing.
#'   
#' @details Sites and "annual" are defined via the so-called 
#'   \code{"TheExcel.csv"}, the listing of which forms the loops of the Big 
#'   Looper.  Annual here corresponds to \code{"Season"}, although one year (or 
#'   "annual" time period) only ever contains one Season.
#'   
annualizeCovars <- function(site,min.date,max.date,season,taxon){
  
  # site <- site
  # min.date <- min.date
  # max.date <- max.date
  # season <- theSeason

  #   ---- Obtain necessary variables from the global environment.  
  time.zone <- get("time.zone",envir=.GlobalEnv)
  db.file <- get( "db.file", envir=.GlobalEnv )
  
  #   ---- Connect to database. 
  ch <- RODBC::odbcConnectAccess(db.file)
  
  #   ---- Develop the TempReportCriteria_TrapVisit table.
  F.buildReportCriteria( site, min.date, max.date )
    
  #   ---- Run the clean-up query.
  F.run.sqlFile( ch, "QryCleanEnvCov.sql" )
    
  #   ---- Now, fetch the result.  
  dbCov <- RODBC::sqlFetch( ch, "EnvDataRaw_Standardized" )
  
  #   ---- While we're in here, get mapping if sites to subSites.
  sitesXwalk <- RODBC::sqlQuery( ch, "SELECT siteID, subSiteID FROM SubSite;" )
    
  close(ch)
    
  #   ---- Calculate the average for each variable from CAMP mdbs.  
  avg <- data.frame(site=site,
                    Season=season,
                    discharge_cfs=mean(na.omit(dbCov$discharge)),
                    waterDepth_cm=mean(na.omit(dbCov$waterDepth)),
                    waterVel_fts=mean(na.omit(dbCov$waterVel)),
                    airTemp_F=mean(na.omit(dbCov$airTemp)),
                    waterTemp_C=mean(na.omit(dbCov$waterTemp)),
                    lightPenetration_cm=mean(na.omit(dbCov$lightPenetration)),
                    turbidity_ntu=mean(na.omit(dbCov$turbidity)),
                    dissolvedOxygen_mgL=mean(na.omit(dbCov$dissolvedOxygen)),
                    conductivity_mgL=mean(na.omit(dbCov$conductivity)),
                    barometer_inHg=mean(na.omit(dbCov$barometer)))

  
  #   ---- We assemble all the unique ourSiteIDs we need for this run.  In this application, I assume that 
  #   ---- I can query once (via information on a subSiteID), as being representative for the site in question. 
  #   ---- This works because in luSubSiteID, ourSiteIDChoice1, etc. does not vary within a particular site.  
  luSubSiteID <- utils::read.csv(paste0(find.package("EnvCovDBpostgres"),"/helperFiles/luSubSiteID.csv"))
  xwalk <- luSubSiteID[luSubSiteID$subSiteID %in% sitesXwalk$subSiteID,]  
  uniqueOurSiteIDsToQuery <- unique(na.omit(c(xwalk$ourSiteIDChoice1,xwalk$ourSiteIDChoice2,xwalk$ourSiteIDChoice3)))
  
  df <- vector("list",length(uniqueOurSiteIDsToQuery))
  m1 <- vector("list",length(uniqueOurSiteIDsToQuery))   #  flow (cfs)
  m2 <- vector("list",length(uniqueOurSiteIDsToQuery))   #  temperature (C)
  
  #   ---- This is nearly verbatim of code in function getTogetherCovarData.  
  for(ii in 1:length(uniqueOurSiteIDsToQuery)){
    
    #   ---- Get covariate data of interest.  
    oursitevar <- uniqueOurSiteIDsToQuery[ii]   
    
    #   ---- Use these when building enhanced efficiency trials.  
    minEffDate <- as.character(min.date)
    maxEffDate <- as.character(max.date)
    
    #   ---- Query the PostgreSQL database for information on temperature and flow.  
    ch <- DBI::dbConnect(RPostgres::Postgres(),    
                    dbname='EnvCovDB',
                    host='streamdata.west-inc.com',
                    port=5432,
                    user="jmitchell",
                    password="G:hbtr@RPH5M.")
    res <- DBI::dbSendQuery(ch,paste0("SELECT COUNT(oursiteid) FROM tbld WHERE ('",min.date,"' <= date AND date <= '",max.date,"') AND oursiteid = ",oursitevar," GROUP BY oursiteid;"))
    nGood <- DBI::dbFetch(res)
    DBI::dbClearResult(res)
    DBI::dbDisconnect(ch)
    
    if(nrow(nGood) > 0){
      df[[ii]] <- EnvCovDBpostgres::queryEnvCovDB("jmitchell","G:hbtr@RPH5M.",minEffDate,maxEffDate,oursitevar,type="D",plot=FALSE)
      df[[ii]]$date <- strptime(df[[ii]]$date,format="%Y-%m-%d",tz=time.zone)
    } else {
      df[[ii]] <- EnvCovDBpostgres::queryEnvCovDB("jmitchell","G:hbtr@RPH5M.",minEffDate,maxEffDate,oursitevar,type="U",plot=FALSE)
      
      if(sum(!is.na(df[[ii]]$flow_cfs)) > 0 & (sum(df[[ii]]$flow_cfs <= -9997 & !is.na(df[[ii]]$flow_cfs)) > 0) ){
        df[[ii]][df[[ii]]$flow_cfs <= -9997 & !is.na(df[[ii]]$flow_cfs),]$flow_cfs <- NA
      }
      if(sum(!is.na(df[[ii]]$temp_c)) > 0){
        
        #   ---- This is mostly due to weird CDEC data.  
        
        #   ---- If we have temps less than -17.8F, then the Celsius value was 0.00.  Chuck these -- they're probably bad values. 
        #   ---- The value of 37.7778 corresponds to 100F.  
        if( any( (df[[ii]][!is.na(df[[ii]]$temp_c),]$temp_c >= 37.7778) | (df[[ii]][!is.na(df[[ii]]$temp_c),]$temp_c <= -17.8) ) ){
          df[[ii]][!is.na(df[[ii]]$temp_c) & ( df[[ii]]$temp_c >= 37.7778 | df[[ii]]$temp_c <= -17.8 ),]$temp_c <- NA
        }
        
        #   ---- They seem to use 32.0 as a substitution value as well.  Weird.  Get rid of those.  
        if( any( (df[[ii]][!is.na(df[[ii]]$temp_c),]$temp_c == 0.0) ) ){
          df[[ii]][!is.na(df[[ii]]$temp_c) & df[[ii]]$temp_c == 0.0,]$temp_c <- NA
        }
      }
    }  
    
    #   ---- Compile the good dates for each metric. 
    min.date.flow <- suppressWarnings(min(df[[ii]][!is.na(df[[ii]]$flow_cfs),]$date))
    max.date.flow <- suppressWarnings(max(df[[ii]][!is.na(df[[ii]]$flow_cfs),]$date))
    min.date.temp <- suppressWarnings(min(df[[ii]][!is.na(df[[ii]]$temp_c),]$date))
    max.date.temp <- suppressWarnings(max(df[[ii]][!is.na(df[[ii]]$temp_c),]$date))
    
    #   ---- If we have flow this go-around, calculate its mean.  
    if(!is.na(min.date.flow) & !is.na(max.date.flow)){
      m1[[ii]] <- mean(na.omit(df[[ii]]$flow_cfs))
    }
    
    #   ---- If we have temp this go-around, calculate its mean.  
    if(!is.na(min.date.temp) & !is.na(max.date.temp)){
      m2[[ii]] <- mean(na.omit(df[[ii]]$temp_c))
    }
    
  }

  #   ---- Now that we're done querying the EnvCovDBpostgres, take as 'the average,' the value from
  #   ---- the lowest list number; e.g., take m1[[1]] over m2[[2]], if both exist.  As of March 2018,
  #   ---- there could be up to three distinct calculated values, because the xwalk above allows up
  #   ---- to three distinct station queries, per subSiteID.
  ii.flow <- 1
  repeat{
    if(!is.null(m1[[ii.flow]]) | ii.flow == length(uniqueOurSiteIDsToQuery)){
      dischargeEnvCovMean <- m1[[ii.flow]]
      break()
    } else {
      ii.flow <- ii.flow + 1
    }
  }
  
  #   ---- Make sure we have something in the case we have no good flow.  
  if(is.null(dischargeEnvCovMean)){
    dischargeEnvCovMean <- NaN
  }
  
  ii.temp <- 1
  repeat{
    if(!is.null(m2[[ii.temp]]) | ii.temp == length(uniqueOurSiteIDsToQuery)){
      tempEnvCovMean <- m2[[ii.temp]]
      break()
    } else {
      ii.temp <- ii.temp + 1
    }
  }
  
  #   ---- Make sure we have something in the case we have no good temp.  
  if(is.null(tempEnvCovMean)){
    tempEnvCovMean <- NaN
  }
  
  avg2 <- data.frame(site=site,
                     Season=season,
                     flow_cfs=dischargeEnvCovMean,
                     temp_c=tempEnvCovMean)
    
    
  all <- merge(avg,avg2,by=c("site","Season"))  
    
  #   ---- Calculate an estimated mean percent Q, for this trap.  Should probably only work 
  #   ---- on the RBDD.  
  
  #   ----  water vel     * area *  half-cone * multipliers for adjustments
  #   ----                         (assume no)  
  q_d <- all$waterVel_fts * 24.6 *      1      / 43650 * 86400 
    
  #   ----  daily total stream Q * multipliers for adjustments
  Q_D <- all$flow_cfs * 86400 / 43560
  
  #   ---- Calculate percent Q.
  all$percQ <- q_d / Q_D
  
  
  
  #   ---- Assemble mean forklengths, and astrological statistics.  
  
  #   ---- Obtain necessary variables from the global environment.  
  fishingGapMinutes <- get("fishingGapMinutes",envir=.GlobalEnv)
  passageRounder <- get("passageRounder",envir=.GlobalEnv)
  
  #   Check that times are less than 1 year apart
  strt.dt <- as.POSIXct( min.date, format="%Y-%m-%d" )
  end.dt <- as.POSIXct( max.date, format="%Y-%m-%d" )
  run.season <- data.frame( start=strt.dt, end=end.dt )
  dt.len <- difftime(end.dt, strt.dt, units="days")
  if( dt.len > 366 )  stop("Cannot specify more than 365 days in F.passage. Check min.date and max.date.")
  
  #   ---- Identify the type of passage report we're doing
  # Utilize this construction to avoid NOTEs about assigning variables to the 
  # .GlobalEnv when running devtools::check().  
  pos <- 1
  envir <- as.environment(pos)
  assign("passReport","ALLRuns",envir=envir)
  passReport <- get("passReport",envir=.GlobalEnv)
  
  #   ---- Fetch the catch and visit data
  tmp.df   <- F.get.catch.data( site, taxon, min.date, max.date, output.file="Fake"  )
  
  catch.df <- tmp.df$catch   # All positive catches, all FinalRun and lifeStages, inflated for plus counts.  Zero catches (visits without catch) are NOT here.
  visit.df <- tmp.df$visit   # the unique trap visits.  This will be used in a merge to get 0's later
  
  catch.dfX <- catch.df      # save for a small step below.  several dfs get named catch.df, so need to call this something else.
  
  # if( nrow(catch.df) == 0 ){
  #   stop( paste( "No catch records between", min.date, "and", max.date, ". Check dates and taxon."))
  # }
  # 
  # #   ---- Summarize catch data by trapVisitID X FinalRun X lifeStage. Upon return, catch.df has one line per combination of these variables
  # catch.df0 <- F.summarize.fish.visit( catch.df, 'unassigned' )   # jason - 5/20/2015 - we summarize over lifeStage, wrt to unassigned.   10/2/2015 - i think by 'unassigned,' i really mean 'unmeasured'???
  # catch.df1 <- F.summarize.fish.visit( catch.df, 'inflated' )     # jason - 4/14/2015 - we summarize over lifeStage, w/o regard to unassigned.  this is what has always been done.
  # catch.df2 <- F.summarize.fish.visit( catch.df, 'assigned' )     # jason - 4/14/2015 - we summarize over assigned.  this is new, and necessary to break out by MEASURED, instead of CAUGHT.
  # 
  # catch.df3 <- F.summarize.fish.visit( catch.df, 'halfConeAssignedCatch' )     # jason - 1/14/2016
  # catch.df4 <- F.summarize.fish.visit( catch.df, 'halfConeUnassignedCatch' )   # jason - 1/14/2016
  # catch.df5 <- F.summarize.fish.visit( catch.df, 'assignedCatch' )             # jason - 1/14/2016
  # catch.df6 <- F.summarize.fish.visit( catch.df, 'unassignedCatch' )           # jason - 1/14/2016
  # catch.df7 <- F.summarize.fish.visit( catch.df, 'modAssignedCatch' )          # jason - 1/14/2016
  # catch.df8 <- F.summarize.fish.visit( catch.df, 'modUnassignedCatch' )        # jason - 1/14/2016
  
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
  release.df <- F.get.release.data( site, taxon=, min.date, max.date, visit.df )
  
  #   ---- Get some averages.  Maybe weight by number of released fish?  For now, I just do a straight average.  
  release.avgs <- data.frame(site=site,
                             Season=season,
                             bdMeanNightProp=mean(na.omit(release.df$meanNightProp)),
                             bdMeanMoonProp=mean(na.omit(release.df$meanMoonProp)),
                             bdMeanForkLength=mean(na.omit(release.df$meanForkLength)))
  
  all <- merge(all,release.avgs,by=c("site","Season"))
  
  return(all)
}