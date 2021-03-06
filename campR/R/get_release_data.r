#' @export
#' 
#' @title F.get.release.data 
#' 
#' @description Fetch data on efficiency trials from an Access database. 
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
#' @param visit.df A data frame resulting from the catch SQL sequence,
#'   specially adapted to have an attribute data frame \code{fl0} attached,
#'   containing \code{trapVisitID}-specific mean fork lengths.
#'   
#' @return A data frame summarizing efficiency trials for the site of interest
#'   for all traps between the dates indicated.  Data include \code{Recaps}
#'   numerators and \code{nReleased} denominators.
#'   
#' @details Function \code{F.get.release.data} utilizes query sequences Build 
#'   Report Criteria and Build Report Criteria Release to obtain all results 
#'   within the specified \code{min.date} and \code{max.date}.  See section
#'   Structured Query Language (SQL) Queries in function 
#'   \code{F.run.sqlFile} for more details.
#'   
#'   Query results include one record for every trap visit within a 
#'   \code{releaseTime}, say 7 days, even if the trap visit did not catch any 
#'   marked fish.  In this case, zeros are recorded for all combinations of 
#'   \code{releaseID}, \code{trapPositionID}, and \code{trapVisitID}.
#'   
#'   Given a specific release, the resulting data frame tells how many fish from
#'   the release were captured on subsequent trap visits, for each trap.  These 
#'   result from collapsing all trap visits and computing the total number of 
#'   each release's captured fish.  Note that generally, at any one time, more
#'   than one release can "go," and so a single trap visit may catch fish from
#'   multiple releases.  Total recaptures are summarized over unique
#'   combinations of trap visits and positions, via variables \code{releaseID}
#'   and \code{trapPositionID}.
#'   
#'   Release records need to have both variables \code{IncludeTest} and 
#'   \code{IncludeCatch} flagged as \code{"Yes"} for inclusion in efficiency 
#'   estimation. Recaptures that took place during half-cone operations are 
#'   multiplied by the value of the \code{halfConeMulti} global variable, which is set 
#'   at 2. Half cone operations are identified by variable \code{HalfCone} 
#'   having a value of \code{"Yes"}.
#'   
#'   Variables \code{HrsToFirstVisitAfter} and \code{HrsToLastVisitAfter} are 
#'   used in function \code{F.est.efficiency} as helper variables to derive a 
#'   batch date when the \code{meanEndTime} variable is \code{NA}.
#'   
#' @section Mean Recapture Time: The mean recapture time is estimated for each 
#'   unique grouping of \code{releaseID} and \code{trapPositionID}.  In the case
#'   of no recaptures, the mean recapture time is recorded as \code{NA}.  In all
#'   other cases, the mean recapture time is calculated as the weighted mean of 
#'   recapture times, weighting on the number of caught fish.  For example, 
#'   suppose fishing takes place at a particular trap over 7 consecutive days. 
#'   If the bulk of fish were caught on day 7, then the mean recapture time 
#'   would be near that 7th day of fishing.  This is in contrast to a "straight"
#'   mean recapture time, which would estimate a mean recapture time 3.5 days
#'   after the start of fishing, regardless of the temporal distribution of 
#'   captured fish over the entire 7-day period.
#'   
#' @seealso \code{F.run.sqlFile}, \code{F.summarize.releases}, \code{F.release.summary}
#' 
#' @author WEST Inc. 
#' 
#' @examples
#' \dontrun{
#' #   ---- Fetch all Chinook salmon efficiency data on the American River 
#' #   ---- between Jan. 16, 2013 and June 8th, 2013.  
#' site <- 57000
#' taxon <- 161980
#' min.date <- "2013-01-01"
#' max.date <- "2013-06-01"
#' visit.df <- visitDF
#' df <- F.get.release.data(site,taxon,min.date,max.date,visit.df=visit.df)
#' }
F.get.release.data <- function( site, taxon, min.date, max.date, visit.df ){
  
  # site <- 34000
  # taxon <- 161980
  # min.date <- min.date2#"2006-01-01"
  # max.date <- max.date2#"2006-12-01"
  # visit.df <- visit.df
  
  #   ---- Get global environment data. 
  halfConeMulti <- get("halfConeMulti",envir=.GlobalEnv)
  
  #   ---- Run report criteria for trap visits. Build TempReportCriteria_Trapvisit.
  nvisits <- F.buildReportCriteria( site, min.date, max.date )
  
  if( nvisits == 0 ){
    warning("Your criteria returned no trap visits.")
    return()
  }
  
  #   ---- Run report criteria for efficiency releases. Build TempReportCriteria_Release.
  nreleases <- F.buildReportCriteriaRelease( site, min.date, max.date )
  
  if( nreleases == 0 ){
    warning("Your criteria returned no releases.")
    return()
  }
  
  #   ---- Open ODBC channel.
  db <- get( "db.file", envir=.GlobalEnv )
  ch <- odbcConnectAccess(db)
  
  #   ---- Develop the TempSamplingSummary table.
  F.run.sqlFile( ch, "QrySamplePeriod.sql", R.TAXON=taxon )
  
  #   ---- Develop the hours fished and TempSamplingSummary table.  
  F.run.sqlFile( ch, "QryEfficiencyTests.sql", R.TAXON=taxon )
  
  #   ---- Now, fetch the result
  release.visit <- sqlFetch( ch, "TempRelRecap_final" )
  F.sql.error.check(release.visit)
  
  close(ch)
  
  
  #   ---- Compile astro statistics.  
  astroStuff <- buildAstroStats(release.visit,visit.df)  # this is slow, speed it up with dplyr
  release.visit <- astroStuff$release.visit
  forEffPlots <- astroStuff$forEffPlots
  fl0 <- astroStuff$fl0
  
  #   ---- Assign time zones to date-time columns
  time.zone <- get( "time.zone", envir=.GlobalEnv )
  attr(release.visit$ReleaseDate, "tzone") <- time.zone
  attr(release.visit$VisitTime, "tzone") <- time.zone
  
  #   ---- Drop any rows that are flagged as "Do not include."
  release.visit <- release.visit[ (release.visit$IncludeTest == "Yes") & (release.visit$IncludeCatch == "Yes"), ]
  
  #   ---- Adjust for halfCone adjustment via the halfConeMulti global variable.
  release.visit$halfConeAdj <- ifelse(release.visit$HalfCone == 'Yes',(halfConeMulti - 1)*release.visit$Recaps,0)
  release.visit$oldRecaps <- release.visit$Recaps
  release.visit$Recaps <- release.visit$oldRecaps + release.visit$halfConeAdj
  
  
  release.visit$nForkLength[is.na(release.visit$nForkLength)] <- 
    fl0$nForkLength[match(release.visit$trapVisitID[is.na(release.visit$nForkLength)],fl0$trapVisitID)]
  
  
  
  
  #   ---- Sum over trapVisits at a trapPosition.  
  by.list <- list(trapPositionID = release.visit$trapPositionID,
                  releaseID = release.visit$releaseID )
  
  #   ---- Create indicator for unique groups below.
  ind  <- tapply( release.visit$Recaps, by.list, FUN=NULL)
  
  #   ---- Calculate the mean recapture time.
  u.groups <- sort(unique(ind))
  ans <- NULL
  for( g in u.groups ){
    tmp <- release.visit[ ind == g, ]
    one.row <- tmp[1,]
    
    #   ---- Number caught.  And number of night and moon minutes.  And total (Jason) fishing minutes.  
    one.row$Recaps <- sum(tmp$Recaps, na.rm=T)
    one.row$allNightMins <- sum(tmp$nightMinutes)        
    one.row$allMoonMins <- sum(as.numeric(tmp$moonMinutes))
    one.row$allSampleMins <- sum(as.numeric(tmp$JasonSampleMinutes))
    one.row$allfl <- sum(tmp[tmp$nForkLength > 0,]$wmForkLength*tmp[tmp$nForkLength > 0,]$nForkLength)
    one.row$allNfl <- sum(tmp$nForkLength) 
    
    #   ---- Compute time to first and last visit after release, even if they did not catch any marked fish. 
    tmp.hdiff <- as.numeric( difftime(tmp$VisitTime, tmp$ReleaseDate, units="hours") )
    one.row$HrsToFirstVisitAfter <-  min( tmp.hdiff )
    one.row$HrsToLastVisitAfter <-  max( tmp.hdiff )
    
    #   ---- Mean time frame of released fish that were captured.
    if( one.row$Recaps == 0 ){
      one.row$meanRecapTime <- NA
      one.row$meanTimeAtLargeHrs <- NA   
      
      one.row$meanNightProp <- one.row$allNightMins / one.row$allSampleMins
      one.row$meanMoonProp <- one.row$allMoonMins / one.row$allSampleMins
      one.row$meanForkLength <- one.row$allfl / one.row$allNfl
    } else {
      tmp.v <- as.numeric(tmp$VisitTime)
      one.row$meanRecapTime <- sum(tmp.v * tmp$Recaps, na.rm=T) / one.row$Recaps
      one.row$meanTimeAtLargeHrs <- sum(tmp.hdiff * tmp$Recaps, na.rm=T) / one.row$Recaps 
      
      one.row$meanNightProp <- one.row$allNightMins / one.row$allSampleMins
      one.row$meanMoonProp <- one.row$allMoonMins / one.row$allSampleMins
      one.row$meanForkLength <- one.row$allfl / one.row$allNfl
    }
    
    #   ---- Drop the columns over which we are summing.
    one.row <- one.row[,-which( names(one.row) %in% c("VisitTime", "trapVisitID", "SampleMinutes")) ]   
    ans <- rbind(ans, data.frame(one.row))
  }
  
  class(ans$meanRecapTime) <- class(release.visit$VisitTime)
  attr(ans$meanRecapTime, "tzone") <- attr(release.visit$VisitTime, "tzone")
  
  cat("First 20 records of RELEASE table:\n")
  print( ans[1:min(nrow(ans),20),] )
  
  #   ---- Store values of some header info as attribute.
  attr(ans, "taxonID" ) <- taxon
  attr(ans, "siteID" ) <- site
  
  ans
  #plot(tmp$sunProp[!is.na(tmp$sunProp)],tmp$moonProp[!is.na(tmp$moonProp)])
  #plot(ans$meanNightProp[!is.na(ans$meanNightProp)],ans$meanMoonProp[!is.na(ans$meanMoonProp)])
  
}