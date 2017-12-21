#' @export
#' 
#' @title F.get.release.data.enh
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
#' @return Results from running the enhanced efficiency model fitting process 
#'   include \code{csv}s of efficiency trials missing a covariate, for each
#'   \code{TrapPositionID} at a \code{subSiteID}.  This prevents these
#'   efficiency trials from inclusion in the model fitting process.
#'   
#'   Each \code{TrapPositionID} also results in a series of plots depicting the
#'   fitted temporal spline, at each point of the backwards-fitting process. 
#'   These could number many, depending on the number of covariates available
#'   for possible exlcusion.
#'   
#'   Each \code{trapPositionID} also outputs a \code{png} containing model
#'   fitting information, including plots of efficiency versus each considered
#'   covariate, along with plotted temporal trends of each covariate against
#'   time.  Additional plots include the final fitted temporal spline (along
#'   with an prediction "curve" derived from the available data), as well as a
#'   final "plot" depicting model summary statistics, obtained via the
#'   \code{summary} function against the logistic efficiency-trial model fits.
#'   
#'   Note that no passage estimates result from the fitting of enhanced
#'   efficiency models.  This is because bootstrapping does not occur, but also
#'   because estimation of passage is not the goal of the model fitting process.
#'   Use the regular function sequence; i.e., functions without the
#'   \code{".enh"} to estimation passage for one-year intervals of interest.
#'   
#' @section Five programs make up the specialized procedure for fitting enhanced
#'   efficiency models.  This means actually compiling the data of efficiency 
#'   trials obtained over several years, and then fitting a generalized additive
#'   model (GAM) to those data.  All five programs have suffixes of 
#'   \code{".enh"}, and originated with the program versions without the suffix.
#'   As such, they are very similar to the originals.
#'   
#'   The first, \code{run_passage.enh} corrals the fitting.  It is different 
#'   from \code{run_passage} in that all of the passage summary that is usually 
#'   created has been suppressed.  This is because there is no need to 
#'   bootstrap, once enhanced efficiency models have been obtained.
#'   
#'   The second, \code{get_release_data.enh} modifies the obtaining of release 
#'   data, so as to obtain astrological data and mean fork-length.  It is very 
#'   similar to its originator.
#'   
#'   The third, \code{est_passage.enh}, corrals the data from 
#'   \code{get_release_data.enh} for use in \code{est_efficiency.enh}.  It too 
#'   should be very similar to its originator.
#'   
#'   The fourth, \code{est_efficiency.enh}, ensures the calculation of weighted 
#'   averages for the three efficiency-trial covariates have to do with mean 
#'   fork-lengths, and percent of fishing performed at night, or while the moon 
#'   is up.  It also emulates closely its originator.
#'   
#'   Finally, the fifth, \code{eff_model.enh}, fits the enhanced efficiency 
#'   models.  It follows a backwards selection procedure, allowing for both 
#'   variable covariate selection, as well as variable temporal spline 
#'   complexity.  It creates graphical output, for each trap, so as to provide 
#'   further hypothesis generation.
#' 
#' @seealso \code{run_passage.enh}, \code{get_release_data},
#'   \code{est_passage.enh}, \code{est_efficiency.enh}, \code{eff_model.enh}
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
#' df <- F.get.release.data(site,taxon,min.date,max.date)
#' }
F.get.release.data.enh <- function( site, taxon, min.date, max.date, visit.df ){

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
  astroStuff <- buildAstroStats(release.visit,visit.df)
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

  
  release.visit[is.na(release.visit$nForkLength),]$nForkLength <- fl0[match(release.visit[is.na(release.visit$nForkLength),]$trapVisitID,fl0$trapVisitID),]$nForkLength
  
  
  
  
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
  attr(ans, "forEffPlots" ) <- forEffPlots

  ans
  #plot(tmp$sunProp[!is.na(tmp$sunProp)],tmp$moonProp[!is.na(tmp$moonProp)])
  #plot(ans$meanNightProp[!is.na(ans$meanNightProp)],ans$meanMoonProp[!is.na(ans$meanMoonProp)])
}