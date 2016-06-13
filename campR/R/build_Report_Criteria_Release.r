#' @export
#' 
#' @title F.buildReportCriteriaRelease
#' 
#' @description Update a table in the underlying Access database to contain release
#' \code{trapVisitID}s and count the number of efficiency trials.
#' 
#' @param site The identification number of the site for which estimates are 
#'   required.
#' @param min.date The start date for data to include. This is a text string in 
#'   the format \code{\%Y-\%m-\%d}, or \code{YYYY-MM-DD}.
#' @param max.date The end date for data to include.  Same format as 
#'   \code{min.date}.
#'   
#' @return Within R, the total integer count of efficiency trials.  Within
#'   Access, table \code{TempReportCriteria_Release} is created, itemizing the releases.
#'   
#' @details Unique efficiency trials are identified by unique \code{releaseIDs} 
#'   falling within the specified \code{min.date} and \code{max.date}.
#'   
#'   Note that the difference between this and the \code{buildReportCriteria} 
#'   query series is this one's focus on releases and subsequent recaptures.
#'   
#' @examples
#' \dontrun{
#' #   ---- American River at Watt Avenue, 2013 Season
#' site <- 57000                 
#' min.date <- "2013-01-16"
#' max.date <- "2013-06-08" 
#' 
#' #   ---- Obtain inclusive releases.    
#' nreleases <- F.buildReportCriteriaRelease( site, min.date, max.date )
#' }
#'
F.buildReportCriteriaRelease <- function( site, min.date, max.date ){

  #   ---- Identify the start and end dates.  
  strt.dt <- as.POSIXct( min.date, format="%Y-%m-%d" )
  end.dt <- as.POSIXct( max.date, format="%Y-%m-%d" )

  #   ---- Communicate with Access database, run queries, and pull down count of efficiency trials.  
  db <- get( "db.file", envir=.GlobalEnv )
  ch <- odbcConnectAccess(db)
  F.run.sqlFile( ch, "QryBuildReportCriteriaRelease.sql", SITE=site, STRT.DT=format(strt.dt, "%m/%d/%Y"), END.DT=format(end.dt, "%m/%d/%Y") )
  ans <- sqlQuery( ch, "SELECT COUNT(1) FROM TempReportCriteria_Release" )
  F.sql.error.check(ans)
  cat(paste(ans, "releases found between", strt.dt, "and", end.dt, "\n\n"))
  close(ch)

  ans[1,1]

}
