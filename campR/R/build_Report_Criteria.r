#' @export
#' 
#' @title F.buildReportCriteria 
#' 
#' @description Update Access database table \code{TempReportCriteria_Trapvisit}
#'   to contain all unique trapping instances for the site and inclusive
#'   calendar dates specified by the user.
#'   
#' @param site The identification number of the site for which estimates are 
#'   required.
#' @param min.date The start date for data to include. This is a text string in 
#'   the format \code{\%Y-\%m-\%d}, or \code{YYYY-MM-DD}.  
#' @param max.date The end date for data to include.  Same format as 
#'   \code{min.date}.
#'   
#' @return Within R, function \code{build_Report_Criteria} returns a data frame 
#'   containing one row and row column of the total number of visits at a site 
#'   between the specified \code{min.date} and \code{max.date}.  Within Access, 
#'   via the \code{RODBC} package, it creates table \code{TempReportCriteria_Trapvisit}, via query series  
#'   Build Report Criteria.
#'   
#' @details Function \code{build_Report_Criteria} is the
#' workhorse function associated with many query series, and sets up data
#' within the Access CAMP database for further processing.  See function 
#' \code{F.run.sqlFile} for more details on query series.  
#' 
#' @seealso \code{sqlQuery}, \code{F.run.sqlFile}, \code{F.sql.error.check}
#'   
#' @author Trent McDonald (tmcdonald@west-inc.com)
#' 
#' @examples
#' \dontrun{
#' #   ---- American River at Watt Avenue, 2013 Season
#' site <- 57000                 
#' min.date <- "2013-01-16"
#' max.date <- "2013-06-08" 
#' 
#' #   ---- Obtain inclusive trap visits.  
#' nvisits <- F.buildReportCriteria( site, min.date, max.date )
#' }

F.buildReportCriteria <- function( site, min.date, max.date ){
  
  strt.dt <- as.POSIXct( min.date, format="%Y-%m-%d" )
  end.dt <- as.POSIXct( max.date, format="%Y-%m-%d" )
  
  db <- get( "db.file", envir=.GlobalEnv ) 
  ch <- odbcConnectAccess(db)
  
  F.run.sqlFile( ch, "QryBuildReportCriteria.sql", SITE=site, STRT.DT=format(strt.dt, "%m/%d/%Y"), END.DT=format(end.dt, "%m/%d/%Y") )
  ans <- sqlQuery( ch, "SELECT COUNT(1) FROM TempReportCriteria_Trapvisit" )
  F.sql.error.check(ans)
  
  cat(paste(ans, "trap visits found between", strt.dt, "and", end.dt, "\n\n"))
  
  close(ch)
  ans[1,1]
  
}
