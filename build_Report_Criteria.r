#' @title Update a table in the Access CAMP database to contain a particular 
#'   site's trapVisitIDs between calendar dates specified by the user.
#'   
#' @param site The \code{siteID} for which data are requested.
#' @param min.date The start date for which data are requested.
#' @param max.date The end date for which data are requested.
#'   
#' @return Within R, function \code{build_Report_Criteria} returns a data frame 
#'   containing one row and row column of the total number of visits at a site 
#'   between the specified \code{min.date} and \code{max.date}.  Additionally, 
#'   it sets up a series of tables, via query sequence 
#'   \code{QryBuildReportCriteria.sql} within the Access CAMP database.
#'   
#' @section:  Details: Generally, function \code{build_Report_Criteria} is the
#' workhorse function associated with many query sequences, and sets up data
#' within the Access CAMP database for further processing.
#' 
#' @seealso \code{sqlQuery}, \code{F.run.sqlFile}, \code{F.sql.error.check}
#'   
#' @aliases
#' 
#' @examples
#' 
#' @export

F.buildReportCriteria <- function( site, min.date, max.date ){

  strt.dt <- as.POSIXct( min.date, format="%Y-%m-%d" )
  end.dt <- as.POSIXct( max.date, format="%Y-%m-%d" )

  db <- get( "db.file", env=.GlobalEnv ) 
  ch <- odbcConnectAccess(db)

  F.run.sqlFile( ch, "QryBuildReportCriteria.sql", SITE=site, STRT.DT=format(strt.dt, "%m/%d/%Y"), END.DT=format(end.dt, "%m/%d/%Y") )
  ans <- sqlQuery( ch, "SELECT COUNT(1) FROM TempReportCriteria_Trapvisit" )
  F.sql.error.check(ans)

  cat(paste(ans, "trap visits found between", strt.dt, "and", end.dt, "\n\n"))

  close(ch)
  ans[1,1]

}