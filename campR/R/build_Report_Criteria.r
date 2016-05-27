#' @title F.buildReportCriteria
#' 
#' @description
#' Update a table in the Access CAMP database to contain a particular 
#'   site's trapVisitIDs between two dates specified by the user.
#'   
#' @param site The \code{siteID} for which data are requested.
#' @param min.date The start date for which data are requested.
#' @param max.date The end date for which data are requested.
#'   
#' @return A data frame 
#'   containing one row and one column of the total number of visits at \code{siteID}  
#'   between \code{min.date} and \code{max.date}.  
#'    
#' @section Side effect: The query run by this function, 
#' \code{QryBuildReportCriteria.sql}, constructs a temporary table 
#' in the CAMP Access file which is later used by other tables and queries
#' (i.e., \code{TempReportCriteria_Trapvisit}). 
#' Many series of queries start and data extractions start with this one.
#' 
#' @seealso \code{sqlQuery}, \code{F.run.sqlFile}, \code{F.sql.error.check}
#'   
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

