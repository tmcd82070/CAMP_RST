#' @export F.get.all.catch.data
#' 
#' @title F.get.all.catch.data 
#' 
#' @description Fetch catch data for a single taxon from an Access data base.
#' 
#' @param site The identification number of the site for which estimates are 
#'   required.
#' @param taxon The species identifier indicating the type of fish of interest. 
#'   This is always \code{161980}; i.e., Chinook Salmon.
#' @param min.date The start date for data to include. This is a text string in 
#'   the format \code{\%Y-\%m-\%d}, or \code{YYYY-MM-DD}.
#' @param max.date The end date for data to include.  Same format as 
#'   \code{min.date}.
#'   
#' @details Function \code{F.get.all.catch.data} is called from within function
#' \code{F.all.catch.table}, and helps 
#'   
#' @return A data frame entitled \code{visit}.  To be included, a record has to
#'   be from the correct \code{site} and \code{taxon}, and be between the values
#'   specified by \code{min.date} and \code{max.date}.
#'   
#' @seealso \code{F.all.catch.table}
#'   
#' @examples 
#' \dontrun{
#' 
#' # requires an mdb.
#' }
F.get.all.catch.data <- function( site, taxon, min.date, max.date ){

  # site <- 7000
  # taxon <- 161980
  # min.date <- "2010-01-01"
  # max.date <- "2010-05-30"

  nvisits <- F.buildReportCriteria( site, min.date, max.date )

  if( nvisits == 0 ){
    warning("Your criteria returned no trapVisit table records.")
    return()
  }

  #   ---- Open ODBC channel.
  db <- get( "db.file", envir=.GlobalEnv )
  ch <- odbcConnectAccess(db)

  #   ---- This SQL file develops the hours fished and TempSamplingSummary table.
  F.run.sqlFile( ch, "QrySamplePeriod.sql", R.TAXON=taxon )

  #   ---- This SQL generates the sum-chinook-by-trap query series.
  F.run.sqlFile( ch, "QrySumChinookByTrap.sql", R.TAXON=taxon )

  #   ---- Now, fetch the result.
  visit <- sqlFetch( ch, "TempChinookSampling_i_final" )
  F.sql.error.check(visit)
  close(ch)

  #   ---- Assign attributes.
  attr(visit, "siteID" ) <- site
  attr(visit, "site.name") <- visit$Site[1]
  attr(visit, "site.abbr") <- visit$siteAbbreviation[1]
  attr(visit, "subsites") <- unique(visit$TrapPositionID)
  cat("First 20 records of catch data frame...\n")
  if( nrow(visit) >= 20 ) print( visit[1:20,] ) else print( visit )

  visit

}
