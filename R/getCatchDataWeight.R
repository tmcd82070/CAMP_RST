#' @export
#' 
#' @title getCatchDataWeight
#'   
#' @description Get the catch data from the database file with the weight
#'   measurement.
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
#' @details When the passage estimation routine is called with the analytical
#' life stage assignment, this function is called. The default queries that
#' retrieve the catch data do not include the weight measurement. This function
#' is needed to retrieve the weight information. This function is intended for
#' internal use only.
#' 
#' @return Data.frame of the catch data with the weight measurements.
#'   
#' @author Jared Studyvin WEST Inc.
#'   
#' @seealso \code{assignLifeStage}
#'   
#' @examples
#' \dontrun{
#' #   ---- Get weight data from the American. 
#' taxon <- 169180
#' site <- 570000
#' min.date <- "2013-01-01"
#' max.date <- "2013-05-30"
#' dfWeight <- getCatchDataWeight(taxon,site,min.date,max.date)
#' }

getCatchDataWeight <- function(taxon,site,min.date,max.date){

    ## the string must be 'db.file', or else f'n 'build_Report_Criteria.r' won't work.
    ##db.file <<- paste0(pathData,"/CAMP.mdb")

  #   ---- Jason comments out.  Program would have erred long before this if 
  #   ---- we couldn't get to the database.  Probably an artifact from testing.
#     if(!file.exists(db.file)){
#         print('file does not exist:')
#         print(db.file)
#         stop()
#     }

    ## ---- build the other queries connie mentions in her new sql query ----
    nvisits <- F.buildReportCriteria( site, min.date, max.date )      # trent f'n to build first report in a query sequence

    if( nvisits == 0 ){
        warning("Your criteria returned no trapVisit table records.")
        odbcCloseAll()
        return()
    }

    db <- get( "db.file", envir=.GlobalEnv )
    ch <- odbcConnectAccess(db)

    F.run.sqlFile(ch, "QrySamplePeriod.sql", R.TAXON=taxon ) # trent f'n that works as an access-sql handler
    ## ---- end building ----

    F.run.sqlFile(ch, "QryNotFishing.sql", R.TAXON=taxon )

    ## ---- finally, the new query ----
    F.run.sqlFile(ch,'QryUnmarkedChinookLifeStages.sql',TRUE,FALSE,R.TAXON=taxon)
    catch <- sqlFetch( ch, "TempSumUnmarkedByTrap_Run_Final2" )       # table name here
    includecatchID <- sqlFetch(ch, "TempSamplingSummary")             # jason add to get variable includeCatchID
    F.sql.error.check(catch)   # trent function to make sure the table seems ok

    close(ch) # disconnect
    ## ---- end new query -----

    ## return the data
    return(catch)

} ##end getCatchDataWeight function
