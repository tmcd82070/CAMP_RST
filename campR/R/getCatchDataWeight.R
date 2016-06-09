#' @export getCatchDataWeight
#' 
#' @title getCatchDataWeight
#' 
#' @description
#' 
#'  Jared Studyvin
#'  14 Jan 2016
#'  get the data for the life stage assignment
#' 
#' 
#' 
#' 
#' 
#' @param taxon <describe argument>
#' @param site <describe argument>
#' @param min.date <describe argument>
#' @param max.date <describe argument>
#' 
#' @details <other comments found in file>
#'  ---- end building ----
#'  ---- finally, the new query ----
#'  ---- end new query -----
#'  return the data
#' 
#' @return <describe return value>
#' 
#' @author WEST Inc.
#' 
#' @seealso \code{\link{<related routine>}}, \code{\link{<related routine>}}
#' 
#' @examples
#' <insert examples>
#' 
############################################
## Jared Studyvin
## 14 Jan 2016
## get the data for the life stage assignment
############################################



getCatchDataWeight <- function(taxon,site,min.date,max.date){



    require(RODBC)

    ## the string must be 'db.file', or else f'n 'build_Report_Criteria.r' won't work.
    ##db.file <<- paste0(pathData,"/CAMP.mdb")

    if(!file.exists(db.file)){
        print('file does not exist:')
        print(db.file)
        stop()
    }

    ## ---- build the other queries connie mentions in her new sql query ----
    nvisits <<- F.buildReportCriteria( site, min.date, max.date )      # trent f'n to build first report in a query sequence

    if( nvisits == 0 ){
        warning("Your criteria returned no trapVisit table records.")
        odbcCloseAll()
        return()
    }


    db <- get( "db.file", env=.GlobalEnv )
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
