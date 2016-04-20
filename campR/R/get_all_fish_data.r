#' @export F.get.all.fish.data
#' 
#' @title F.get.all.fish.data
#' 
#' @description
#' 
#'    Fetch ALL the fish data from Access data base between two dated. 
#'    This is a version of F.get.indiv.fish.data, but that routine returns records for a single taxon.
#' 
#'    input:
#'    site = site ID of place we want to do estimates for. Note; Site is a location along a river where
#'        we can compute passage.  Subsites are places in the river (thalweg, right bank, etc) at the project.
#'        Project is a parent of subsite.
#'        Site is also a parent of subsite.
#'        Project is the funding source of the site.  Site and Project are parallel fields.  In theory,
#'        this makes it easier query sites and projects because sometimes the same site is sampled by
#'        two different projects.
#'    min.date and max.date = minimum and maximum dates for a visit to be included. A string in format "YYYY-MM-DD"
#' 
#' 
#' 
#'    ---NOTE: build_report_Criteria must be run before here. 
#' 
#' 
#'    *******
#'    Open ODBC channel
#' 
#' @param  site <describe argument>
#' @param  min.date <describe argument>
#' @param  max.date  <describe argument>
#' 
#' @details <other comments found in file>
#' NA
#'    Subsampling is handled above in the SQL with [CatchRaw].[n]*[CatchRaw].[subsampleDenominator]/[CatchRaw].[subsampleNumerator]
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
F.get.all.fish.data <- function( site, min.date, max.date ){
#
#   Fetch ALL the fish data from Access data base between two dated. 
#   This is a version of F.get.indiv.fish.data, but that routine returns records for a single taxon.
#
#   input:
#   site = site ID of place we want to do estimates for. Note; Site is a location along a river where
#       we can compute passage.  Subsites are places in the river (thalweg, right bank, etc) at the project.
#       Project is a parent of subsite.
#       Site is also a parent of subsite.
#       Project is the funding source of the site.  Site and Project are parallel fields.  In theory,
#       this makes it easier query sites and projects because sometimes the same site is sampled by
#       two different projects.
#   min.date and max.date = minimum and maximum dates for a visit to be included. A string in format "YYYY-MM-DD"
#


#   ---NOTE: build_report_Criteria must be run before here. 


#   *******
#   Open ODBC channel
db <- get( "db.file", env=.GlobalEnv ) 
ch <- odbcConnectAccess(db)

# ====== 


cat("SQL to retrieve ALL catch records between ")
cat(paste(min.date, "and", max.date, "\n"))


#   Execute the final SQL statement
catch <- F.run.sqlFile( ch, "QryAllCatch.sql" )

cat(paste(nrow(catch), "records retrieved.\n\n"))

if(nrow(catch) >= 10) {cat("First 10 records...\n"); print(catch[1:10,])} else {cat("Catch records...\n"); print(catch)}


#   Check for missing catches
if( any( is.na(catch$n) )){
    cat("Number of fish is missing for the following records:\n")
    print( catch[ is.na(catch$n), ] )
    stop("There are missing catches. Make sure at least 0 is entered for every count. ")
}






cat("Subsites found...\n")
subSites.found <- sort(unique(catch$trapPositionID))
print(subSites.found)
subsite.string <- paste(subSites.found, collapse="+")


#   Subsampling is handled above in the SQL with [CatchRaw].[n]*[CatchRaw].[subsampleDenominator]/[CatchRaw].[subsampleNumerator]


odbcClose(ch)

catch

}

