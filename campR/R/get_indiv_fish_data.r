#' @export F.get.indiv.fish.data
#' 
#' @title F.get.indiv.fish.data
#' 
#' @description
#' 
#'    Fetch the fish data for a SINGLE TAXON from an Access data base. The resulting data 
#'    set has one line per fish (or group of fish of same length). 
#' 
#'    input:
#'    site = site ID of place we want to do estimates for. Note; Site is a location along a river where 
#'        we can compute passage.  Subsites are places in the river (thalweg, right bank, etc) at the project.  
#'        Project is a parent of subsite.
#'        Site is also a parent of subsite.
#'        Project is the funding source of the site.  Site and Project are parallel fields.  In theory, 
#'        this makes it easier query sites and projects because sometimes the same site is sampled by 
#'        two different projects. 
#'    taxon = the taxon number(s) (from luTaxon) to retrieve.  If a scalar, only 
#'        one taxon is retrieved.  If vector of taxon id's, the sum of all 
#'        taxons is retrieved.
#'    run = the single run ID of the fish we want.  If run = NA, all records for the fish
#'        will be pulled. 
#'    min.date and max.date = minimum and maximum dates for a visit to be included. A string in format "YYYY-MM-DD"
#'    keep = string specifying the type of fish to keep in the records. keep="unmarked" keeps all 
#'        fish without efficiency trail marks (all fish not in efficiency trial).  keep="marked" keeps 
#'        only fish that were involved in an efficiency trial. keep="all" (anything else) will keep 
#'        all fish records, both marked and unmarked. 
#' 
#'    To be included in the catch data, a record has to be from the site, 
#'    of the correct taxon, of the correct run, and between min and max date. 
#' 
#' 
#' 
#'    *******
#'    Retrieve db file name and table names and any other constants
#' 
#' @param  site <describe argument>
#' @param  taxon <describe argument>
#' @param  run <describe argument>
#' @param  min.date <describe argument>
#' @param  max.date <describe argument>
#' @param  keep="unmarked"  <describe argument>
#' 
#' @details <other comments found in file>
#'    Need these to determine visits.
#'    Need these to filter visits.
#'    *******
#'    Open ODBC channel
#'    *******
#'    Retreive common names for the site
#'    Fetch subsite names
#'    Fetch species name 
#'    Fetch run name
#'  F.sql.error.check(catch)
#'  if( nrow(catch) == 0 ){
#'      return(catch)    
#'  }
#'    Now, subset to run. We cannot do this in the SQL above because we need unknown runs in order to expand for plus counts
#'  catch <- catch[ !is.na(catch$finalRunID) & catch$finalRunID == run, ]
#'  -----
#'  JASON: THE CATCH QUERY CLEARLY ALREADY HAS THE SUBSITEID NAMES/LABELS, SO OBSOLETE. 1/26/2015
#'  -----
#'    Find subsiteID names
#'  subSites.found <- data.frame(subSiteID=sort(unique(catch$trapPositionID)))
#'  subSites.found <- merge( subSites.found, subsite.names, by="subSiteID", all.x=T )
#'  print(subSites.found)
#' subsite.string <- paste(subSites.found, collapse="+")
#'    A note on subsampling: subsampling is handled by the people entering data.  I.e., they enter a subsampleNumerator 
#'    and a subsampleDenominator, and these are used in the SQL statement above.  The calculation is 
#'    [CatchRaw].[n]*[CatchRaw].[subsampleDenominator]/[CatchRaw].[subsampleNumerator]
#'    subsampleMethodID should not be used to find subsampling. 
#'    Store values of run.season as attribute for convienance. 
#'  attr(catch, "subsites") <- subSites.found
#'  attr(catch, "taxonID" ) <- taxon.string
#'    If there are none of the particular taxon caught, there are no records in data frame catch. 
#'    This is correct, just be sure to check for nrow(catch) > 0 in any routines that use this data.
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
F.get.indiv.fish.data <- function( site, taxon, run, min.date, max.date, keep="unmarked" ){
#
#   Fetch the fish data for a SINGLE TAXON from an Access data base. The resulting data 
#   set has one line per fish (or group of fish of same length). 
#
#   input:
#   site = site ID of place we want to do estimates for. Note; Site is a location along a river where 
#       we can compute passage.  Subsites are places in the river (thalweg, right bank, etc) at the project.  
#       Project is a parent of subsite.
#       Site is also a parent of subsite.
#       Project is the funding source of the site.  Site and Project are parallel fields.  In theory, 
#       this makes it easier query sites and projects because sometimes the same site is sampled by 
#       two different projects. 
#   taxon = the taxon number(s) (from luTaxon) to retrieve.  If a scalar, only 
#       one taxon is retrieved.  If vector of taxon id's, the sum of all 
#       taxons is retrieved.
#   run = the single run ID of the fish we want.  If run = NA, all records for the fish
#       will be pulled. 
#   min.date and max.date = minimum and maximum dates for a visit to be included. A string in format "YYYY-MM-DD"
#   keep = string specifying the type of fish to keep in the records. keep="unmarked" keeps all 
#       fish without efficiency trail marks (all fish not in efficiency trial).  keep="marked" keeps 
#       only fish that were involved in an efficiency trial. keep="all" (anything else) will keep 
#       all fish records, both marked and unmarked. 
#
#   To be included in the catch data, a record has to be from the site, 
#   of the correct taxon, of the correct run, and between min and max date. 
#


#   *******
#   Retrieve db file name and table names and any other constants
No.code <- get("No.code", pos=.GlobalEnv)
Yes.code <- get("Yes.code", pos=.GlobalEnv)
tables <- get( "table.names", envir=.GlobalEnv )
db <- get( "db.file", envir=.GlobalEnv ) 


#   *******
#   First, save the start and stop dates of the run. 
#   Need these to determine visits.
#   Need these to filter visits.
strt.dt <- as.POSIXct( min.date, format="%Y-%m-%d" )
end.dt <- as.POSIXct( max.date, format="%Y-%m-%d" )
run.season <- data.frame( start=strt.dt, end=end.dt )


#   *******
#   Open ODBC channel
ch <- odbcConnectAccess(db)


#   *******
#   Retreive common names for the site
sites <- sqlQuery( ch, paste("SELECT siteName, siteAbbreviation, siteID, streamName FROM", tables["sites"], 
        "WHERE (siteID =", site, ")" ))
F.sql.error.check(sites)        
site.stream <- as.character(sites$streamName)
site.abbr <- as.character(sites$siteAbbreviation)
site.name <- as.character(sites$siteName)

#   Fetch subsite names
subsite.names <- sqlQuery( ch, paste("SELECT subSiteID, subSiteName FROM", tables["subsites"], 
        "WHERE (siteID =", site, ")" ))
F.sql.error.check(subsite.names)
subsite.names$subSiteName <- as.character(subsite.names$subSiteName)        


#   Fetch species name 
sp.codes <- sqlQuery(ch, paste("SELECT taxonID, commonName FROM", tables["species.codes"]))
F.sql.error.check(sp.codes)
sp.commonName <- as.character(sp.codes$commonName[ sp.codes$taxonID %in% taxon ])

#   Fetch run name
runs <- sqlQuery(ch, paste( "SELECT run, runID FROM", tables["run.codes"] ))
F.sql.error.check(runs)
run.name <- as.character(runs$run[ runs$runID == run ])



cat( paste(site.name, site.abbr, site.stream,  sep=":") )
cat("\n")
cat( paste(sp.codes$taxonID[ sp.codes$taxonID %in% taxon ], sp.commonName, run.name, sep=":") )
cat("\n")

tmp.df <- F.get.catch.data( site, taxon, min.date, max.date  )
catch <- tmp.df$catch

# F.sql.error.check(catch)

# if( nrow(catch) == 0 ){
#     return(catch)    
# }

if(nrow(catch) >= 20) {cat("First 20 catch records...\n"); print(catch[1:20,])} else {cat("Catch records...\n"); print(catch)}

cat(paste(nrow(catch), "total records in catch table.\n")) 


#   Now, subset to run. We cannot do this in the SQL above because we need unknown runs in order to expand for plus counts
# catch <- catch[ !is.na(catch$finalRunID) & catch$finalRunID == run, ]
catch <- catch[ !is.na(catch$FinalRun) & catch$FinalRun == run.name, ]

# -----
# JASON: THE CATCH QUERY CLEARLY ALREADY HAS THE SUBSITEID NAMES/LABELS, SO OBSOLETE. 1/26/2015
# -----
#   Find subsiteID names
# subSites.found <- data.frame(subSiteID=sort(unique(catch$trapPositionID)))
# subSites.found <- merge( subSites.found, subsite.names, by="subSiteID", all.x=T )

cat("Subsites found...\n")
# print(subSites.found)
print(unique(catch$TrapPosition))
#subsite.string <- paste(subSites.found, collapse="+")


#   A note on subsampling: subsampling is handled by the people entering data.  I.e., they enter a subsampleNumerator 
#   and a subsampleDenominator, and these are used in the SQL statement above.  The calculation is 
#   [CatchRaw].[n]*[CatchRaw].[subsampleDenominator]/[CatchRaw].[subsampleNumerator]
#   subsampleMethodID should not be used to find subsampling. 
 

#   Store values of run.season as attribute for convienance. 
attr(catch, "site") <- site
attr(catch, "site.name" ) <- site.name
attr(catch, "site.abbr") <- site.abbr
attr(catch, "site.stream") <- site.stream
# attr(catch, "subsites") <- subSites.found
# attr(catch, "taxonID" ) <- taxon.string
attr(catch, "species.name") <- sp.commonName
attr(catch, "runID") <- run
attr(catch, "run.name") <- run.name
attr(catch, "run.season") <- run.season

#   If there are none of the particular taxon caught, there are no records in data frame catch. 
#   This is correct, just be sure to check for nrow(catch) > 0 in any routines that use this data.

odbcClose(ch)

catch

}
