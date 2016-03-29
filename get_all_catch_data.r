F.get.all.catch.data <- function( site, taxon, min.date, max.date ){
#
#   Fetch the catch data for a SINGLE TAXON from an Access data base. 
#   This function retrieves all catch data for 
#
#   input:
#   db = full path and name of the Access data base to retrieve data from
#   tables = vector with named components containing names
#           of the table in db to pull values from
#   site = site ID of place we want to do estimates for.
#   taxon = the taxon number (from luTaxon) to retrieve.  
#   min.date = minimum date for data to include. This is a text string in the format %Y-%m-%d, or YYYY-MM-DD
#   max.date = maximum date for data to include.  Same format as min.date
#
#   To be included in the catch data, a record has to be from the site,
#   of the correct taxon, of the correct run, and between min and max dates.
#



#   *****
nvisits <- F.buildReportCriteria( site, min.date, max.date )

if( nvisits == 0 ){
    warning("Your criteria returned no trapVisit table records.")
    return()
}


#   *****
#   Open ODBC channel
db <- get( "db.file", env=.GlobalEnv )
ch <- odbcConnectAccess(db)


#   *****
#   This SQL file develops the hours fished and TempSamplingSummary table
F.run.sqlFile( ch, "QrySamplePeriod.sql", R.TAXON=taxon )

#   *****
#   This SQL generates the sum chinook by trap series of queries
F.run.sqlFile( ch, "QrySumChinookByTrap.sql", R.TAXON=taxon )


#   *****
#   Now, fetch the result
visit <- sqlFetch( ch, "TempChinookSampling_i_final" )
F.sql.error.check(visit)

close(ch)
#   ******
#   Fetch run name
#run.name <- sqlQuery( ch, paste("SELECT run AS runName FROM luRun WHERE runID=", run ))
#F.sql.error.check(run.name)


#   Assign attributes
attr(visit, "siteID" ) <- site
attr(visit, "site.name") <- visit$Site[1]
attr(visit, "site.abbr") <- visit$siteAbbreviation[1]
#attr(visit, "runID") <- run
#attr(visit, "run.name") <- run.name
#attr(visit, "run.season") <- run.season
#attr(visit, "site.stream") <- site.stream
attr(visit, "subsites") <- unique(visit$TrapPositionID)
#attr(visit, "taxonID" ) <- taxon.string
#attr(visit, "species.name") <- sp.commonName
#
cat("First 20 records of catch data frame...\n")
if( nrow(visit) >= 20 ) print( visit[1:20,] ) else print( visit )

#f.banner("F.get.catch.data - Complete")


visit

}
