F.get.catch.data <- function( site, taxon, min.date, max.date ){
#
#   Fetch the catch data for a SINGLE TAXON from an Access data base. Do some initial
#   computations, like dates.
#
#   input:
#   db = full path and name of the Access data base to retrieve data from
#   tables = vector with named components containing names
#           of the table in db to pull values from
#   site = site ID of place we want to do estimates for. 
#   taxon = the taxon number(s) (from luTaxon) to retrieve.  If a scalar, only 
#       one taxon is retrieved.  If vector of taxon id's, the sum of all 
#       taxons is retrieved.
#   run = the single run ID of the fish we want.  If run = NA, all records for the fish
#       will be pulled. 
#   min.date = minimum date for data to include. This is a text string in the format %Y-%m-%d, or YYYY-MM-DD
#   max.date = maximum date for data to include.  Same format as min.date
#
#   To be included in the catch data, a record has to be from the site, 
#   of the correct taxon, of the correct run, and between min and max dates. 
#

#f.banner <- function( x ){
#    cat("\n")
#    cat(paste(rep("=",50), collapse="")); 
#    cat(x); 
#    cat(paste(rep("=",50), collapse="")); 
#    cat("\n")
#}

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
#   This SQL generates times when the traps were not fishing
F.run.sqlFile( ch, "QryNotFishing.sql" )   


#   *****
#   This SQL generates unmarked fish by run and life stage
F.run.sqlFile( ch, "QryUnmarkedByRunLifestage.sql", R.TAXON=taxon )   


#   *****
#   Now, fetch the result 
catch <- sqlFetch( ch, "TempSumUnmarkedByTrap_Run_Final" )
F.sql.error.check(catch)

close(ch) 

#   ******************************************************************
#   Assign time zone (probably does not matter)
time.zone <- get( "time.zone", env=.GlobalEnv )
attr(catch$StartTime, "tzone") <- time.zone
attr(catch$EndTime, "tzone") <- time.zone


#   ********************************************************************
#   At this point, catch has all visits in it, even if no fish were caught.  
#   It also has non-fishing intervals.  This is how you identify these intervals:
#       1. zero catch = catch$Unmarked == 0  ($FinalRun and $LifeStage are both "Unassigned" for these lines)
#       2. not fishing = catch$TrapStatus == "Not fishing"  (equivalently, $trapVisitID is missing for these lines.  Only time its missing.)
#
#   Pull apart the visits from the catch, because plus count expansion only applys to positive catches.    
#   Recall, catch currently has multiple lines per trapVisit delineating fish with different fork lengths. 

visit.ind <- !duplicated( catch$trapVisitID ) | (catch$TrapStatus == "Not fishing")
visits <- catch[visit.ind,!(names(catch) %in% c("Unmarked", "FinalRun", "lifeStage", "forkLength", "RandomSelection"))]

#   ********************************************************************
#   Subset the catches to just positives.  Toss the 0 catches and non-fishing visits.
catch <- catch[ (catch$Unmarked > 0) & (catch$TrapStatus == "Fishing"), ]


<<<<<<< HEAD

# get summary counts of catch run vs. lifetstage for internal checking.
totalFish <<- sum(catch$Unmarked)
totalRun <<- aggregate(catch$Unmarked, list(FinalRun=catch$FinalRun), FUN=sum)
totalLifeStage <<- aggregate(catch$Unmarked, list(LifeStage=catch$lifeStage), FUN=sum)
totalRunXLifeStage <<- aggregate(catch$Unmarked, list(LifeStage=catch$lifeStage,FinalRun=catch$FinalRun), FUN=sum)

=======
>>>>>>> 9d98868ded31a228a275a2ef0e507154e8d0e2ca
catch$Unassd <- catch$lifeStage # jason add to ID the unassigned lifeStage -- necessary to separate measured vs caught.
#   ********************************************************************

#   Expand the Plus counts
catch <- F.expand.plus.counts( catch )


#   Reassign factor levels because they may have changed.  I.e., we may have eliminated "Unassigned"
catch$FinalRun <- as.character( catch$FinalRun ) 
catch$lifeStage <- as.character( catch$lifeStage ) 
#catch$lifeStage <- as.character( catch$Unassd ) jason - possibly delete


#   ********************************************************************
#   Assign batch dates
visits <- F.assign.batch.date( visits )
catch <- F.assign.batch.date( catch )


#   Assign attributes
attr(catch, "siteID" ) <- site
attr(catch, "site.name") <- catch$siteName[1]
#attr(catch, "site.abbr") <- catch$siteAbbreviation[1]
#attr(catch, "runID") <- run
#attr(catch, "run.name") <- run.name
#attr(catch, "run.season") <- run.season
#attr(catch, "site.stream") <- site.stream 
attr(catch, "subsites") <- unique(catch$trapPositionID)
#attr(catch, "taxonID" ) <- taxon.string 
#attr(catch, "species.name") <- sp.commonName
#
cat("First 20 records of catch data frame...\n")
if( nrow(catch) >= 20 ) print( catch[1:20,] ) else print( catch )

#f.banner("F.get.catch.data - Complete")


#   Return two data frames. One containing positive catches.  The other containing visit and fishing information. 
list( catch=catch, visit=visits )

}
