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
tables <- get( "table.names", env=.GlobalEnv )
db <- get( "db.file", env=.GlobalEnv ) 


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




#   **********************************************************************************************
#   SQL for CATCH
#   We need to pull catch data that is from right dates and right species.  
#   Note: I hard wired the name of the primary keys (.trapVisitID) here.  Table names are 
#   in parameter 'tables', but the link keys of these tables is hard wired here.  In addition, 
#   variables used in the WHERE clauses are hardwired (e.g., 'dateSampleStarted', etc.).
#       Joins: trap visit to catch to mark found 
#       Conditions: date range correct AND correct run AND correct taxon AND not a recaptured fish


visit.vars <- c("projectDescriptionID",
                "trapPositionID",
                "halfConeID",
                "visitTime")
visit.vars.sql <- F.vec2sqlstr(tables["trap.visit"], visit.vars)

catch.vars <- c("catchRawID",
                "trapVisitID",
                "taxonID",
                "finalRunID",
                "fishOriginID",
                "lifeStageID",
                "subsampleMethodID",
                "mortID",
                "releaseID",
                "randomID", 
                "forkLength",
                "totalLength" )
catch.vars.sql <- F.vec2sqlstr(tables["catch"], catch.vars)

catch.vars.sql <- paste( catch.vars.sql, ", [CatchRaw].[n]*[CatchRaw].[subsampleDenominator]/[CatchRaw].[subsampleNumerator] AS n", sep="")

#   Construct Taxon where.  remember there could be more than one species.
taxon.string <- taxon[1]
taxon.where <- paste( "AND (((", tables["catch"], ".taxonID)='", taxon[1], "') ", sep="" )
if( length(taxon) > 1 ){
    for( i in 2:length(taxon)){
        taxon.string<- paste( taxon.string, "+", taxon[i], sep="" )
        taxon.where <- paste( taxon.where, "OR ((", tables["catch"], ".taxonID)='", taxon[i], "') ", sep="")
    }
}
taxon.where <- paste( taxon.where, ") " )

#   Run WHERE clause.  
run.where <- ""   # this always pulls all records - so that plus counts can be implemented.

# If run is NA, pull all records.
#if( is.na( run ) ){
#    run.where <- ""
#} else {
#    run.where <- paste( "AND ((", tables["catch"], ".finalRunID)=", run, ") ", sep="")
#}


#   To get records for a single ProjectDescriptionID (forget site), you need to do the following: You need to figure out what 
#   subsites are associated with the site, the pull all the catch records for those subsites.  There is no way to 
#   do it otherwise.  I don't understand why there 
#   is no siteID, nor why we cannot use projectDescriptionID directly, but we can't.  
#
#   This code produces
#   a bunch of OR clauses to retreive data from 1 or more specified subsites. 
#subsites <- sqlQuery(ch, paste( "SELECT siteID, subSiteName, subSiteID, projectDescriptionID FROM", tables["subsites"], 
#    "WHERE (siteID=", site, ")" ))
#F.sql.error.check(subsites)
#print(subsites)
#SubSiteID <- subsites$subSiteID
#
#subsite.string <- SubSiteID[1]
#subsite.where <- paste( "AND (((", tables["trap.visit"], ".trapPositionID)=", SubSiteID[1], ") ", sep="" )
#if( nrow(subsites) > 1 ){
#    for( i in 2:nrow(subsites)){
#        subsite.string<- paste( subsite.string, "+", SubSiteID[i], sep="" )
#        subsite.where <- paste( subsite.where, "OR ((", tables["trap.visit"], ".trapPositionID)=", SubSiteID[i], ") ", sep="")
#    }
#}
#subsite.where <- paste( subsite.where, ") " )

subsite.where <- paste( "AND ((SubSite.siteID)=", site, ") ") 


#   Fish origin WHERE clause
#fishorigin.where <- paste( "AND ((", tables["catch"], ".fishOriginID)>1) ", sep="")


#   The final SQL statement
#sql.catch <- paste( "SELECT ", visit.vars.sql, ", ", catch.vars.sql, " ",
#    "FROM ", tables["trap.visit"], " INNER JOIN ", tables["catch"], " ON (", tables["trap.visit"], ".trapVisitID = ", tables["catch"], ".trapVisitID) ",
#    "AND (", tables["trap.visit"], ".projectDescriptionID = ", tables["catch"], ".projectDescriptionID) ",
#    "WHERE (((", tables["trap.visit"], ".visitTime2)>=#", format(run.season$start, "%m/%d/%Y"), "#) ", 
#    "AND ((", tables["trap.visit"], ".visitTime2)<=#", format(run.season$end, "%m/%d/%Y"), "#) ",
#    "AND ((", tables["trap.visit"], ".visitTypeID)>1) ",
#    "AND ((", tables["trap.visit"], ".visitTypeID)<5) ",
#    "AND ((", tables["trap.visit"], ".fishProcessedID)<2) ",
#    "AND ((", tables["trap.visit"], ".trapFunctioningID)<4) ",
#    "AND ((", tables["trap.visit"], ".includeCatchID)=", Yes.code, ") ",
#    "AND ((", tables["trap.visit"], ".dataCollectedID)=", Yes.code, ") ",
#    "AND ((", tables["catch"], ".randomID)=", Yes.code, ") ",
#    taxon.where,
#    subsite.where,
#    fishorigin.where,
#    run.where,
#     ")", sep="")

#    "AND ((", tables["catch"], ".randomID)<>", No.code, ") ",

sql.catch <- paste( "SELECT ", visit.vars.sql, ", ", catch.vars.sql, " ",
    "FROM (SubSite INNER JOIN TrapVisit ON SubSite.subSiteID = TrapVisit.trapPositionID) ",
    "INNER JOIN CatchRaw ON (TrapVisit.trapVisitID = CatchRaw.trapVisitID) AND (TrapVisit.projectDescriptionID = CatchRaw.projectDescriptionID) ",
    "WHERE (((", tables["trap.visit"], ".visitTime2)>=#", format(run.season$start, "%m/%d/%Y"), "#) ", 
    "AND ((", tables["trap.visit"], ".visitTime2)<=#", format(run.season$end, "%m/%d/%Y"), "#) ",
    "AND ((", tables["trap.visit"], ".visitTypeID)>1) ",
    "AND ((", tables["trap.visit"], ".visitTypeID)<5) ",
    "AND ((", tables["trap.visit"], ".fishProcessedID)<2) ",
    "AND ((", tables["trap.visit"], ".trapFunctioningID)<4) ",
    "AND ((", tables["trap.visit"], ".includeCatchID)=", Yes.code, ") ",
    "AND ((", tables["trap.visit"], ".dataCollectedID)=", Yes.code, ") ",
    taxon.where,
    subsite.where,
    run.where,
     ")", sep="")


cat("SQL to retrieve catch records:\n")
out.sql <- gsub( ",", "\n,", sql.catch, fixed=T)
out.sql <- gsub( "FROM", "\nFROM", out.sql, fixed=T)
out.sql <- gsub( ") ", ")\n", out.sql, fixed=T)
cat(out.sql)
cat("\n")


#   Execute the final SQL statement
catch <- sqlQuery(ch, sql.catch)
F.sql.error.check(catch)

if( nrow(catch) == 0 ){
    return(catch)    
}

if(nrow(catch) >= 20) {cat("First 20 catch records...\n"); print(catch[1:20,])} else {cat("Catch records...\n"); print(catch)}

cat(paste(nrow(catch), "total records in catch table.\n")) 

#   Check for missing catches
if( any( is.na(catch$n) )){
    cat("Number of fish is missing for the following records:\n")
    print( catch[ is.na(catch$n), ] )
    stop("There are missing catches. Make sure at least 0 is entered for every count. ")

    #   Or, could replace missing "n" with 0.  
    catch$n[ is.na(catch$n) ] <- 0
}


#   Subset fish caught based on keep parameter
if( keep == "unmarked" ){
    #   Toss out the release fish.  Those that are used in release trials should not be here
    keeep <- (catch$releaseID == 0) | (catch$releaseID >= 200)
    cat(paste(sum(!keeep), "efficiency trial records dropped\n"))
    cat("Efficiency trail ID's found:\n")
    print( unique(catch$releaseID[!keeep]))
    catch <- catch[keeep,]
} else if( keep == "marked" ){
    #   Toss out the unmarked fish.  
    keeep <- (catch$releaseID != 0) & (catch$releaseID < 200)
    cat(paste(sum(!keeep), "fish not involved in efficiency trial dropped\n"))
    catch <- catch[keeep,]
}

#   Drop the rows with 0 counts.  These 0 count rows can mess up the plus count computations, plus they take up space. 
catch <- catch[ catch$n > 0, ]



#   Expand for the Plus Counts
catch <- F.expand.plus.counts( catch )


#   Now, subset to run. We cannot do this in the SQL above because we need unknown runs in order to expand for plus counts
catch <- catch[ !is.na(catch$finalRunID) & catch$finalRunID == run, ]

#   Find subsiteID names

subSites.found <- data.frame(subSiteID=sort(unique(catch$trapPositionID)))
subSites.found <- merge( subSites.found, subsite.names, by="subSiteID", all.x=T )

cat("Subsites found...\n")
print(subSites.found)
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
attr(catch, "subsites") <- subSites.found
attr(catch, "taxonID" ) <- taxon.string
attr(catch, "species.name") <- sp.commonName
attr(catch, "runID") <- run
attr(catch, "run.name") <- run.name
attr(catch, "run.season") <- run.season


#   If there are none of the particular taxon caught, there are no records in data frame catch. 
#   This is correct, just be sure to check for nrow(catch) > 0 in any routines that use this data.

odbcClose(ch)

catch

}

