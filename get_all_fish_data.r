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

#print(sites)

#   *******
#   Fetch species name
sp.codes <- sqlQuery(ch, paste("SELECT taxonID, commonName FROM", tables["species.codes"]))
F.sql.error.check(sp.codes)

#print(sp.codes)

#   *******
#   Fetch run name
runs <- sqlQuery(ch, paste( "SELECT run, runID FROM", tables["run.codes"] ))
F.sql.error.check(runs)

#print(runs)


#   *******
#   Fetch fish origin names
origins <- sqlQuery(ch, paste( "SELECT fishOriginID, fishOrigin FROM", tables["fish.origin"] ))
F.sql.error.check(runs)

#print(origins)

#   **********************************************************************************************
#   SQL for CATCH


visit.vars <- c("projectDescriptionID",
                "trapPositionID",
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
                "forkLength" )
catch.vars.sql <- F.vec2sqlstr(tables["catch"], catch.vars)

catch.vars.sql <- paste( catch.vars.sql, ", [CatchRaw].[n]*[CatchRaw].[subsampleDenominator]/[CatchRaw].[subsampleNumerator] AS n", sep="")




#   The SQL statement
sql.catch <- paste( "SELECT ", visit.vars.sql, ", ", catch.vars.sql, ", SubSite.siteID", " ",
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
    "AND ((", tables["subsites"], ".siteID)=", site, ") ", 
     ")", sep="")


cat("SQL to retrieve ALL catch records between ")
cat(paste(min.date, "and", max.date, "\n"))
out.sql <- gsub( ",", "\n,", sql.catch, fixed=T)
out.sql <- gsub( "FROM", "\nFROM", out.sql, fixed=T)
out.sql <- gsub( ") ", ")\n", out.sql, fixed=T)
cat(out.sql)
cat("\n")


#   Execute the final SQL statement
catch <- sqlQuery(ch, sql.catch)
F.sql.error.check(catch)

if(nrow(catch) >= 20) {cat("First 20 catch records...\n"); print(catch[1:20,])} else {cat("Catch records...\n"); print(catch)}

cat(paste(nrow(catch), "total records in catch table.\n"))

#   Check for missing catches
if( any( is.na(catch$n) )){
    cat("Number of fish is missing for the following records:\n")
    print( catch[ is.na(catch$n), ] )
    stop("There are missing catches. Make sure at least 0 is entered for every count. ")
}



#   Drop the rows with 0 counts.  These 0 count rows can mess up the plus count computations, plus they take up space.
catch <- catch[ catch$n > 0, ]

#   Drop all marked fish
keep <- (catch$releaseID == 0) | (catch$releaseID == 255)
cat(paste(sum(!keep), "efficiency trial records dropped\n"))
catch <- catch[keep,]


#   DO NOT Expand for the Plus Counts
#catch <- F.expand.plus.counts( catch )



cat("Subsites found...\n")
subSites.found <- sort(unique(catch$trapPositionID))
print(subSites.found)
subsite.string <- paste(subSites.found, collapse="+")


#   Subsampling is handled above in the SQL with [CatchRaw].[n]*[CatchRaw].[subsampleDenominator]/[CatchRaw].[subsampleNumerator]



#   Merge in the values for the look up codes
catch <- merge( catch, sites, by="siteID", all.x=T )
#print(catch[1:10,])
catch <- merge( catch, sp.codes, by="taxonID", all.x=T )
#print(catch[1:10,])
catch <- merge( catch, runs, by.x="finalRunID", by.y="runID", all.x=T )
#print(catch[1:10,])
catch <- merge( catch, origins, by="fishOriginID", all.x=T )
print(catch[1:10,])

catch$siteName <- as.character( catch$siteName )
catch$siteAbbreviation <- as.character( catch$siteAbbreviation )
catch$streamName <- as.character( catch$streamName )
catch$commonName <- as.character( catch$commonName )
catch$run <- as.character( catch$run )
catch$fishOrigin <- as.character( catch$fishOrigin )



odbcClose(ch)

catch

}

