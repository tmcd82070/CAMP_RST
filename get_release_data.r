F.get.release.data <- function( site, run, min.date, max.date ){
#
#   Fetch the release data from an Access data base. Do some initial
#   computations, like dates.
#
#   input:
#   site = site ID of place we want to do estimates for. 
#   run = run ID of fish we want to do estimates for.  This is only used to find trap visits which 
#       might have caught released fish.  I.e., it is only used in a call to F.get.indiv.visit.data below.
#   min.date = minimum date for data to include. This is a text string in the format %Y-%m-%d, or YYYY-MM-DD
#   max.date = maximum date for data to include.  Same format as min.date


#   I used to use taxon and run here, but Mike convinced me that we did not need to. If you add these back in, 
#   you can uncomment a few lines below.
##   taxon = taxon number (from luTaxon) to retrieve   NOT USED
##       get run.season.  Released fish are not assigned a runID.   NOT USED

f.banner <- function( x ){

    cat("\n")
    cat(paste(rep("=",50), collapse="")); 
    cat(x); 
    cat(paste(rep("=",50), collapse="")); 
    cat("\n")
}

f.banner("F.get.release.data - START")


#   *******
#   Retrieve db file name and table names and any other constants
No.code <- get("No.code", pos=.GlobalEnv)
Yes.code <- get("Yes.code", pos=.GlobalEnv)
tables <- get( "table.names", env=.GlobalEnv )
db <- get( "db.file", env=.GlobalEnv ) 


#   *******
#   First, deterimine the start and stop dates of the run. 
#   Need these to determine visits.
#   Need these to filter visits.
strt.dt <- as.POSIXct( min.date, format="%Y-%m-%d" )
end.dt <- as.POSIXct( max.date, format="%Y-%m-%d" )
run.season <- data.frame( start=strt.dt, end=end.dt )


#   *******
#   Open channel to data base
ch <- odbcConnectAccess(db)


#   ----  Fetch species name and site name lookup table
site.info <- sqlQuery( ch, paste("SELECT siteName, siteAbbreviation, streamName FROM", tables["sites"], 
        "WHERE (siteID =", site, ")" ))
        
F.sql.error.check(site.info)

site.name <- as.character(site.info$siteName)
site.stream <- as.character(site.info$streamName)
site.abbr <- as.character(site.info$siteAbbreviation)


sp.codes <- sqlQuery(ch, paste("SELECT taxonID, commonName FROM", tables["species.codes"]))
F.sql.error.check(sp.codes)
sp.commonName <- as.character(sp.codes$commonName[ sp.codes$taxonID %in% taxon ])


runs <- sqlQuery(ch, paste( "SELECT run, runID FROM", tables["run.codes"] ))
F.sql.error.check(runs)
run.name <- as.character(runs$run[ runs$runID == run ])




cat( paste(site.abbr, site.name, site.stream, sep=":") )
cat("\n")
cat( paste(sp.codes$taxonID[ sp.codes$taxonID %in% taxon ], sp.commonName, run.name, sep=":") )
cat("\n")

cat(paste("Site = ", site, "\n"))
cat(paste("Start date = ", run.season$start, "\n"))
cat(paste("End date = ", run.season$end, "\n"))


#   You need to convert over and use some version of this SQL statement.  It is much simpliler than the one below. 
#   But, you ran out of time in Dec 2012, and went with the hairy one below.
#release.recap.sql <- "SELECT ReleaseXTargetSite.projectDescriptionID, 
#   SubSite.siteID, 
#    SubSite.subSiteID AS trapPositionID, 
#    CatchRaw.catchRawID, 
#    Release.releaseID, 
#    Release.nReleased, 
#    Release.releaseTime, 
#    Release.testDays, 
#    CatchRaw.n, 
#    TrapVisit.visitTime, 
#    TrapVisit.visitTime2, 
#    CatchRaw.taxonID
#FROM TrapVisit 
#    RIGHT JOIN ((Release 
#        LEFT JOIN CatchRaw ON 
#            (Release.releaseID = CatchRaw.releaseID) AND (Release.projectDescriptionID = CatchRaw.projectDescriptionID)) 
#        INNER JOIN (ReleaseXTargetSite 
#        INNER JOIN SubSite ON ReleaseXTargetSite.targetSiteID = SubSite.siteID) ON 
#            (Release.releaseID = ReleaseXTargetSite.releaseID) AND (Release.projectDescriptionID = ReleaseXTargetSite.projectDescriptionID)) ON 
#            (TrapVisit.trapVisitID = CatchRaw.trapVisitID) AND (TrapVisit.projectDescriptionID = CatchRaw.projectDescriptionID)
#WHERE 
#    (((SubSite.siteID)=1000) 
#    AND ((Release.nReleased) Is Not Null) 
#    AND ((CatchRaw.taxonID)="161980" Or (CatchRaw.taxonID) Is Null));

######

#   These are the minimum set of variables that the release data frame needs (in est_efficiency.r)
# "releaseID", "releaseTime", "nReleased", "testDays",    "trapPositionID", "n", "recapTime"

sql.part1 <-
"SELECT Site.siteName AS ReleaseSite
, SubSite.subSiteName AS ReleaseSubSite
, SubSite.siteID AS ReleaseSiteID
, ProjectDescription.projectName AS Project
, Release.projectDescriptionID
, Release.releaseID
, luReleasePurpose.releasePurpose AS ReleasePurpose
, luTaxon.commonName AS markedTaxon
, Release.markedTaxonID AS markedTaxonID
, luRun.run AS markedRun
, Release.markedRunID AS markedRunID
, luLifeStage.lifeStage AS markedLifeStage
, Site_1.siteName AS SourceOfFish
, Release.checkedTime AS PrereleaseCheckTime
, Release.nMortWhileHandling AS MortsWhileHandling
, Release.nMortAtCheck AS MortsAtCheck
, Release.nMarkExamined AS NumberOfMarksExamined
, Release.nMarkNotRetained AS NumberOfMarksMissing
, Release.propMarked AS ProportionMarked
, Release.nReleased AS nReleased
, Release.releaseTime AS releaseTime
, luLightCondition.lightCondition AS ReleaseLightConditions
, Release.testDays AS testDays
, luNoYes.noYes AS IncludeTest
, Release.dataSheetNumber AS ReleaseDataSheetNumber
, Site_2.siteName AS RecapSite
, Site_2.siteID AS RecapSiteID
, SubSite_1.subSiteName AS RecapSubsite
, SubSite_1.subSiteID AS trapPositionID
, TrapVisit.visitTime AS recapTime
, TrapVisit.trapVisitID AS trapVisitID
, Sum([CatchRaw].[n]*[CatchRaw].[subsampledenominator]/[CatchRaw].[subsamplenumerator]) AS n
, CatchRaw.forkLength AS forkLengthMM
FROM 
    (Site AS Site_2 RIGHT JOIN 
    (SubSite AS SubSite_1 RIGHT JOIN 
    TrapVisit 
ON SubSite_1.subSiteID = TrapVisit.trapPositionID) 
ON Site_2.siteID = SubSite_1.siteID) 
RIGHT JOIN (SubSite 
RIGHT JOIN (Site 
INNER JOIN ((ProjectDescription 
INNER JOIN (luTaxon 
INNER JOIN (luRun 
RIGHT JOIN (luReleasePurpose 
RIGHT JOIN (luNoYes 
RIGHT JOIN (luLightCondition 
RIGHT JOIN (luLifeStage 
RIGHT JOIN (Site AS Site_1 
RIGHT JOIN Release 
ON Site_1.siteID = Release.sourceOfFishSiteID) 
ON luLifeStage.lifeStageID = Release.markedLifeStageID) 
ON luLightCondition.lightConditionID = Release.releaseLightConditionID) 
ON luNoYes.noYesID = Release.includeTestID) 
ON luReleasePurpose.releasePursposeID = Release.releasePurposeID) 
ON luRun.runID = Release.markedRunID) 
ON luTaxon.taxonID = Release.markedTaxonID) 
ON ProjectDescription.projectDescriptionID = Release.projectDescriptionID) 
LEFT JOIN CatchRaw 
ON (Release.releaseID = CatchRaw.releaseID) AND (Release.projectDescriptionID = CatchRaw.projectDescriptionID)) 
ON Site.siteID = Release.releaseSiteID) 
ON SubSite.subSiteID = Release.releaseSubSiteID) 
ON (TrapVisit.trapVisitID = CatchRaw.trapVisitID) AND (TrapVisit.projectDescriptionID = CatchRaw.projectDescriptionID)"

sql.where <- paste(
    "WHERE ((luNoYes.noYes)= 'Yes') ", 
    "\n AND ((Release.releaseTime)>=#", format(run.season$start, "%m/%d/%Y %H:%M:%S"), "#) ", 
    "\n AND ((Release.releaseTime)<=#", format(run.season$end, "%m/%d/%Y %H:%M:%S"), "#) ", 
    "\n AND ((Site_2.siteID)= ", site, ") ", 
#    "\n AND ((Release.markedTaxonID)='", taxon, "') ",
#    "\n AND ((Release.markedRunID)=", run, ") ", 
sep="")

sql.part2 <-
"GROUP BY Site.siteName
, SubSite.subSiteName
, SubSite.siteID
, ProjectDescription.projectName
, Release.projectDescriptionID
, Release.releaseID
, luReleasePurpose.releasePurpose
, luTaxon.commonName
, Release.markedTaxonID
, luRun.run
, Release.markedRunID
, luLifeStage.lifeStage
, Site_1.siteName
, Release.checkedTime
, Release.nMortWhileHandling
, Release.nMortAtCheck
, Release.nMarkExamined
, Release.nMarkNotRetained
, Release.propMarked
, Release.nReleased
, Release.releaseTime
, luLightCondition.lightCondition
, Release.testDays
, luNoYes.noYes
, Release.includeTestComments
, Release.comments
, Release.dataSheetNumber
, Site_2.siteName
, Site_2.siteID
, SubSite_1.subSiteName
, SubSite_1.subSiteID 
, TrapVisit.visitTime
, TrapVisit.trapVisitID
, CatchRaw.forkLength

HAVING (((Release.releaseID)<>0 And (Release.releaseID)<>255))
"

release.recap.sql <- paste( sql.part1, sql.where, sql.part2, sep="\n" )

cat("\nRelease and recapture SQL statement: \n")
#out.sql <- gsub( ",", "\n,", release.recap.sql, fixed=T)
#out.sql <- gsub( "FROM", "\nFROM", release.recap.sql, fixed=T)
#out.sql <- gsub( ") ", ")\n", out.sql, fixed=T)
cat(release.recap.sql)
cat("\n")

catch <- sqlQuery(ch,  release.recap.sql )
F.sql.error.check(catch)

if( nrow(catch) == 0 ){
    cat(paste("\nNO EFFICIENCY TRIALS FOUND at site", site, "between", run.season$start, "and",  run.season$end, "\n"))
    ans <- data.frame(n=0)
    return(ans)
}

print(catch[1:min(nrow(catch),20),])

#   I am pretty sure missing combinations of the aggregate variables return trapPositionID = NA.  Drop these.
cat(paste("Number of missing trapPositionID's =", sum(is.na(catch$trapPositionID)), "\n"))
catch <- catch[ !is.na(catch$trapPositionID), ]

attr(catch$recapTime, "tzone") <- "America/Los_Angeles"
attr(catch$releaseTime, "tzone") <- "America/Los_Angeles"

cat(paste(nrow(catch), " records in raw release and recapture data frame.\n"))        

cat("Release ID's found:\n")
print( unique(catch$releaseID) )

##   If you want to Summarize catches by release and trapvisit, uncomment this code down to #!!!
#by.list <- list( releaseID=catch$releaseID, trapVisitID=catch$trapVisitID )
#
#tmp.n <- tapply( catch$n, by.list, sum, na.rm=T )
#by.grid <- expand.grid(releaseID=as.numeric(dimnames(tmp.n)[[1]]), trapVisitID=as.numeric(dimnames(tmp.n)[[2]]))
#
#
#tmp.n <- c(tmp.n)
#ind <- !is.na(tmp.n)
#
#tmp.l <- catch$forkLengthMM * catch$n 
#tmp.l <- tapply( tmp.l, by.list, sum, na.rm=T )  #numerator of weighted average (fork length weighted by count of fish at that length)
#tmp.l <- c(tmp.l)
#tmp.l <- tmp.l[ind] / tmp.n[ind]
#
#tmp.d <- tapply( catch$recapTime, by.list, function(x){x[1]} )
#tmp.d <- c(tmp.d)[ind]
#class(tmp.d) <- class(catch$recapTime)
#attr(tmp.d, "tzone") <- attr(catch$recapTime, "tzone")
#
#tmp.R <- tapply( catch$nReleased, by.list, function(x){x[1]} )
#tmp.R <- c(tmp.R)
#
#tmp.tp <- tapply( catch$trapPositionID, by.list, function(x){if(length(unique(x))>1){print(unique(x))}; x[1]} )
#tmp.tp <- c(tmp.tp)
# 
#
#release.visit <<- data.frame( by.grid[ind,], 
#        recapTime=tmp.d, 
#        trapPositionID = tmp.tp[ind],
#        nReleased=tmp.R[ind],
#        n=tmp.n[ind], 
#        meanForkLength=tmp.l )
#
#
##   Merge all combinations of releaseID and trapPositionID so we can make sure to record all the 0 catches
#pos.rel <- expand.grid( trapPositionID = unique(catch$trapPositionID), releaseID=unique(catch$releaseID)) 
#release.visit <- merge( release.visit, pos.rel, by=c("releaseID", "trapPositionID"), all=T )
#
#
#!!!
#
#
#
#
##   NOTE: the just-created data frame, 'release.visit', has one row per releaseID X trapVisitID combination.  This is equivalent to 
##   one row for each combination of (releaseID, trapPositionID, visitTime).  For a specific release, this data frame tells 
##   how many fish from the release were captured on subsequent trap visits.  In future,
##   we may want to do something fancy like a removal estimator or other analysis
##   that requires recapture information through time (over multiple visits). If so, You will want to use 
##   this data frame (release.visit). Note
##   also that there are covariates in CatchRaw that we could potentially use, like
##   fork length, fishOriginID, taxonID of the recaptured fish.   
##
##   However, for now, we will collapse the trap visits and compute total number of each release's fish captured ever.  




##   The above SQL (table catch), gives us one row for every marked fish that was captured.  
##   However, if no marked fish were captured from a particular release, does it show up in this table (table catch)?
##   Do we have to find out which trapPositionID's were operating during each of the releases?  To do this
##   we would pull from the releaseXsite table to determine which subsites (trapPositions) were targeted by each 
##   release. 
#
#   I don't think so.  I think even the missing catches are here.
#
##   First, find all the releases targeted at our site.
#releaseXtarget.sql <- paste("SELECT Release.projectDescriptionID, Release.releaseID, ReleaseXTargetSite.targetSiteID
#FROM Release 
#INNER JOIN ReleaseXTargetSite ON (Release.releaseID = ReleaseXTargetSite.releaseID) 
#AND (Release.projectDescriptionID = ReleaseXTargetSite.projectDescriptionID)
#WHERE (((ReleaseXTargetSite.targetSiteID)=", site, "))", sep="")
#
#cat("\nRelease X TargetSite SQL statement: \n")
#cat(releaseXtarget.sql)
#cat("\n")
#
#releaseXtarget <<- sqlQuery(ch,  releaseXtarget.sql )
#F.sql.error.check(releaseXtarget)
#cat(paste(nrow(releaseXtarget), " records in releaseXTargetSite data frame.\n\n\n"))        
#
#
##   Next, find all the visits and associated subsites (trapPositions) that were operating during releases that targeted our site.
#visits <<- F.get.indiv.visit.data( site, run, min.date, max.date )
#
#
#print(sort(unique(visits$trapPositionID)))
#
#
###here!!! decide whether you need the visits data frame.  Decide how to use releaseXtarget table. Reuse some of your old code. 
#
#
#
#   Merge in release size information
#release.visit <- merge( release, release.catch, by="releaseID", all.x=T )
#there <- !is.na(release.visit$n)
#release.visit$n[ !there ] <- 0
#
#cat("First 20 records of RELEASE x VISIT table:\n")
#print( release.visit[1:20,] )
#
#
#
#
#
#
######
#   MY ORIGINAL CODE BELOW HERE.
#
#   ----------------------------------------------------------------------------------------------
#cat("Retrieving RELEASE information...\n")
#sql.release <-  paste( "SELECT releaseID, releaseTime, nReleased, releaseSiteID, releaseSubSiteID, testDays",
#        " FROM ", tables["release"], 
#        " WHERE (((", tables["release"], ".releaseTime)>=#", format(run.season$start, "%m/%d/%Y %H:%M:%S"), "#)", 
#        " AND ((", tables["release"], ".releaseTime)<=#", format(run.season$end, "%m/%d/%Y %H:%M:%S"), "#)", 
#        " AND ((", tables["release"], ".includeTestID)=", Yes.code, ")", 
#        " AND ((", tables["release"], ".projectDescriptionID)=", site, "))", sep="")  
#        
#cat(gsub( ") ", ")\n", sql.release, fixed=T))
#cat("\n")
#        
#        
#release <- sqlQuery(ch,  sql.release )
#F.sql.error.check(release)
#cat("Releases found:\n")
#print(release)
#cat(paste(nrow(release), "release records.\n"))        
#
#
#
##   ---- Bomb out if no releases
#if( nrow(release) == 0 ){
#    #   No releases in data base
#    ans <- data.frame( releaseID=NA, releaseTime=NA, nReleased=NA, releaseSiteID=NA, releaseSubSiteID=NA, 
#            testDays=NA, trapPositionID=NA, n=NA, meanForkLength=NA, meanVisitTime=NA, meanTimeAtLargHrs=NA)
# 
#    cat("First 20 records of RELEASE table:\n")
#    print( ans )
#            
#    return(ans)
#}    
#
#
##   ----------------------------------------------------------------------------------------------
##   Query the visits
#f.banner("Retrieving VISITS to match releases")
#
#visit <- F.get.indiv.visit.data( site, run, min.date, max.date )
#
#
#
#
#
##   ----------------------------------------------------------------------------------------------
##   Query the catch information
#
#f.banner(" Retrieving CATCH of marked fish ")
#catch <- F.get.indiv.fish.data( site, taxon, run, min.date, max.date, keep="marked" )
#
#odbcClose(ch)
#
#
##   ----------------------------------------------------------------------------------------------
##   Summarize catch info by trapVisitID and releaseID = how many of each release were caught during each subsequent visit?
#
#f.banner(" Sumarizing recaptures ")        
#
#if( nrow(catch) == 0 ){
#    #   No released fish were ever caught => 0 efficiency
#    #   Bomb out.
#    na <- rep(NA, nrow(release))
#    ans <- cbind( release, trapPositionID=na, n=rep(0,nrow(release)), meanForkLength=na, meanVisitTime=na, meanTimeAtLargHrs=na )
#
#    class(ans$meanVisitTime) <- class(visit$visitTime)
#    attr(ans$meanVisitTime, "tzone") <- attr(visit$visitTime, "tzone")
#    attr(ans$releaseTime, "tzone") <- attr(visit$visitTime, "tzone")
#     
#    cat("NO RELEASED FISH CAUGHT\n") 
#    cat("First 20 records of RELEASE table:\n")
#    print( ans[1:min(20, nrow(ans)),] )
#
#    return(ans)
#}    
#
#
#
##   Summarize catches by release and visit
#by.list <- list( releaseID=catch$releaseID, trapVisitID=catch$trapVisitID )
#
#tmp.n <- tapply( catch$n, by.list, sum, na.rm=T )
#by.grid <- expand.grid(releaseID=dimnames(tmp.n)[[1]], trapVisitID=dimnames(tmp.n)[[2]])
#
#tmp.n <- c(tmp.n)
#ind <- !is.na(tmp.n)
#
#tmp.l <- catch$forkLength * catch$n 
#tmp.l <- tapply( tmp.l, by.list, sum, na.rm=T )  #numerator of weighted average (fork length weighted by count of fish at that length)
#tmp.l <- c(tmp.l)
#tmp.l <- tmp.l[ind] / tmp.n[ind]
#
#tmp.d <- tapply( catch$visitTime, by.list, function(x){x[1]} )
#tmp.d <- c(tmp.d)[ind]
#class(tmp.d) <- class(catch$visitTime)
#attr(tmp.d, "tzone") <- attr(catch$visitTime, "tzone")
#
#release.catch <- data.frame( by.grid[ind,], visitTime=tmp.d, n=tmp.n[ind], meanForkLength=tmp.l )
#
#
#
#
##   Merge in trapPositionID to release.catch so can sum over it
#ind <- !duplicated( paste(catch$trapPositionID, catch$trapVisitID) )
#tmp <- catch[ind,c("trapPositionID","trapVisitID")]    # unique combinations of trapPositionID and trapVisitID
#release.catch <- merge(release.catch, tmp, by="trapVisitID" , all.x=T)  
#release.catch$releaseID <- as.numeric(as.character(release.catch$releaseID))
#
##   Merge all combinations of releaseID and trapPositionID so we can make sure to record all the 0 catches
#pos.rel <- expand.grid( trapPositionID = unique(release.catch$trapPositionID), releaseID=unique(release$releaseID)) 
#release.catch <- merge( release.catch, pos.rel, by=c("releaseID", "trapPositionID"), all=T )
#
#
##   Merge in release size information
#release.visit <- merge( release, release.catch, by="releaseID", all.x=T )
#there <- !is.na(release.visit$n)
#release.visit$n[ !there ] <- 0
#
#cat("First 20 records of RELEASE x VISIT table:\n")
#print( release.visit[1:20,] )
#
##tmp.v <<- release.visit
#


#f.banner(" Collapsing visits for mean efficiency ")

by.list <- list(trapPositionID = catch$trapPositionID, 
            releaseID = catch$releaseID )

ind  <- tapply( catch$n, by.list, FUN=NULL)

u.groups <- unique(ind)
ans <- NULL
for( g in u.groups ){
    tmp <- catch[ ind == g, ]
    one.row <- tmp[1,]

    #   Number caught
    one.row$n <- sum(tmp$n, na.rm=T)
    
    #   Mean fork length of released fish that were captured
    if( one.row$n == 0 ){
        one.row$meanForkLength <- NA
        one.row$meanRecapTime <- NA
        one.row$meanTimeAtLargeHrs <- NA
    } else {
        one.row$meanForkLength <- sum(tmp$n * tmp$forkLengthMM, na.rm=T) / one.row$n
    
        #   Mean time released fish caught
        tmp.v <- as.numeric(tmp$recapTime)
        one.row$meanRecapTime <- sum(tmp.v * tmp$n, na.rm=T) / one.row$n
        
        #   Mean time at large, in hours
        tmp.td <- as.numeric(tmp$recapTime) - as.numeric(tmp$releaseTime)
        one.row$meanTimeAtLargeHrs <- sum(tmp.td * tmp$n, na.rm=T) / (one.row$n * (60*60))

        #   Could compute SD of forkLength here.  (must use weighted SD formula)
    }

    one.row <- one.row[,-which( names(one.row) %in% c("trapVisitID", "recapTime", "forkLengthMM")) ]   # drop columns that are not constant over 'by' indicies
    
    
    ans <- rbind(ans, data.frame(one.row))
}    
 
class(ans$meanRecapTime) <- class(catch$recapTime)
attr(ans$meanRecapTime, "tzone") <- attr(catch$recapTime, "tzone")

 
ans$trapPositionID[ is.na(ans$trapPositionID) ] <- unique(ans$trapPositionID)[1]
ans$n[ is.na(ans$n) ] <- 0 
    
cat("First 20 records of RELEASE table:\n")   # All variables in catch are in ans, but don't print them
print( ans[1:min(nrow(ans),20),c("releaseID", "releaseTime", "nReleased", "testDays",    "trapPositionID", "n", "meanRecapTime")] )


#   Store values of some header info as attribute for convienance. 
attr(ans, "taxonID" ) <- taxon
attr(ans, "species.name") <- sp.commonName
attr(ans, "siteID" ) <- site
attr(ans, "site.stream" ) <- site.stream
attr(ans, "site.name") <- site.name
attr(ans, "site.abbr") <- site.abbr  
attr(ans, "runID") <- run
attr(ans, "run.name") <- run.name
attr(ans, "run.season") <- run.season

f.banner(" F.get.release.data - COMPLETE ")

#warning("In get_release_data, check for zero released fish caught")

ans

}

