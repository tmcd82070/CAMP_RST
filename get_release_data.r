F.get.release.data <- function( site, taxon, min.date, max.date ){
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


#   There is some differences of opinion on whether taxon should be included here.  Mike convinced me that we did not need to. But, 
#   Connie's SQL, which we converted to uses taxon.  I will go with Connie's SQL and use taxon. 


#   *****  
#   Run report criteria for trap visits. Builds TempReportCriteria_Trapvisit.
nvisits <- F.buildReportCriteria( site, min.date, max.date )

if( nvisits == 0 ){
    warning("Your criteria returned no trap visits.")
    return()
}


#   *****
#   Run report criteria for efficiency releases. Builds TempReportCriteria_Release.
nreleases <- F.buildReportCriteriaRelease( site, min.date, max.date )

if( nreleases == 0 ){
    warning("Your criteria returned no releases.")
    return()
}


#   *****
#   Open ODBC channel
db <- get( "db.file", env=.GlobalEnv ) 
ch <- odbcConnectAccess(db)

#   *****
#   This SQL file develops the TempSamplingSummary table 
F.run.sqlFile( ch, "QrySamplePeriod.sql", R.TAXON=taxon )  


#   *****
#   This SQL file develops the hours fished and TempSamplingSummary table 
F.run.sqlFile( ch, "QryEfficiencyTests.sql", R.TAXON=taxon )   


#   *****
#   Now, fetch the result
release.visit <- sqlFetch( ch, "TempRelRecap_final" )
F.sql.error.check(release.visit)

close(ch) 

#   In release.visit, there is one record for every trap visit within releaseTime (e.g., 7 days) 
#   after each release, even if the trap visit did not catch any marked fish.  i.e., the 0's are in here
#   because all combinations of (releaseID, trapPositionID, trapVisitID) upon which marked fish could have
#    been cause are here.
#   
#   At times, more than one release was "going", and a single trap visit 
#   could have caught fish from multiple releases.
#
# For a specific release, release.visit tells 
#   how many fish from the release were captured on subsequent trap visits to each trap.  In future,
#   we may want to do something fancy like a removal estimator or other analysis
#   that requires recapture information through time (over multiple visits). If so, You will want to use 
#   this data frame (release.visit). 
#
#   However, for now, we will collapse the trap visits and compute total number of each release's fish captured ever.  
tmp <<- release.visit 

#   *****
#   Drop any rows that are flagged as "Do not include"
release.visit <- release.visit[ (release.visit$IncludeTest == "Yes") & (release.visit$IncludeCatch == "Yes"), ]



#   Sum over trapVisits at a trapPosition

by.list <- list(trapPositionID = release.visit$trapPositionID, 
            releaseID = release.visit$releaseID )

ind  <- tapply( release.visit$Recaps, by.list, FUN=NULL)

u.groups <- sort(unique(ind))
ans <- NULL
for( g in u.groups ){
    tmp <- release.visit[ ind == g, ]
    one.row <- tmp[1,]

    #   Number caught
    one.row$Recaps <- sum(tmp$Recaps, na.rm=T)

    #   Compute time to first and last visit after release, even if they did not catch any marked fish
    tmp.hdiff <- as.numeric( difftime(tmp$VisitTime, tmp$ReleaseDate, units="hours") )
    one.row$HrsToFirstVisitAfter <-  min( tmp.hdiff ) 
    one.row$HrsToLastVisitAfter <-  max( tmp.hdiff )
    
    #   Mean fork length of released fish that were captured
    if( one.row$Recaps == 0 ){
        # This case should not happen
        #one.row$meanForkLength <- NA
        one.row$meanRecapTime <- NA
        one.row$meanTimeAtLargeHrs <- NA
    } else {
        #one.row$meanForkLength <- sum(tmp$n * tmp$forkLengthMM, na.rm=T) / one.row$n
    
        #   Mean time released fish were caught
        tmp.v <- as.numeric(tmp$VisitTime)
        one.row$meanRecapTime <- sum(tmp.v * tmp$Recaps, na.rm=T) / one.row$Recaps
        
        #   Mean time at large, in hours
        one.row$meanTimeAtLargeHrs <- sum(tmp.hdiff * tmp$Recaps, na.rm=T) / one.row$Recaps

    }
    

    one.row <- one.row[,-which( names(one.row) %in% c("VisitTime", "trapVisitID", "SampleMinutes")) ]   # drop columns that are we are summing over
    
    
    ans <- rbind(ans, data.frame(one.row))
}    
 
class(ans$meanRecapTime) <- class(release.visit$VisitTime)
attr(ans$meanRecapTime, "tzone") <- attr(release.visit$VisitTime, "tzone")

    
cat("First 20 records of RELEASE table:\n")  
print( ans[1:min(nrow(ans),20),c("releaseID", "ReleaseDate", "TrapPosition", "trapPositionID", "nReleased",   "Recaps", "meanRecapTime", "meanTimeAtLargeHrs")] )


#   Store values of some header info as attribute for convienance. 
attr(ans, "taxonID" ) <- taxon
#attr(ans, "species.name") <- sp.commonName
attr(ans, "siteID" ) <- site
#attr(ans, "site.stream" ) <- site.stream
#attr(ans, "site.name") <- site.name
#attr(ans, "site.abbr") <- site.abbr  
#attr(ans, "runID") <- run
#attr(ans, "run.name") <- run.name
#attr(ans, "run.season") <- run.season



ans

}

