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
#   This SQL file develops the hours fished and TempSamplingSummary table 
F.run.sqlFile( ch, "QryEfficiencyTests.sql", R.TAXON=taxon )   

#   *****
#   Now, fetch the result
release.visit <- sqlFetch( ch, "TempRelRecap_final" )
F.sql.error.check(release.visit)

#   *****
#   Drop any rows that are flagged as "Do not include"
release.visit$IncludeCatch[ is.na(release.visit$IncludeCatch) ] <- "Yes"   # could have release, but no recaptures.  IncludeCatch is <na> for these.  We want to include them.
release.visit <- release.visit[ (release.visit$IncludeTest == "Yes") & (release.visit$IncludeCatch == "Yes"), ]

#   *****
#   Put in a valid trapPositionID for any missing catches.  Any trapPosition will work.  The catch for this trap is 0.  Later, catches at other trapPositions are added as zeros.
ind <- which( !is.na(release.visit$trapPositionID) )[1]
mis <- which( is.na(release.visit$trapPositionID) )
release.visit$trapPositionID[ mis  ] <- release.visit$trapPositionID[ ind ]
release.visit$TrapPosition[ mis  ] <- release.visit$TrapPosition[ ind ]
release.visit$RST[ mis  ] <- release.visit$RST[ ind ]
release.visit$HalfCone[ mis  ] <- release.visit$HalfCone[ ind ]


#
##   NOTE: the data frame , 'release.visit', has one row per releaseID X trapVisitID combination.  This is equivalent to 
##   one row for each combination of (releaseID, trapPositionID, visitTime).  For a specific release, this data frame tells 
##   how many fish from the release were captured on subsequent trap visits.  In future,
##   we may want to do something fancy like a removal estimator or other analysis
##   that requires recapture information through time (over multiple visits). If so, You will want to use 
##   this data frame (release.visit). 
##
##   However, for now, we will collapse the trap visits and compute total number of each release's fish captured ever.  



by.list <- list(trapPositionID = release.visit$trapPositionID, 
            releaseID = release.visit$releaseID )

ind  <- tapply( release.visit$Recaps, by.list, FUN=NULL)

u.groups <- unique(ind)
ans <- NULL
for( g in u.groups ){
    tmp <- release.visit[ ind == g, ]
    one.row <- tmp[1,]

    #   Number caught
    one.row$Recaps <- sum(tmp$Recaps, na.rm=T)
    
    #   Mean fork length of released fish that were captured
    if( one.row$Recaps == 0 ){
        #one.row$meanForkLength <- NA
        one.row$meanRecapTime <- NA
        one.row$meanTimeAtLargeHrs <- NA
    } else {
        #one.row$meanForkLength <- sum(tmp$n * tmp$forkLengthMM, na.rm=T) / one.row$n
    
        #   Mean time released fish were caught
        tmp.v <- as.numeric(tmp$VisitTime)
        one.row$meanRecapTime <- sum(tmp.v * tmp$Recaps, na.rm=T) / one.row$Recaps
        
        #   Mean time at large, in hours
        if( is.na(tmp$VisitTime[1]) ){
            one.row$meanTimeAtLargeHrs <- Inf
        } else {
            tmp.v <- as.numeric(difftime(tmp$VisitTime, tmp$ReleaseDate, units="hours"))
            one.row$meanTimeAtLargeHrs <- sum(tmp.v * tmp$Recaps, na.rm=T) / one.row$Recaps
        }

    }

    one.row <- one.row[,-which( names(one.row) %in% c("VisitTime")) ]   # drop columns that are we are summing over
    
    
    ans <- rbind(ans, data.frame(one.row))
}    
 
class(ans$meanRecapTime) <- class(release.visit$VisitTime)
attr(ans$meanRecapTime, "tzone") <- attr(release.visit$VisitTime, "tzone")

#   Add in a 'cell' in the table when a trap position did not catch a single fish from a particular release 
ans$trapPositionID[ is.na(ans$trapPositionID) ] <- unique(ans$trapPositionID)[1]
ans$Recaps[ is.na(ans$Recaps) ] <- 0 
    
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

