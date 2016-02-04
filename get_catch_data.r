F.get.catch.data <- function( site, taxon, min.date, max.date,autoLS=FALSE ){
##
##   Fetch the catch data for a SINGLE TAXON from an Access data base. Do some initial
##   computations, like dates.
##
##   input:
##   db = full path and name of the Access data base to retrieve data from
##   tables = vector with named components containing names
##           of the table in db to pull values from
##   site = site ID of place we want to do estimates for.
##   taxon = the taxon number(s) (from luTaxon) to retrieve.  If a scalar, only
##       one taxon is retrieved.  If vector of taxon id's, the sum of all
##       taxons is retrieved.
##   run = the single run ID of the fish we want.  If run = NA, all records for the fish
##       will be pulled.
##   min.date = minimum date for data to include. This is a text string in the format %Y-%m-%d, or YYYY-MM-DD
##   max.date = maximum date for data to include.  Same format as min.date
##   autoLS = FALSE, nothing new is done. autoLS =TRUE the life stage is assigned using a mixture distribution/clustering analysis
##
##
##
##   To be included in the catch data, a record has to be from the site,
##   of the correct taxon, of the correct run, and between min and max dates.
##

##f.banner <- function( x ){
##    cat("\n")
##    cat(paste(rep("=",50), collapse=""));
##    cat(x);
##    cat(paste(rep("=",50), collapse=""));
##    cat("\n")
##}

    ##   *****

    if(autoLS){
        ## this is the catch data with weight
        catch <- getCatchDataWeight(taxon,site,min.date,max.date)
        ## need to open a channel for later
         db <- get( "db.file", env=.GlobalEnv )
        ch <- odbcConnectAccess(db)
    }else{
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

                                        # stop('in the name of safety.')      # jason stop to show connie what can be output to the console window.  delete later probably.
                                        #   *****
                                        #   Now, fetch the result
        catch <- sqlFetch( ch, "TempSumUnmarkedByTrap_Run_Final" )

    } #end else


    includecatchID <- sqlFetch(ch, "TempSamplingSummary")             # jason add to get variable includeCatchID
    F.sql.error.check(catch)

    close(ch)



#   ******************************************************************
#   Assign time zone (probably does not matter)
time.zone <- get( "time.zone", env=.GlobalEnv )
attr(catch$StartTime, "tzone") <- time.zone
attr(catch$EndTime, "tzone") <- time.zone

#  jason add all this get includeCatchID:  Assign time zone (definitely does matter -- otherwise it goes to MST)
time.zone <- get( "time.zone", env=.GlobalEnv )
# includecatchID$StartTime <- includecatchID$timeSampleStarted
includecatchID$EndTime <- includecatchID$timeSampleEnded
includecatchID$ProjID <- includecatchID$projectDescriptionID
includecatchID$timeSampleStarted <- includecatchID$timeSampleEnded <- includecatchID$projectDescriptionID <- includecatchID$trapVisitID <- includecatchID$sampleGearID <- NULL
# attr(includecatchID$StartTime, "tzone") <- time.zone
attr(includecatchID$EndTime, "tzone") <- time.zone
includecatchID <- includecatchID[,c('trapPositionID','EndTime','ProjID','includeCatchID')]

# sampleGearID ProjID trapPositionID trapVisitID
catch <- merge(catch,includecatchID,by=c('trapPositionID','EndTime','ProjID'),all.x=TRUE)


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

######################################
## Jared Addition
## this is where the life stage assignment will happen from a function call
if(autoLS){
    cat('Starting mixture distribution estimation to assign life stage. \n')
    ##write.csv(catch,paste0(output.file,site,'.csv'),row.names=FALSE)
    cat('\n')
    cat('\n')
    cat('\n')
    cat('\n')
    cat('\n')
    cat('\n')
    return(catch)
    stop('That is good enough')
}

######################################



# get summary counts of catch run vs. lifetstage for internal checking.
totalFish <<- sum(catch$Unmarked)
totalRun <<- aggregate(catch$Unmarked, list(FinalRun=catch$FinalRun), FUN=sum)
totalLifeStage <<- aggregate(catch$Unmarked, list(LifeStage=catch$lifeStage), FUN=sum)
totalRunXLifeStage <<- aggregate(catch$Unmarked, list(LifeStage=catch$lifeStage,FinalRun=catch$FinalRun), FUN=sum)

catch$Unassd <- catch$lifeStage # jason add to ID the unassigned lifeStage -- necessary to separate measured vs caught.
#   ********************************************************************


# expand half-cone operations to full-cone.  this needs to happen prior to plus-counting.
# catch$preUnmarked <- catch$Unmarked
# catch$Unmarked <- ifelse(catch$halfConeID == 1,2*catch$preUnmarked,catch$preUnmarked)
#
# tapply(catch$Unmarked, catch$halfConeID, sum)
#
# catch.temp <- catch[catch$FinalRun == 'Fall',]
# hcY <- data.frame(hcY=tapply(catch.temp[catch.temp$halfConeID == 1,]$Unmarked, catch.temp[catch.temp$halfConeID == 1,]$SampleDate, sum))
# hcY$matchDate <- rownames(hcY)
# hcN <- data.frame(hcN=tapply(catch.temp[catch.temp$halfConeID == 2,]$Unmarked, catch.temp[catch.temp$halfConeID == 2,]$SampleDate, sum))
# hcN$matchDate <- rownames(hcN)
# hc <- merge(hcY,hcN,by=c('matchDate'),all.x=TRUE,all.y=TRUE)
#
# sum(hc$hcY[!is.na(hc$hcY)],hc$hcN[!is.na(hc$hcN)]) == sum(catch.temp$Unmarked)   # check if we have all the fish we should
#
#
# plot(as.Date(hc$matchDate[!is.na(hc$hcY)]),hc$hcY[!is.na(hc$hcY)],col='red'  ,pch=19,xlim=c(min(as.Date(hc$matchDate)),max(as.Date(hc$matchDate))),ylim=c(0,max(hc$hcY,hc$hcN,na.rm=TRUE)))
# par(new=TRUE)
# plot(as.Date(hc$matchDate[!is.na(hc$hcN)]),hc$hcN[!is.na(hc$hcN)],col='black',pch=19,xlim=c(min(as.Date(hc$matchDate)),max(as.Date(hc$matchDate))),ylim=c(0,max(hc$hcY,hc$hcN,na.rm=TRUE)))
# par(new=TRUE)
# segments(min(as.Date(hc$matchDate)),mean(hc$hcY[!is.na(hc$hcY)]),max(as.Date(hc$matchDate)),mean(hc$hcY[!is.na(hc$hcY)]),col='red')
# par(new=TRUE)
# segments(min(as.Date(hc$matchDate)),mean(hc$hcN[!is.na(hc$hcN)]),max(as.Date(hc$matchDate)),mean(hc$hcN[!is.na(hc$hcN)]),col='black')




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
