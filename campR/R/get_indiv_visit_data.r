#' @export F.get.indiv.visit.data
#' 
#' @title F.get.indiv.visit.data
#' 
#' @description
#' 
#'    Fetch the visit data for a particular site and run from an Access data base. The resulting data
#'    set should have one line for each visit to a trap at the site. .
#' 
#'    input:
#'    db = full path and name of the Access data base to retrieve data from
#'    tables = vector with named components containing names
#'            of the table in db to pull values from
#'    site = site ID of place we want to do estimates for.  Remember, site encompases multiple traps.   Individual screw 
#'            traps are subsites.  Project could be multiple sites.
#'    run = the single run ID of the fish we want.  If run = NA, all records  will be pulled.
#' 
#'        YOU DO NOT ACTUALLY NEED RUN HERE.  IT MAKES NO DIFFERENCE, TAKE THIS OUT OF PARAMETERS TO THIS CALL.
#' 
#'    min.date and max.date = minimum and maximum dates for data to be included. Format = "YYYY-MM-DD"
#' 
#' 
#'    This routine is very similar to get_catch_data, except that it does not average over subsites.
#'    Raw catches are returned.
#' 
#'    Here is the way to interpret visitTime and visitTime2
#'     "visit start" field (named "visitTime")
#'     for visitTypeID = 1 Date/time of arrival at trap
#'     for visitTypeID = 2 Date/time the trap was emptied
#'     for visitTypeID = 3 Date/time the trap was emptied
#'     for visitTypeID = 4 Date/time the trap was emptied
#'     for visitTypeID = 5 Date/time of arrival at trap
#'     for visitTypeID = 6 Date/time of arrival at trap
#' 
#' 
#'     "visit end" field (named "visitTime2")
#'     for visitTypeID = 1 Date/time the trap began fishing
#'     for visitTypeID = 2 Date/time the trap began fishing
#'     for visitTypeID = 3 Date/time the trap began fishing
#'     for visitTypeID = 4 Date/time of the end of the visit
#'     for visitTypeID = 5 Date/time of the end of the visit
#'     for visitTypeID = 6 Date/time of the end of the visit 
#' 
#' 
#' 
#' 
#'    *******
#'    First, put the start and stop dates of the run into a list
#'    Need these to filter visits.
#' 
#' @param  site <describe argument>
#' @param  run <describe argument>
#' @param  min.date <describe argument>
#' @param  max.date  <describe argument>
#' 
#' @details <other comments found in file>
#'    *******
#'    Retreive site info to get common names.  
#'    *******
#'    Visits SQL
#'    Make variable vectors one long string for sql statement
#' 
#'    THIS IS MIKE'S TEMPLATE QUERY FOR TRAP VISITS
#' SELECT Site.siteName AS TrappingSite
#' , SubSite.subSiteName AS TrapPosition
#' , ProjectDescription.projectName AS Project
#' , TrapVisit.projectDescriptionID
#' , TrapVisit.trapVisitID
#' , TrapVisit.dataSheetNumber AS TrapVisitDataSheetNumber
#' , TrapVisit.visitTime AS VisitTime1
#' , TrapVisit.visitTime2 AS VisitTime2
#' , luVisitType.visitType AS VisitType
#' , luFishProcessed.fishProcessed AS FishProcessed
#' , luSampleGear.sampleGear
#' , luNoYes.noYes AS TrapInThalweg
#' , luTrapFunctioning.trapFunctioning AS TrapFunction
#' , TrapVisit.coneDepthAtStart AS ConeDepthAtStart
#' , TrapVisit.coneDepthAtEnd AS ConeDepthAtEnd
#' , luNoYes_1.noYes AS HalfCone
#' , luNoYes_2.noYes AS IncludeCatchInAnalyses
#' , TrapVisit.includeCatchComments AS IncludeCatchComments
#' , luDebrisVolumeCat.debrisVolumeCat AS DebrisVolumeCategory
#' , TrapVisit.debrisVolume AS DebrisVolumeMeasure
#' , TrapVisit.debrisVolumeUnits
#' , TrapVisit.debrisType AS DebrisType
#' , TrapVisit.comments AS Comments
#' , luNoYes_3.noYes AS DataCollected
#' FROM 
#'     (Site INNER JOIN SubSite ON Site.siteID = SubSite.siteID) 
#'     INNER JOIN (ProjectDescription 
#'         INNER JOIN (luVisitType 
#'             RIGHT JOIN (luTrapFunctioning 
#'                 RIGHT JOIN (luSampleGear 
#'                     RIGHT JOIN (luNoYes 
#'                         RIGHT JOIN (luFishProcessed 
#'                             RIGHT JOIN (luDebrisVolumeCat 
#'                                 RIGHT JOIN (luNoYes AS luNoYes_3 
#'                                     RIGHT JOIN (luNoYes AS luNoYes_2 
#'                                         RIGHT JOIN (luNoYes AS luNoYes_1 
#'                                             RIGHT JOIN TrapVisit 
#'     ON luNoYes_1.noYesID = TrapVisit.halfConeID) 
#'     ON luNoYes_2.noYesID = TrapVisit.includeCatchID) 
#'     ON luNoYes_3.noYesID = TrapVisit.dataCollectedID) 
#'     ON luDebrisVolumeCat.debrisVolumeCatID = TrapVisit.debrisVolumeCatID) 
#'     ON luFishProcessed.fishProcessedID = TrapVisit.fishProcessedID) 
#'     ON luNoYes.noYesID = TrapVisit.inThalwegID) 
#'     ON luSampleGear.sampleGearID = TrapVisit.sampleGearID) 
#'     ON luTrapFunctioning.trapFunctioningID = TrapVisit.trapFunctioningID) 
#'     ON luVisitType.visitTypeID = TrapVisit.visitTypeID) 
#'     ON ProjectDescription.projectDescriptionID = TrapVisit.projectDescriptionID) 
#'     ON SubSite.subSiteID = TrapVisit.trapPositionID
#' ORDER BY Site.siteName, SubSite.subSiteName, TrapVisit.visitTime, TrapVisit.visitTime2;
#' 
#'    Note: query pulls visits from 7 days prior to start of run in order to compute sampleStart and sampleEnd. Must delete these records later.
#'    No visits found, exit nicely.
#'    This query pulls the trap re-starts
#'    Append restarts to end of visits.  Times will be sorted in assign_sample_period.r
#'    Make sure time zones are Pacific. In case analysis is run in another time zone.
#'    Assign sample period start and stop
#'    Get rid of missing sampleEnd's.  sampleEnd's are missing for lines when trap restarted.  
#'    Get rid of missing sampleStarts's.  This is an error.  Happens when trap stopped (visitType=4), but then next record trap is running (visitType=2 or 3) without an intervening trap start (visitType=1)
#' warning("Trap stopped, but no trap start record.  See LOG.")
#'    Drop them, this will create a gap, that will later be imputed.
#'    Assign batch date if missing
#'    Delete the "extra" records outside the run period
#'    Assign gap lengths
#'    Find subsiteID names
#'    Fetch subsite names
#'    Store values as attribute for convienance.
#' 
#' @return <describe return value>
#' 
#' @author WEST Inc.
#' 
#' @seealso \code{\link{<related routine>}}, \code{\link{<related routine>}}
#' 
#' @examples
#' \dontrun{
#' <insert examples>
#' 
#' }
F.get.indiv.visit.data <- function( site, run, min.date, max.date ){
#
#   Fetch the visit data for a particular site and run from an Access data base. The resulting data
#   set should have one line for each visit to a trap at the site. .
#
#   input:
#   db = full path and name of the Access data base to retrieve data from
#   tables = vector with named components containing names
#           of the table in db to pull values from
#   site = site ID of place we want to do estimates for.  Remember, site encompases multiple traps.   Individual screw 
#           traps are subsites.  Project could be multiple sites.
#   run = the single run ID of the fish we want.  If run = NA, all records  will be pulled.
#
#       YOU DO NOT ACTUALLY NEED RUN HERE.  IT MAKES NO DIFFERENCE, TAKE THIS OUT OF PARAMETERS TO THIS CALL.
#
#   min.date and max.date = minimum and maximum dates for data to be included. Format = "YYYY-MM-DD"
#
#
#   This routine is very similar to get_catch_data, except that it does not average over subsites.
#   Raw catches are returned.
#
#   Here is the way to interpret visitTime and visitTime2
#    "visit start" field (named "visitTime")
#    for visitTypeID = 1 Date/time of arrival at trap
#    for visitTypeID = 2 Date/time the trap was emptied
#    for visitTypeID = 3 Date/time the trap was emptied
#    for visitTypeID = 4 Date/time the trap was emptied
#    for visitTypeID = 5 Date/time of arrival at trap
#    for visitTypeID = 6 Date/time of arrival at trap
#
#
#    "visit end" field (named "visitTime2")
#    for visitTypeID = 1 Date/time the trap began fishing
#    for visitTypeID = 2 Date/time the trap began fishing
#    for visitTypeID = 3 Date/time the trap began fishing
#    for visitTypeID = 4 Date/time of the end of the visit
#    for visitTypeID = 5 Date/time of the end of the visit
#    for visitTypeID = 6 Date/time of the end of the visit 




#   *******
#   First, put the start and stop dates of the run into a list
#   Need these to filter visits.
strt.dt <- as.POSIXct( min.date, format="%Y-%m-%d" )
end.dt <- as.POSIXct( max.date, format="%Y-%m-%d" )
run.season <- data.frame( start=strt.dt, end=end.dt )

#   *******
#   Retrieve db file name and table names
tables <- get( "table.names", envir=.GlobalEnv )
db <- get( "db.file", envir=.GlobalEnv ) 


#   *******
#   Open ODBC channel
ch <- odbcConnectAccess(db)


#   *******
#   Retreive site info to get common names.  
site.info <- sqlQuery( ch, paste("SELECT siteName, siteAbbreviation, streamName FROM", tables["sites"], 
        "WHERE (siteID =", site, ")" ))
        
F.sql.error.check(site.info)

site.name <- as.character(site.info$siteName)
site.stream <- as.character(site.info$streamName)
site.abbr <- as.character(site.info$siteAbbreviation)

cat( paste(site.abbr, site.name, site.stream, sep=":") )
cat("\n")

cat(paste("Site = ", site, "\n"))




if( !is.na(run) ){
    runs <- sqlQuery(ch, paste( "SELECT run, runID FROM", tables["run.codes"] ))
    F.sql.error.check(runs)
    run.name <- as.character(runs$run[ runs$runID == run ])
} else {
    run.name = "NA"
}
cat(paste("Run = ", run.name, "\n"))
cat(paste("Start date = ", run.season$start, "\n"))
cat(paste("End date = ", run.season$end, "\n"))


#   *******
#   Visits SQL

#   Make variable vectors one long string for sql statement
visit.vars <- c("visitTime",
                "visitTime2",
                "trapVisitID",
                "visitTypeID",
                "fishProcessedID",
                "trapPositionID",
                "batchDate",
                "trapFunctioningID", 
                "includeCatchID",
                "dataCollectedID")
visit.vars.sql <- F.vec2sqlstr(tables["trap.visit"], visit.vars)
visit.vars.sql <- paste( "Site.siteID,", visit.vars.sql )
    

s.tab <- tables["sites"]
ss.tab <- tables["subsites"]
tv.tab <- tables["trap.visit"]

######
#   THIS IS MIKE'S TEMPLATE QUERY FOR TRAP VISITS
#SELECT Site.siteName AS TrappingSite
#, SubSite.subSiteName AS TrapPosition
#, ProjectDescription.projectName AS Project
#, TrapVisit.projectDescriptionID
#, TrapVisit.trapVisitID
#, TrapVisit.dataSheetNumber AS TrapVisitDataSheetNumber
#, TrapVisit.visitTime AS VisitTime1
#, TrapVisit.visitTime2 AS VisitTime2
#, luVisitType.visitType AS VisitType
#, luFishProcessed.fishProcessed AS FishProcessed
#, luSampleGear.sampleGear
#, luNoYes.noYes AS TrapInThalweg
#, luTrapFunctioning.trapFunctioning AS TrapFunction
#, TrapVisit.coneDepthAtStart AS ConeDepthAtStart
#, TrapVisit.coneDepthAtEnd AS ConeDepthAtEnd
#, luNoYes_1.noYes AS HalfCone
#, luNoYes_2.noYes AS IncludeCatchInAnalyses
#, TrapVisit.includeCatchComments AS IncludeCatchComments
#, luDebrisVolumeCat.debrisVolumeCat AS DebrisVolumeCategory
#, TrapVisit.debrisVolume AS DebrisVolumeMeasure
#, TrapVisit.debrisVolumeUnits
#, TrapVisit.debrisType AS DebrisType
#, TrapVisit.comments AS Comments
#, luNoYes_3.noYes AS DataCollected
#FROM 
#    (Site INNER JOIN SubSite ON Site.siteID = SubSite.siteID) 
#    INNER JOIN (ProjectDescription 
#        INNER JOIN (luVisitType 
#            RIGHT JOIN (luTrapFunctioning 
#                RIGHT JOIN (luSampleGear 
#                    RIGHT JOIN (luNoYes 
#                        RIGHT JOIN (luFishProcessed 
#                            RIGHT JOIN (luDebrisVolumeCat 
#                                RIGHT JOIN (luNoYes AS luNoYes_3 
#                                    RIGHT JOIN (luNoYes AS luNoYes_2 
#                                        RIGHT JOIN (luNoYes AS luNoYes_1 
#                                            RIGHT JOIN TrapVisit 
#    ON luNoYes_1.noYesID = TrapVisit.halfConeID) 
#    ON luNoYes_2.noYesID = TrapVisit.includeCatchID) 
#    ON luNoYes_3.noYesID = TrapVisit.dataCollectedID) 
#    ON luDebrisVolumeCat.debrisVolumeCatID = TrapVisit.debrisVolumeCatID) 
#    ON luFishProcessed.fishProcessedID = TrapVisit.fishProcessedID) 
#    ON luNoYes.noYesID = TrapVisit.inThalwegID) 
#    ON luSampleGear.sampleGearID = TrapVisit.sampleGearID) 
#    ON luTrapFunctioning.trapFunctioningID = TrapVisit.trapFunctioningID) 
#    ON luVisitType.visitTypeID = TrapVisit.visitTypeID) 
#    ON ProjectDescription.projectDescriptionID = TrapVisit.projectDescriptionID) 
#    ON SubSite.subSiteID = TrapVisit.trapPositionID
#ORDER BY Site.siteName, SubSite.subSiteName, TrapVisit.visitTime, TrapVisit.visitTime2;
######


#   Note: query pulls visits from 7 days prior to start of run in order to compute sampleStart and sampleEnd. Must delete these records later.
sql.visit <- paste( "SELECT ", visit.vars.sql, 
    " FROM (", s.tab, " INNER JOIN ", ss.tab, " ON ", s.tab, ".siteID", " = ", ss.tab, ".siteID", ")", 
    " INNER JOIN ", tv.tab, " ON ", ss.tab, ".subsiteID", "=", tv.tab, ".trapPositionID", 
    " WHERE (((", s.tab, ".siteID)=", site, ") ",
    "AND ((", tv.tab, ".visitTime)>=#", format(run.season$start-(7*24*60*60), "%m/%d/%Y"), "#) ",
    "AND ((", tv.tab, ".visitTime2)<=#", format(run.season$end, "%m/%d/%Y"), "#) ",
    "AND ((", tv.tab, ".visitTypeID)=2 Or (", tv.tab, ".visitTypeID)=4) ",
    "AND ((", tv.tab, ".includeCatchID)=1 Or (", tv.tab, ".includeCatchID)>250)); ",
    sep="")
    
cat("\nVISITS where fish were processed SQL statement: \n")
out.sql <- gsub( ",", "\n,", sql.visit, fixed=T)
out.sql <- gsub( "FROM", "\nFROM", out.sql, fixed=T)
out.sql <- gsub( ") ", ")\n", out.sql, fixed=T)
cat(out.sql)
cat("\n")

visit <- sqlQuery(ch, sql.visit)
F.sql.error.check(visit)
cat(paste("\nNumber of fish processing visits found:", nrow(visit), "\n"))

if( nrow(visit) == 0 ){
    #   No visits found, exit nicely.
    odbcClose(ch)

    attr(visit, "siteDescriptionID" ) <- site
    attr(visit, "site.name") <- site.name
    attr(visit, "site.abbr") <- site.abbr
    attr(visit, "runID") <- run
    attr(visit, "run.name") <- run.name
    attr(visit, "run.season") <- run.season
    
    cat("Check dates...\n")
    return(visit)   
}


#   This query pulls the trap re-starts
sql.visit <- paste( "SELECT ", visit.vars.sql, 
    " FROM (", s.tab, " INNER JOIN ", ss.tab, " ON ", s.tab, ".siteID", " = ", ss.tab, ".siteID", ")", 
    " INNER JOIN ", tv.tab, " ON ", ss.tab, ".subsiteID", "=", tv.tab, ".trapPositionID", 
    " WHERE (((", s.tab, ".siteID)=", site, ") ",
    "AND ((", tv.tab, ".visitTime)>=#", format(run.season$start-(7*24*60*60), "%m/%d/%Y"), "#) ",
    "AND ((", tv.tab, ".visitTime2)<=#", format(run.season$end, "%m/%d/%Y"), "#) ",
    "AND ((", tv.tab, ".visitTypeID)=1 Or (", tv.tab, ".visitTypeID)=3));",
    sep="")

cat("\nVISITS where trap was stopped or started SQL statement: \n")
out.sql <- gsub( ",", "\n,", sql.visit, fixed=T)
out.sql <- gsub( "FROM", "\nFROM", out.sql, fixed=T)
out.sql <- gsub( ") ", ")\n", out.sql, fixed=T)
cat(out.sql)
cat("\n")

restarts <- sqlQuery(ch, sql.visit)
F.sql.error.check(restarts)
cat(paste("\nNumber of start-stop visits found:", nrow(restarts), "\n"))


#   Append restarts to end of visits.  Times will be sorted in assign_sample_period.r
visit <- rbind(visit, restarts) 

#cat(paste("\nNumber of visits found:", nrow(visit), "\n"))


#   Make sure time zones are Pacific. In case analysis is run in another time zone.
visit$visitTime <- as.POSIXct( format(visit$visitTime, format="%Y-%m-%d %H:%M:%S"), format="%Y-%m-%d %H:%M:%S", tz="America/Los_Angeles") 
visit$visitTime2 <- as.POSIXct( format(visit$visitTime2, format="%Y-%m-%d %H:%M:%S"), format="%Y-%m-%d %H:%M:%S", tz="America/Los_Angeles")  
visit$batchDate <- as.POSIXct( format(visit$batchDate, format="%Y-%m-%d %H:%M:%S"), format="%Y-%m-%d %H:%M:%S", tz="America/Los_Angeles")  



#   Assign sample period start and stop
visit <- F.assign.sample.period(visit)


#   Get rid of missing sampleEnd's.  sampleEnd's are missing for lines when trap restarted.  
visit <- visit[!is.na(visit$sampleEnd),]

#   Get rid of missing sampleStarts's.  This is an error.  Happens when trap stopped (visitType=4), but then next record trap is running (visitType=2 or 3) without an intervening trap start (visitType=1)
if( sum(is.na(visit$sampleStart)) > 0 ){
    #warning("Trap stopped, but no trap start record.  See LOG.")
    cat("\n\n\nObservations with no trap start records (these were dropped):\n")
    print(visit[ is.na(visit$sampleStart), ] )
    cat("\n\n")
    #   Drop them, this will create a gap, that will later be imputed.
    visit <- visit[!is.na(visit$sampleStart),]
}


#   Assign batch date if missing
visit <- F.assign.batch.date(visit)


#   Delete the "extra" records outside the run period
ind <- (run.season$start <= visit$sampleEnd) & (visit$sampleEnd <= run.season$end)
visit <- visit[ind,]


#   Assign gap lengths
visit <- F.assign.gaplen( visit )

cat(paste("\nNumber of trap visits:", nrow(visit), "\n"))
cat(paste("First 10 visit records:\n"))
print(visit[1:10,])

#   Find subsiteID names
#   Fetch subsite names
subsite.names <- sqlQuery( ch, paste("SELECT subSiteID, subSiteName FROM", tables["subsites"], 
        "WHERE (siteID =", site, ")" ))
F.sql.error.check(subsite.names)
subsite.names$subSiteName <- as.character(subsite.names$subSiteName)        

subSites.found <- data.frame(subSiteID=sort(unique(visit$trapPositionID)))
subSites.found <- merge( subSites.found, subsite.names, by="subSiteID", all.x=T )


#   Store values as attribute for convienance.
attr(visit, "siteDescriptionID" ) <- site
attr(visit, "site.name") <- site.name
attr(visit, "site.abbr") <- site.abbr
attr(visit, "subsites" ) <- subSites.found
attr(visit, "runID") <- run
attr(visit, "run.name") <- run.name
attr(visit, "run.season") <- run.season

odbcClose(ch)

visit

}

