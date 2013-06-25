F.find.recaps <- function( db, tables ){
#
#   Find the releases that are associated with recaptures.  I.e., for every record in 'CatchRaw' that
#   for a fish that has a mark, compute recapID (Y or N) and a releaseID for where
#   the fish came from.
#
#   input:
#   db = full path and name of the Access data base to retrieve data from
#   tables = vector of names of tables in db to pull values from.
#       These values should be as follows:
#       tables$catch = the name of the CatchRaw table
#       tables$mark.found = name of the MarkExisting table
#       tables$trap.visit = name of TrapVisit table
#       tables$release = name of Release table
#       tables$mark.applied = name of MarkApplied table
#
#   Output:
#   The values of recapID and releaseID are written to the tables$catch table in the
#   data base


library(RODBC)

ch <- odbcConnectAccess(db)

#   Retreive the YES/NO codes from the luNoYes table.  Needed for sqlQuery below.
luNoYes <- sqlFetch(ch, tables["yes.no.codes"])
no <- luNoYes$noYesID[ luNoYes$noYes == "No" ]
yes <- luNoYes$noYesID[ luNoYes$noYes == "Yes" ]


if( interactive() ) cat("ASSIGNING RECAP ID'S:\n")


#   ---- Fetch all the tables.  As .mdb get huge, will have to do something different here. (e.g., subset to year/run)
if( interactive() ) cat("\tRetrieving RELEASE information...")
release <- sqlQuery(ch, paste( "SELECT releaseID, releaseDate, releaseTime, testDays, releaseSiteID, markedTaxonID",
        "FROM", tables["release"]))   # Fetches the release information
if( interactive() ) cat(paste(nrow(release), "records found.\n"))        
        

if( interactive() ) cat("\tRetrieving MARKS APPLIED information...")
mark.applied <- sqlQuery(ch, paste( "SELECT releaseID, appliedMarkTypeID, appliedMarkColorID, appliedMarkPositionID",
        "FROM", tables["mark.applied"]))   # Fetches the mark information
if( interactive() ) cat(paste(nrow(mark.applied), "records found.\n"))        




if( interactive() ) cat("\tRetrieving RELEASE BY TARGET SITE information...")
release.x.targetsite <- sqlQuery(ch, paste( "SELECT releaseID, targetSiteID",
        "FROM", tables["rel.x.target"]))   # Fetches the info on which release was targeted for which RST
if( interactive() ) cat(paste(nrow(release.x.targetsite), "records found.\n"))        



if( interactive() ) cat("\tRetrieving MARKS FOUND information...")
mark.found <- sqlQuery(ch, paste( "SELECT catchRawID, markTypeID, markColorID, markPositionID",
        "FROM", tables["mark.found"] ))  # Fetches catches with marks on them
if( interactive() ) cat(paste(nrow(mark.found), "records found.\n"))        



if( interactive() ) cat("\tRetrieving RECAPTURED FISH information...")
catch <- sqlQuery(ch, paste( "SELECT trapVisitID, catchRawID, atCaptureRecapID, taxonID",
        "FROM", tables["catch"]))  # Fetches raw catch data that were recaptures
if( interactive() ) cat(paste(nrow(catch), "records found.\n"))        



if( interactive() ) cat("\tRetrieving TRAP VISIT information...")
trap.visit <- sqlQuery(ch, paste( "SELECT siteID, trapVisitID, dateSampleEnded, timeSampleEnded",
        "FROM", tables["trap.visit"]))
if( interactive() ) cat(paste(nrow(trap.visit), "records found.\n"))        


#   ---- Date and time stored in two separate columns.  Merge into one datetime field.
if( interactive() ) cat(paste("\tComputing dates....\n"))        
release <- F.fix.datetime( release, "releaseDate", "releaseTime" )
trap.visit <- F.fix.datetime( trap.visit, "dateSampleEnded", "timeSampleEnded" )


#   ---- Compute interval of time during which the releases were valid
release$releaseTrialEnd <- release$releaseDate + release$testDays * 24 * 60 * 60


#   ---- Merge release, mark.applied, and target site tables
if( interactive() ) cat(paste("\tMerging releases, marks applied, and target site information....\n"))        
release <- merge( release, release.x.targetsite, by="releaseID", all.x=TRUE )
release <- merge( release, mark.applied, by="releaseID", all.x=TRUE )



#   ---- Merge the recaptures with visit data to get date
if( interactive() ) cat(paste("\tMerging trap visits, catches, and marks found information....\n"))        
#catch <- catch[ !is.na(catch$markTypeID), ]  #  Only want fish with a mark on them
catch <- merge( catch, trap.visit, by="trapVisitID", all.x=T )
catch <- merge( catch, mark.found, by="catchRawID", all.Y=T )




#   ---- Find recaptures from every release
if( interactive() ) cat(paste("\tComputing recapture ID's....\n"))        

recapID <- rep( no, nrow(catch))
releaseID.in.capture.table <- rep( NA, nrow(catch))


for( i in 1:nrow(release)){


    # The following *.ok variables define the criterion by which we determine a recapture
    date.ok <- (release$releaseDate[i] <= catch$dateSampleEnded) & (catch$dateSampleEnded <= release$releaseTrialEnd[i])
    target.site.ok <- release$targetSiteID[i] == catch$siteID
    mark.type.ok <- release$appliedMarkTypeID[i] == catch$markTypeID
    mark.color.ok <- release$appliedMarkColorID[i] == catch$markColorID
    mark.position.ok <- release$appliedMarkPositionID[i] == catch$markPositionID
    taxon.ok <- release$markedTaxonID[i] == catch$taxonID


    recaps <- date.ok & target.site.ok & mark.type.ok & mark.color.ok & mark.position.ok & taxon.ok

    #   This stores the fact that this catch was recap, AND the release ID for it.
    if( any(recaps) ){
        recapID[ recaps ] <- yes  # 'Yes', note this value must match 'Yes' in luNoYes table
        releaseID.in.capture.table[ recaps ] <- release$releaseID[i]
    }

}


catch$atCaptureRecapID <- recapID
catch$releaseID <- releaseID.in.capture.table


#   ---- Use an update query to write recapID and releaseID to catch table.
#        First, get rid of rows and columns we do not need to update.
#        There should be no missing values in this reduced data frame.
if( interactive() ) cat(paste("\tUpdating Access file...."))        
catch <- catch[ catch$atCaptureRecapID == yes, c("catchRawID", "atCaptureRecapID", "releaseID")]
sqlUpdate( ch, catch, tablename=tables["catch"], index="catchRawID" )
cat(paste( nrow(catch), "'recapID' records in table", tables["catch"], "updated.\n"))



odbcClose(ch)

invisible(1)

}
