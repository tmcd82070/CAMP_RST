#####################################
## Jared Studyvin
## 11 Jan 2016
## PSMFS 469-07.005 phase 4
## PM: Trent
## Query the data base to get the data for life stage assignment
#####################################


library(RODBC)


taxon <- '161980'           # always chinook salmon
site <- '42000'             # this is the river -- this is in table 'site' in the mdbs.
min.date <- '1990-01-01'    # the pre queries need to restrict to a time frame --
max.date <- '2015-12-31'    #  some data goes back to the 90s

pathData <- "//lar-file-srv/Data/PSMFC_CampRST/ThePlatform/CAMP_RST20151130/Data/TestingDBs/CAMP_RBDD_29Jan2015"     # i point to an mdb
codePath <- "C:/Users/jmitchell/Desktop/getQueryToWork"                                                              # i point to a folder containing necessary goods
setwd(codePath)                                                                                                      # point somewhere / somehow to where the queries are

source( paste0(codePath,"/run_sqlFile.r" ))                                                                          # a trent f'n from the platform
source( paste0(codePath,"/sql_error_check.r" ))                                                                      # a trent f'n from the platform
source( paste0(codePath,"/build_Report_Criteria.r" ))                                                                # a trent f'n from the platform

# dbPathAll <- gsub('//','/',paste0(list.dirs(pathData,TRUE,FALSE),'/CAMP.mdb'))
# dbPath <- dbPathAll[1]  

db.file <<- paste0(pathData,"/CAMP.mdb")                                                                             # the string must be 'db.file', or else f'n 'build_Report_Criteria.r' won't work.


# ---- build the other queries connie mentions in her new sql query ----
nvisits <- F.buildReportCriteria( site, min.date, max.date )      # trent f'n to build first report in a query sequence

if( nvisits == 0 ){
  warning("Your criteria returned no trapVisit table records.")
  return()
}

db <- get( "db.file", env=.GlobalEnv ) 
ch <- odbcConnectAccess(db)

F.run.sqlFile( ch, "QrySamplePeriod.sql", R.TAXON=taxon )         # trent f'n that works as an access-sql handler
# ---- end building ----

# ---- finally, the new query ----
F.run.sqlFile(ch,'QryUnmarkedChinookLifeStages.sql',TRUE,FALSE,R.TAXON=taxon)
catch <- sqlFetch( ch, "TempSumUnmarkedByTrap_Run_Final2" )       # table name here
F.sql.error.check(catch)                                          # trent function to make sure the table seems ok

close(ch)                                                         # disconnect
# ---- end new query -----
