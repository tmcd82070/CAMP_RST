############################################
## Jared Studyvin
## 14 Jan 2016
## get the data for the life stage assignment
############################################



getDataForLifeStageAssign <- function(pathData,taxon='161980',site='42000',min.date='1990-01-01',max.date='2015-12-31'){

require(RODBC)

## the string must be 'db.file', or else f'n 'build_Report_Criteria.r' won't work.
db.file <<- paste0(pathData,"CAMP.mdb")
file.exists(db.file)

# ---- build the other queries connie mentions in her new sql query ----
nvisits <- F.buildReportCriteria( site, min.date, max.date )      # trent f'n to build first report in a query sequence

if( nvisits == 0 ){
  warning("Your criteria returned no trapVisit table records.")
  return()
}

db <- get( "db.file", env=.GlobalEnv )
ch <- odbcConnectAccess(db)

F.run.sqlFile(ch, "QrySamplePeriod.sql", R.TAXON=taxon ) # trent f'n that works as an access-sql handler
## ---- end building ----

## ---- finally, the new query ----
F.run.sqlFile(ch,'QryUnmarkedChinookLifeStages.sql',TRUE,FALSE,R.TAXON=taxon)
catch <- sqlFetch( ch, "TempSumUnmarkedByTrap_Run_Final2" )       # table name here
F.sql.error.check(catch)   # trent function to make sure the table seems ok

close(ch) # disconnect
## ---- end new query -----

## return the data
return(catch)

} ## function
