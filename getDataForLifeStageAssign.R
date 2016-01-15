############################################
## Jared Studyvin
## 14 Jan 2016
## get the data for the life stage assignment
############################################



getDataForLifeStageAssign <- function(pathData,taxon,site,min.date,max.date){

print('Data from:')
print(tail(strsplit(pathData,'/')[[1]],1))


require(RODBC)

## the string must be 'db.file', or else f'n 'build_Report_Criteria.r' won't work.
db.file <<- paste0(pathData,"/CAMP.mdb")

if(!file.exists(db.file)){
    print('file does not exist:')
    print(db.file)
    stop()
}

# ---- build the other queries connie mentions in her new sql query ----
nvisits <- F.buildReportCriteria( site, min.date, max.date )      # trent f'n to build first report in a query sequence

if( nvisits == 0 ){
  warning("Your criteria returned no trapVisit table records.")
  return()
}

print('Data from:')
print(tail(strsplit(pathData,'/')[[1]],1))


db <- get( "db.file", env=.GlobalEnv )
ch <- odbcConnectAccess(db)

F.run.sqlFile(ch, "QrySamplePeriod.sql", R.TAXON=taxon ) # trent f'n that works as an access-sql handler
## ---- end building ----


print('Data from:')
print(tail(strsplit(pathData,'/')[[1]],1))


## ---- finally, the new query ----
F.run.sqlFile(ch,'QryUnmarkedChinookLifeStages.sql',TRUE,FALSE,R.TAXON=taxon)
catch <- sqlFetch( ch, "TempSumUnmarkedByTrap_Run_Final2" )       # table name here
F.sql.error.check(catch)   # trent function to make sure the table seems ok

close(ch) # disconnect
## ---- end new query -----

print('Data from:')
print(tail(strsplit(pathData,'/')[[1]],1))

## return the data
return(catch)



} ## function
