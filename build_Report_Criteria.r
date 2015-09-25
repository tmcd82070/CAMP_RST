F.buildReportCriteria <- function( site, min.date, max.date ){
#
#   This updates a table in the Access file to contain the trapVisitID's we want. 
#   i.e., the trapVisitID's for a particular site between min.date and max.date
#

strt.dt <- as.POSIXct( min.date, format="%Y-%m-%d" )
end.dt <- as.POSIXct( max.date, format="%Y-%m-%d" )


db <- get( "db.file", env=.GlobalEnv ) 
ch <- odbcConnectAccess(db)

F.run.sqlFile( ch, "QryBuildReportCriteria.sql", SITE=site, STRT.DT=format(strt.dt, "%m/%d/%Y"), END.DT=format(end.dt, "%m/%d/%Y") )


ans <- sqlQuery( ch, "SELECT COUNT(1) FROM TempReportCriteria_Trapvisit" )
F.sql.error.check(ans)

cat(paste(ans, "trap visits found between", strt.dt, "and", end.dt, "\n\n"))

close(ch)

ans[1,1]

}


