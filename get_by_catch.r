F.getByCatch <- function( site, min.date, max.date ){
#
#   Retreive all non-Chinook data between two dates at site.
#

#   ---NOTE: build_report_Criteria must be run before here.


#   *******
#   Open ODBC channel
db <- get( "db.file", env=.GlobalEnv )
ch <- odbcConnectAccess(db)


# ========================================================

cat("SQL to retrieve BY-CATCH records between ")
cat(paste(min.date, "and", max.date, "\n"))


#   Execute the final SQL statement
catch <- F.run.sqlFile( ch, "QryByCatch.sql" )


cat(paste(nrow(catch), "records retrieved.\n\n"))

if(nrow(catch) >= 10) {cat("First 10 records...\n"); print(catch[1:10,])} else {cat("All records...\n"); print(catch)}

odbcClose(ch)

catch

}
