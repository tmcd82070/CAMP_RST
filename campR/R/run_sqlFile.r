#' @title Opens, formats, and runs all SQL queries from a text file.
#'   
#' @param ch Output originating from a call to RODBC function odbcConnectAccess,
#'   which essentially opens a connection between R and Access, or more 
#'   generally, an ODBC database.
#' @param sqlFile A text file containing one or more SQL-formatted query strings.
#' @param ... Additional parameters needed for evaluation of the underlying SQL queries contained in sqlFile.  See 'Details.'
#' 
#' @param sql.code.dir Directory storing all SQL code for the CAMP RST platform.   
#'   
#'   
#'   
#' @return FALSE, assuming successful completion of a SQL query.  Otherwise, an 
#'   error message to either the Console window, if running directly via R, or 
#'   to the run_R.out file, if running via the Platform.
#'   
#' @details Function F.sql_error_check is used immediately after the
#' use of RODBC function sqlQuery, which queries an Access database via a text
#' string formatted as a SQL query. Function sqlQuery, if successful, results in
#' a dataframe, possibly with zero rows. Otherwise, a character vector with
#' error messsages is returned. Function F.sql.error.check examines if an error
#' message is returned, and if found, stops execution and prints an error
#' message to the R_run.out text file or the R Console.
#' 
#' @seealso sqlQuery
#'   
#' @aliases
#' 
#' @examples
#' db.file <- "C:/path/CAMP.mdb"                    # the location of CAMP.mdb
#' db <- get( "db.file", env=.GlobalEnv )           # assign the mdb string to an object
#' ch <- odbcConnectAccess(db)                      # open a connection between R and Access
#' 
#' # a good query
#' ans <- sqlQuery( ch, "SELECT * FROM luRun" )     # select all rows via a SQL query
#' F.sql.error.check(ans)                           # check for query result validity
#' 
#' #' # a bad query
#' ans <- sqlQuery( ch, "SELECT * FROM fakeTable" ) # select all rows via a SQL query
#' F.sql.error.check(ans)                           # check for query result validity
#' 
#' @export

F.run.sqlFile <- function( ch, sqlFile, echo=TRUE, check.drops=FALSE, sql.code.dir, ... ){
#
#   This function opens the file sqlFile, and runs the sql statements contained therein.
#
#   Inputs:
#       ch = an OPEN RODBC channel
#       sqlFile = name of a text file containing one or more SQL statements. Statements must 
#           be separated by a semicolon ";".  Comments, that are "--" at the start of a line are allowed.
#       echo = if TRUE, the SQL statements are echoed to the R screen. 
#       check.drops = if TRUE, this will check for errors following DROP statements.  The reason for this is that when 
#           sending SQL statements to ACCESS, DROP statements on non-existant tables cause an error. 
#           Many times, we don't want this.  We just need to make sure the table is not there 
#           so we can create it later.  Set this =FALSE and these errors will pass through. 
#           I am not sure about the behavior of DROP when querying other SQL data bases. 
#       ... = named parameters to be substituted into the SQL statement.  This allows R to pass 
#           parameters to SQL for incorporation into things like WHERE clauses. See note.
#
#   Value:
#   The result of the *LAST* SQL statment in the file. Therefore, if you need results back (i.e., you 
#   are not just manipulating tables in the data base), you need to split things up so that the 
#   last SQL statement in the file is the query that returns the results you want.
#
#   Note: any named parameters passed in here will get substituted into the sql statement in 
#   their place.  If, for example, the sql has TAXON in it somewhere, and a parameter to this function is TAXON='16890', 
#   the value '16890' gets substituted into the SQL prior to execution.
#
#
#   

# ch <- ch
# sqlFile <- "QryFishingGaps.sql"
# echo=TRUE
# check.drops=FALSE
# R.FISHGAPMIN=fishingGapMinutes 
  
if( missing(sql.code.dir) ){
	sql.code.dir <- get("sql.code.dir", pos=.GlobalEnv)
}	

sqlFile <- file.path(sql.code.dir, sqlFile)
	 
  
#   --- This is an internal function to check for SQL errors
f.sql.error.check <- function( obj ){
#   Note: this aborts everything when an error occurs. 
#   This could be re-programmed to exit nicely with TRUE or FALSE.
#   Note: This detects an error if obj is 2 in length and has ERROR in the second 
#   position. 

if( is.vector(obj) & length(obj) >= 2 ){
    if( length(grep("ERROR", obj[2])) > 0 ){
        #   An error has occurred.
        stop(paste(obj,"\n"))
    }
}

FALSE
}

#   -----

#   Figure out what additional parameters there are, if any
param <- list(...)

#   Read the SQL statement
sql.statements <- readLines( sqlFile, warn=FALSE )   # a vector of strings, one for each line

#   Substitute the parameters, if any.
#   Note: double quotes cannot be used to surround string parameters, as in "R.TAXON".  This messes with R's and SQL's string representations. 
#   In your sql, use single quotes for strings.
if( length(param) > 0 ){
    for( i in 1:length(param)){
        parm <- names(param)[i]
        parm.value <- param[[i]]
        sql.statements <- gsub( parm, parm.value, sql.statements, fixed=TRUE )
    }    
}

if( echo ) cat(paste("==== Running ", deparse(substitute(sqlFile)), "====\n"))


#   Remove the comments.  These must be at the start of a line, and the whole line is dropped. 
comment.lines <- grep( "^--", sql.statements )
if( length(comment.lines) > 0 ){
    sql.statements <- sql.statements[ -comment.lines ]
}

#   Make into one giant string for the computers
sql.statements <- paste( sql.statements, collapse=" ") # a single string, all lines appended

#   Collapse multiple spaces into one space.  this is not necessary, but is saves some space.
sql.statements <- gsub( " +", " ", sql.statements )

#   Now split the long string at the semicolons.  I do this because sqlQuery does not allow more than one sql statement at a time.  
#   This way, I can put multiple sql statements in a file, and then loop over them
sql.statements <- strsplit( sql.statements, ";" )[[1]]

#   Take out leading spaces
sql.statements <- gsub( "^ ", "", sql.statements )

#   Take out any tabs, which mess things up
sql.statements <- gsub( "\t", "", sql.statements )

#   Take out the last element, caused by the end of file
sql.statements <- sql.statements[nchar(sql.statements) > 0]

#   Do the users wishes re drop statements
if( check.drops ){
    drops <- -1
} else {
    #   Don't want to check for errors on drop statements. flag these
    drops <- grep( "^(?i)drop", sql.statements )
}


for( i in 1:length(sql.statements) ){

    #   Print out the sql for us humans, if called for
    if( echo ){
        out.sql <- gsub( ", ", "\n, ", sql.statements[i], fixed=T)
        out.sql <- gsub( "FROM", "\nFROM", out.sql, fixed=T)
        out.sql <- gsub( "WHERE", "\nWHERE", out.sql, fixed=T)
        out.sql <- gsub( "AND", "\nAND", out.sql, fixed=T)
        out.sql <- gsub( "INTO", "\nINTO", out.sql, fixed=T)
        out.sql <- gsub( "ORDER BY", "\nORDER BY", out.sql, fixed=T)
        out.sql <- gsub( "GROUP BY", "\nGROUP BY", out.sql, fixed=T)
        out.sql <- gsub( "JOIN", "JOIN\n", out.sql, fixed=T)
        
        cat(out.sql)
        cat("\n\n-------------------------------------------------------------\n")
    }

    #   run the query
    ans <- sqlQuery( ch, sql.statements[i] )
    
    if( !(i %in% drops) ){
        #   Check for an error
        f.sql.error.check( ans )
    }

}

if( echo ){
    #   If we get here, we are good.
    cat("Success.\n")
}

ans
}
