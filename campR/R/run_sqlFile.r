#' @title Opens, formats, and runs all SQL queries from a text file.
#'   
#' @param ch A conneciton handle originating from a call to RODBC function odbcConnectAccess,
#'   which essentially opens a connection between R and Access, or more 
#'   generally, an ODBC database.
#' @param sqlFile A text file containing one or more SQL-formatted query 
#'   strings.
#' @param ... Additional parameters needed for evaluation of the underlying SQL 
#'   queries contained in sqlFile.  See 'Details.'
#'   
#' @return By itself, function \code{F.run.sqlFile} returns nothing with respect to R. 
#'   It may, however, based on the queries contained in the \code{sqlFile}, delete and 
#'   re-create several different tables in the accompanying CAMP database. 
#'   Function \code{sqlFetch} from package \code{RODBC} is then usually paired with this 
#'   function to bring in the query-derived Access tables as an R data frame.
#'   
#' @details Function \code{F.run.sqlFile} is often paired with the 
#' \code{RODBC} function \code{sqlQuery}, which reads and returns tables housed in an Access database.
#' 
#' @section Modifying Access-developed SQL queries for use by R-package \code{RODBC}:
#' Report generation via the Platform requires communication between the
#' underlying R code and the Access databases housing the data. The 32-bit
#' R-package \code{RODBC} performs the link between these two systems, allowing for
#' SQL-type queries to be sent from R to Access. The resulting queries, once
#' completed and housed in the Access database, can then be read into R.
#' 
#' Examples of original Access SQL queries that have been processed for use in R
#' can be found in the Code section of this repository. All have a prefix of
#' \code{Qry}, a suffix of \code{.sql}, and have text-file icons. Use these as a guide when
#' updating any new batch of SQL code.
#' 
#' In this project, Connie Shannon, a CAMP RST contractor, writes all Access
#' queries for use in the R code. However, her queries are not generally
#' readable by R (and \code{RODBC}) for direct use in R scripts. Modifications must
#' first be made to text files containing her queries. This list attempts to
#' enumerate the necessary changes.
#' 
#' \enumerate{
#'   \item {Comments. Most SQL scripts developed by Connie contain helpful comments. While
#' helpful, \code{RODBC} must be made to recognize that these are in fact comments, and
#' not developed SQL strings. Use a double hyphen (\code{--}) to demonstrate to \code{RODBC} a
#' comment for which processing is unnecessary. For example,
#' 
#' \code{-- This is a comment and won't be read} would communicate to RODBC that this
#' particular line should be skipped, whereas
#' 
#' \code{SELECT * FROM THIS.TABLE.HERE} forces RODBC to act. Note that double hyphens
#' need only be entered on the left. Additionally, the space following the
#' double hyphen is unnecessary. However, it makes comments more readable.}
#' 
#'   \item {Variables. Many SQL queries developed for the Platform require subsetting the
#' data to chinook salmon via SQL \code{WHERE} clauses. However, in most cases, if not
#' all, the R code has been developed to allow for selection of any piscine
#' species. This means that the \code{WHERE} clause is a variable set by the user. It
#' also means that this variable quantity must be passed from R to Access.
#' Within the database, chinook salmon have the character identifier \code{'161980'}.
#' 
#' In Connie's queries, \code{WHERE} statements for chinook salmon are identified via
#' this identifier. However, because the R code has been developed to allow this
#' value to vary, Connie's SQL code must be adapted to take variable values in
#' RODBC. To do this, change all instances of \code{'161980'} to \code{'R.TAXON'}. The
#' appellation \code{R.TAXON} reflects the R-code name for the species variable, and is
#' how a variable quantity is communicated to Access.
#' 
#' So, specifically, in any developed SQL, change all instances of
#' 
#' \code{CatchRaw.taxonID = '161980'} to
#' 
#' \code{CatchRaw.taxonID = 'R.TAXON'}.}
#' 
#'   \item{\code{INTO SQL} Statements. Queries for use in the
#' Platform are developed for multiple use. This means that once a query for one
#' set of criteria is created, the code must be able to recreate a new query,
#' based on separate criteria, without error. Practically, this means that
#' different queries must be able to overwrite resultant tables in Access
#' databases flawlessly. The way that Access and \code{RODBC} does this varies; this
#' necessitates more complex changes to SQL code developed by Connie.
#' 
#' Within Access, the use of the SQL keyword \code{INTO} forces a table that currently
#' exists to be dropped, while also recreating whatever table data are to be
#' place into. For example, suppose within Access a query generates a final
#' table named \code{Table_fish}. Now, however, a new query is generated, also within
#' Access, creating a new \code{Table_fish}. The new \code{Table_fish} overwrites the old
#' \code{Table_fish}; Access does this via SQL code resembling
#' 
#' \code{SELECT * INTO Table_fish FROM Table_helper;}
#' 
#'  where the special keyword \code{INTO}
#' deletes out the old \code{Table_fish} and replaces it with the new version. Syntax
#' of this nature must be modified for use in \code{RODBC} R code -- the \code{INTO} statement
#' does not work in quite this way. While \code{RODBC} will create the named table, it
#' won't first delete out its previous version. This must be done explicitly.
#' 
#' To work around this, use a \code{DROP TABLE} statement. Then, the use of the \code{INTO}
#' statement will work within R. Explicitly, modifying the code block above,
#' write
#' 
#' \code{DROP TABLE Table_fish;}
#' \code{SELECT * INTO Table_fish FROM Table_helper;} as two
#' separate SQL queries, where the semi-colon \code{;} demonstrates the end of a SQL
#' query.
#' 
#' One final to-do is required. Once a SQL query has been modified to include a
#' \code{DROP TABLE} statement, an initial table must then be produced in all Access
#' databases in which the query is to be run. Practically, the first time this
#' particular query is run, SQL cannot drop the table \code{Table_fish} because it will
#' not exist yet, not having been created yet. To work around this, from within
#' Access, copy and paste any one of the existing tables, and rename it to the
#' name of the final table, which in this case, is \code{Table_fish}. Note that some
#' queries create multiple interim tables. These may also need to be modified
#' via \code{DROP TABLE} statements. On completion, the modified SQL will be fooled
#' into having a table to delete, and will work as intended.}
#' 
#'   \item{\code{INSERT INTO} [Not sure] add records to an empty table? not sure i remember
#' this exactly.}
#' 
#'   \item{Dates [Not sure] Maybe these have to have preceding and antecedent \code{#}-signs
#' added to them, along with specific values replaced by variable names.}
#' }
#' 
#' @section Running SQL Queries via R-package \code{RODBC}:
#' 
#' The R code utilized in the Platform utilizes a certain sequence of steps in
#' order to query against an Access database from within R. First, the \code{RODBC}
#' package must be loaded. Additionally, R must be made aware of the location of
#' the Access database of interest. Here, the database specific for the American
#' river is used. Additionally, the Platform version tied to 11/30/2015 is
#' identified for development.
#' (more stuff to copy from github?  blah blah blah)
#' 
#' @seealso sqlFetch
#'   
#' @aliases
#' 
#' @examples
#' # what to do?
#' 
#' @export

F.run.sqlFile <- function( ch, sqlFile, echo=TRUE, check.drops=FALSE, ... ){
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
