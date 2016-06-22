#' @export
#' 
#' @title F.run.sqlFile
#'   
#' @description Opens, formats, and runs all SQL statements from a text file.
#'   
#' @param ch A connection handle originating from a call to RODBC function
#'   \code{odbcConnectAccess}, which essentially opens a connection between R and
#'   Access, or more generally, an ODBC database.
#'   
#' @param sqlFile Name of a text file containing one or more SQL statements. 
#'   This file must exist in the directory given by \code{sql.code.dir}, which 
#'   is a global variable (see \code{\link{GlobalVars}}).  Variable
#'   \code{sql.code.dir} can be reset as needed with a call to
#'   \code{GlobalVars} or by reassignment in regular code. Statements in
#'   \code{sqlFile} must be separated by a semicolon ";". Comments, or lines
#'   starting with "\code{--}", are allowed.
#'   
#' @param echo If TRUE, submitted SQL statements echo to the log file. Defaults
#'   to TRUE.
#'   
#' @param check.drops If TRUE, the routine checks for errors following SQL
#'   \code{DROP} statements.  This is done since \code{DROP} statements on 
#'   non-existent tables causes an abort error. The \code{DROP} simply needs
#'   ensures the non-existence of the table; this way, it can be created later.
#'   Set this to \code{FALSE} and these \code{DROP} errors will pass through. 
#'   This behavior of \code{DROP} may be specific to Access, and thus may not
#'   occur when querying other SQL data bases (e.g., mySQL or MSSQL).
#'   
#' @param ... Additional named parameters needed for evaluation of the
#'   underlying SQL queries contained in \code{sqlFile}. This allows R to pass
#'   parameters to SQL for incorporation into things like WHERE clauses.  See
#'   Details.
#'   
#' @return Results of the \bold{LAST} SQL statment in \code{sqlFile}. In many
#'   cases, the SQL statments in \code{sqlFile} delete, create, and populate 
#'   tables in the Access database housing data.  Results housed in this 
#'   database are then later queried via \code{sqlQuery} or \code{sqlFetch}.  One could, however,
#'   arrange the SQL statements in \code{sqlFile} so that the last statement is
#'   a query or fetch which produces the desired results.  In that case, this 
#'   routine will return those results.
#'   
#' @details Function \code{F.run.sqlFile} is often paired with the \code{RODBC}
#'   function \code{sqlQuery}, which reads and returns tables housed in an
#'   Access database.
#'   
#'   Function \code{F.run.sqlFile} works by replacing variables in provided SQL
#'   statements with values passed to it via the \code{R} \code{"..."}
#'   parameter.
#' 
#' @section Modifying Access-developed SQL queries for use by R-package \code{RODBC}:
#' Report generation requires communication between the
#' underlying R code and the Access databases housing the data. The 32-bit
#' R-package \code{RODBC} performs the link between these two systems, allowing for
#' SQL-type queries to be sent from R to Access. The resulting queries, once
#' completed and housed in the Access database, can then be read into R.
#' 
#' Examples of original Access SQL queries that have been processed for use in R
#' can be found in the folder named in global variable \code{sql.code.dir}. Use
#' these as a guide when updating any new batch of SQL code.
#' 
#' In this package, Access SQL queries are not immediately readable by R (and
#' \code{RODBC}). Modifications must first be made. This list attempts to 
#' enumerate the necessary changes.
#' 
#' \itemize{
#' \item \strong{Comments} Most SQL scripts contain helpful comments. While 
#' helpful, \code{RODBC} must be made to recognize that these are in fact
#' comments, and not developed SQL strings. Use a double hyphen (\code{--}) to
#' demonstrate to \code{RODBC} a comment for which processing is unnecessary.
#' For example, \code{-- This is a comment and will not be read} would
#' communicate to RODBC that this particular line should be skipped, whereas 
#' \code{SELECT * FROM THIS.TABLE.HERE} forces RODBC to act. Note that double
#' hyphens need only be entered on the left. Additionally, the space following
#' the double hyphen is unnecessary. However, it makes comments more readable.
#' 
#' \item \strong{Quotes} All quoted strings must utilize single quotes
#' (\code{'example'}), and not double quotes (\code{"example"}) to wrap text
#' strings.  Double quotes force the underlying SQL strings to be
#' misinterpreted, and thus fail.
#' 
#' \item \strong{Variables} Many SQL queries developed for the Platform require
#' subsetting the data to chinook salmon via SQL \code{WHERE} clauses. However,
#' in most cases, if not all, the R code has been developed to allow for
#' selection of any piscine species. This means that the \code{WHERE} clause is
#' a variable set by the user. It also means that this variable quantity must be
#' passed from R to Access. Within the database, chinook salmon have the
#' character identifier "\code{161980}".
#' 
#' In the \code{campR} package, \code{WHERE} statements for chinook salmon are
#' identified via this identifier. However, because the R code has been
#' developed to allow this value to vary, SQL code has been adapted to take
#' variable values via \code{RODBC} and variable "\code{R.TAXON}". The 
#' appellation \code{R.TAXON} reflects the R-code name for the species variable,
#' and is how a variable quantity is communicated to Access.
#' 
#' \item \strong{\code{INTO SQL} Statements} Queries for use in the Platform are
#' developed for multiple use. This means that once a query for one set of
#' criteria is created, the code must be able to recreate a new query, based on
#' separate criteria, without error. Practically, this means that different
#' queries must be able to overwrite resultant tables in Access databases
#' flawlessly. The way that Access and \code{RODBC} does this varies; this 
#' necessitates more complex changes to developed SQL code.
#' 
#' Within Access, the use of the SQL keyword \code{INTO} forces a table that
#' currently exists to be dropped.  It also recreates the table into which data
#' are to be placed. For example, suppose within Access a query generates a
#' final table named \code{Table_fish}. Now, however, a new query is generated,
#' also within Access, creating a new \code{Table_fish}. The new
#' \code{Table_fish} overwrites the old \code{Table_fish}; Access does this via
#' SQL code resembling
#' 
#' \code{SELECT * INTO Table_fish FROM Table_helper;}
#' 
#' where the special keyword \code{INTO} deletes out the old \code{Table_fish}
#' and replaces it with the new version. Syntax of this nature must be modified
#' for use in \code{RODBC} R code -- the \code{INTO} statement does not work in
#' this way. While \code{RODBC} will create the named table, it will not first
#' delete out its previous version. This must be done explicitly.
#' 
#' To work around this, use a \code{DROP TABLE} statement. Then, the use of the
#' \code{INTO} statement will work within R. Explicitly, modifying the code
#' block above, write
#' 
#' \code{DROP TABLE Table_fish;} \code{SELECT * INTO Table_fish FROM
#' Table_helper;} as two separate SQL queries, where the semi-colon \code{;}
#' demonstrates the end of a SQL query.
#' 
#' One final to-do is required. Once a SQL query has been modified to include a 
#' \code{DROP TABLE} statement, an initial table must then be produced in all
#' Access databases in which the query is to be run. Practically, the first time
#' this particular query is run, SQL cannot drop the table \code{Table_fish}
#' because it will not exist, not having been created yet. To work around this,
#' from within Access, copy and paste any one of the existing tables, and rename
#' it to the name of the final table, which in this case, is \code{Table_fish}.
#' Note that some queries create multiple interim tables. These may also need to
#' be modified via \code{DROP TABLE} statements. On completion, the modified SQL
#' will be fooled into having a table to delete, and will work as intended.
#' 
#' \item \strong{Dates} Calendar dates utilized as values within a \code{WHERE}
#' clause must have preceding and antecedent \code{#}-signs added to them.
#'  }
#' 
#' @section Structured Query Language (SQL) Queries: Communication between
#'   \code{R} and Access databases takes place via the \code{RODBC} package and
#'   the SQL query language. Queries are organized into series, with different
#'   series manipulating data housed in Access databases in different ways. 
#'   Series within the \code{campR} package include the following.
#' \itemize{
#'   
#'   \item{\strong{All Catch}} Reproduces the functionality within the
#'   Quality-Control database button "ViewAllCatch."  Function
#'   \code{F.get.all.fish.data} calls this query after first running the Build
#'   Report Criteria query series.
#'   
#'   \item{\strong{Build Report Criteria}} A workhorse query called frequently 
#'   prior to the calling of other queries, due to its finding all 
#'   \code{trapVisitID}s between two specified dates; i.e., the \code{min.date} 
#'   and \code{max.date}.  This series results in table
#'   \code{TempReportCriteria_Trapvisit} in the underlying Access database.
#'   
#'   \item{\strong{Build Report Criteria Release}} A query similar to that of 
#'   the Build Report Criteria seres, but adapted for identifying releases. This
#'   series results in table \code{TempReportCriteria_Release} in the underlying
#'   Access database.
#'   
#'   \item{\strong{By Catch}} The means by which by-catch is identified.  
#'   
#'   \item{\strong{Efficiency Tests}} Compiles efficiency-test release and
#'   recapture data. This series depends on first running the Build Report
#'   Criteria series, followed by the Sample Period series.  In running, the 
#'   \code{includeCatch} variable is pre-set to \code{'yes'} for all recaptured 
#'   fish.
#'   
#'   In cases where releases temporally overlap, and for which a
#'   \code{trapVisitID} may fall within the test period of more than one
#'   release, the series adds an additional row for each release.  In this case,
#'   more than one row in the final table for each \code{trapVisitID} may
#'   result.  Thus, these are only unique with the addition of \code{releaseID}.
#'   
#'   This query series is inappropriate for programs using inclined plane and 
#'   other types of traps.
#'   
#'   Note that this series includes recaptures with zero records only for trap 
#'   samples collected during the 36-hour period following a release.  This
#'   prevents zero-recapture records from being added to the table for traps not
#'   deployed until several days following release.  Additionally, the series
#'   does not add release records when no recaptures occur within 36 hours of a
#'   release.  If a release has no recaptures, than all visits with that
#'   \code{releaseID} will have a recapture total of 0.
#'   
#'   Generally, it is possible a test may be approved, leading to the release of
#'   fish, even when no traps were deployed.  Currently, the series makes no
#'   accommodation for these, and so they are excluded.
#'   
#'   \item{\strong{Fishing Gaps}} Modifies the identifier for a trap, via its
#'   \code{trapPositionID}, when lengthy temporal gaps occur in its catch time
#'   series.  The running of this query requires series Build Report Criteria
#'   and Sample Period be run first.  The Fishing Gaps series only runs if a
#'   lengthy gap is found, which is currently defined as any gap in excess of
#'   10,080 minutes, or 7 days.  This value is set in the \code{GlobalVars}
#'   \code{R} function via global variable \code{fishingGapMinutes}. Traps
#'   identifiers are modified by appending a decimal to their corresponding 
#'   \code{trapPositionID}.
#'   
#'   \item{\strong{Non-valid Fishing}}  Displays target species by run.  Series
#'   results include trap gear codes and half-cone fields.  Currently, this
#'   series excludes adult and grilse life stages.
#'   
#'   Note that the series only pulls data for non-valid trap visits, where the
#'   variable \code{includeCatchID} equal \code{2}, and for which there are
#'   target fish in the catch.
#'   
#'  \item{\strong{Not Fishing}} Develops a table that itemizes hours not fishing.  The 
#'  resulting table references table \code{tempSamplingSummary} and is built 
#'  similarly. The table identifies gaps in sampling to the minute and includes 
#'  both \code{trapPositionID} and trapping date.  This means it can be used in 
#'  conjuction with table \code{tempSamplingSummary}.  The running of this query
#'  requires series Build Report Criteria and Sample Period be run first.
#'   
#'   \item{\strong{Sample Period}} Calculates the amount of time between starts
#'   and stops of a trapping instance, as itemized via variable
#'   \code{trapVisitID}.
#'   
#'   \item{\strong{Sum Chinook by Trap}} Sums all the chinook caught via trap, i.e., 
#'   variable \code{trapVisitID}.  Note that species Chinook salmon enters the
#'   series via variable \code{R.TAXON}.
#'   
#'   \item{\strong{Unmarked by Run and Lifestage}} Displays target species by run.  This 
#'   series requires each of the QryBuildReportCriteria, QrySamplePeriod, and 
#'   QryNotFishing series be developed first prior to implementation.  The
#'   series only pulls data for valid trap visits, where within table
#'   \code{tempSamplingSummary}, variable \code{includeCatchID} equals \code{1}
#'   and variable \code{visitTypeID} has a value larger than one. The table
#'   within the Access database holding the result of this series is
#'   \code{TempSumUnmarkedByTrap_Run_Final}.
#'   
#'   \item{\strong{Unmarked Chinook by Lifestage}} Displays target species data for life
#'   stage analysis. This series requires each of the QryBuildReportCriteria and
#'   QrySamplePeriod series be developed first.
#'   
#'   The series pulls catch data from valid trap visits, identified when
#'   variable \code{includeCatch} equals \code{'yes'} or when 
#'   \code{includeCatch} is non-valid, or equals \code{'no'}, but variable
#'   \code{visitTypeID} has a value greater than \code{1}, i.e., \code{Start
#'   trap & begin trapping}.  This series includes variables \code{weight} and
#'   \code{finalRunMethod} for use in life stage assignment.
#'   
#'   This series is different from that used in production analysis, being
#'   modified slightly for use in life stage development. This series uses two
#'   of the same output tables, but the final table name was reset to
#'   \code{TempSumUnmarkedByTrap_Run_Final2}.
#'   
#'   \item{\strong{Weekly Effort}}  Develops trapping effort data for graphical display. 
#'   The graph displays effort by Julian week, as summarized within the
#'   \code{Dates} table in the accomapanying Access database.  The graph also
#'   itemizes effort that resulted in a valid sample, effort for invalid samples
#'   not used in analyses, and times when no trapping effort was extended.
#'   
#'   The series depends on output tables from other SQL series.  As such, series
#'   QryBuildReportCriteria and QrySamplePeriod must be run prior to the
#'   implementation of this series. Dependent tables include
#'   \code{TempSamplingSummary}, \code{TempSamplingSummary_Final},
#'   \code{TempNonSamplingSummary}, \code{NonTrapSample1}, 
#'   \code{TempEffortSummary_a}, \code{TempEffortSummary_b}, and
#'   \code{TempEffortSummary_a1}. Note that table \code{TempEffortSummary_b}
#'   currently contains the final output table.
#'   
#' }
#' 
#' @seealso \code{sqlQuery}, \code{sqlFetch}
#'   
#' @author WEST Inc.
#' 
#' @examples
#' \dontrun{
#' #   ---- Open ODBC channel.
#' db <- get( "db.file", envir=.GlobalEnv )
#' ch <- odbcConnectAccess(db)
#' 
#' #   ---- Develop the hours fished and TempSamplingSummary table in the
#' #   ---- underlying Access database.  
#' F.run.sqlFile( ch, "QrySamplePeriod.sql", R.TAXON=taxon )
#' }
#'  
F.run.sqlFile <- function( ch, sqlFile, echo=TRUE, check.drops=FALSE, ... ){

  #   ---- An internal function to check for SQL errors.  This aborts everything 
  #   ---- when an error occurs.  This could be re-programmed to exit nicely with 
  #   ---- TRUE or FALSE.  Note that this detects an error if obj is 2 in length 
  #   ---- and has ERROR in the second position. 
  f.sql.error.check <- function( obj ){
    if( is.vector(obj) & length(obj) >= 2 ){
      if( length(grep("ERROR", obj[2])) > 0 ){
        
        #   ---- An error has occurred.
        stop(paste(obj,"\n"))
      }
    }
    FALSE
  }
  
  #   ---- Figure out what additional parameters there are, if any.
  param <- list(...)
  
  #	  ---- Get the sql.code.directory. Look in current directory if sql.code.dir 
  #   ---- is not found in the global environment.
  sql.dir <- get0( "sql.code.dir", envir=.GlobalEnv, ifnotfound="." )
  
  #   ---- Read the SQL statement.  Note that this is a vector of strings, with 
  #   ---- one for each line.
  sql.statements <- readLines( file.path(sql.dir, sqlFile), warn=FALSE )   
  
  #   ---- Substitute the parameters, if any.  Double quotes cannot be used to surround 
  #   ---- string parameters, as in "R.TAXON".  This messes with R's and SQL's string 
  #   ---- representations.  In your sql, use single quotes for strings.
  if( length(param) > 0 ){
    for( i in 1:length(param)){
      parm <- names(param)[i]
      parm.value <- param[[i]]
      sql.statements <- gsub( parm, parm.value, sql.statements, fixed=TRUE )
    }    
  }
  
  if( echo ) cat(paste("==== Running ", deparse(substitute(sqlFile)), "====\n"))
  
  #   ---- Remove comments. These must be at the start of a line. The whole line is dropped. 
  comment.lines <- grep( "^--", sql.statements )
  if( length(comment.lines) > 0 ){
      sql.statements <- sql.statements[ -comment.lines ]
  }
  
  #   ---- Make into one giant string for the computers.  
  sql.statements <- paste( sql.statements, collapse=" ") 
  
  #   ---- Collapse multiple spaces into one space.  Not necessary, but saves space.
  sql.statements <- gsub( " +", " ", sql.statements )
  
  #   ---- Now split the long string at the semicolons.  Do this because sqlQuery does
  #   ---- not allow more than one SQL statement at a time.  This way, I can put multiple 
  #   ---- sql statements in a file, and then loop over them.
  sql.statements <- strsplit( sql.statements, ";" )[[1]]
  
  #   ---- Take out leading spaces.
  sql.statements <- gsub( "^ ", "", sql.statements )
  
  #   ---- Take out any tabs, which mess things up.
  sql.statements <- gsub( "\t", "", sql.statements )
  
  #   ---- Take out the last element, caused by the end of file.
  sql.statements <- sql.statements[nchar(sql.statements) > 0]
  
  #   ---- Do the users' wishes; i.e., re drop statements.
  if( check.drops ){
    drops <- -1
  } else {
    
    #   ---- Don't want to check for errors on drop statements.  Flag these.
    drops <- grep( "^(?i)drop", sql.statements )
  }
  
  #   ---- Loop over the unique SQL statements.  
  for( i in 1:length(sql.statements) ){
  
    #    ---- Print out the SQL for us humans, if called for.
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
  
    #   ---- Run the query.
    ans <- sqlQuery( ch, sql.statements[i] )
      
    if( !(i %in% drops) ){
      
      #   ---- Check for an error.
      f.sql.error.check( ans )
    }
  }
  
  if( echo ){
    
      #   ---- If we get here, we are good.
      cat("Success.\n")
  }
  
  ans
}
