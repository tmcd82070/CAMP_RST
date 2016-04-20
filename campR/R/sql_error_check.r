#' @title Check if a SQL statement executed correctly.
#'   
#' @param obj The results from an Access query. See 'Details.'
#'   
#' @return FALSE, assuming successful completion of a SQL query.  Otherwise, an 
#'   error message to either the Console window, if running directly via R, or 
#'   to the run_R.out file, if running via the Platform.
#'   
#' @section:  Details: Function F.sql_error_check is used immediately after the
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
#' # db.file <- "C:/yourPath/CAMP.mdb"              # the location of CAMP.mdb
#' db <- get( "db.file", env=.GlobalEnv )           # assign the mdb string to an object
#' ch <- odbcConnectAccess(db)                      # open a connection between R and Access
#' 
#' # a good query
#' ans <- sqlQuery( ch, "SELECT * FROM luRun" )     # select all rows via a SQL query
#' F.sql.error.check(ans)                           # check for query result validity
#' 
#' # a bad query
#' ans <- sqlQuery( ch, "SELECT * FROM fakeTable" ) # select all rows via a SQL query
#' F.sql.error.check(ans)                           # check for query result validity
#' 
#' @export

F.sql.error.check <- function( obj ){

  # results from a bad call to sqlQuery lead to a character vector with the 
  # word "ERROR".  look for these;  if none found, let the function complete.
  
  if( is.vector(obj) & length(obj) >= 2 ){
    if( length(grep("ERROR", obj[2])) > 0 ){
      #   An error has occurred.
      stop(paste(obj,"\n"))
    }
  }
  FALSE
}
