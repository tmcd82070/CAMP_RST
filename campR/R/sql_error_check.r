#' @export
#' 
#' @title F.sql.error.check
#' 
#' @description Check if a SQL statement executed correctly.
#'   
#' @param obj The results from an Access query. See Details.
#'   
#' @return \code{FALSE}, assuming successful completion of a SQL query.  Otherwise, an 
#'   error message to either the Console window, if running directly via R, or 
#'   to the \code{run_R.out} file, if running via the Platform.
#'   
#' @details  Function \code{F.sql_error_check} is used immediately after the
#' use of \code{RODBC} function \code{sqlQuery}, which queries an Access database via a text
#' string formatted as a SQL query. Function \code{sqlQuery}, if successful, results in
#' a dataframe, possibly with zero rows. Otherwise, a character vector with
#' error messsages is returned. Function \code{F.sql.error.check} examines if an error
#' message is returned, and if found, stops execution and prints an error
#' message to the \code{R_run.out} text file or the R Console.
#' 
#' @seealso \code{sqlQuery}
#'   
#' @author WEST Inc.
#' 
#' @examples
#' \dontrun{
#' #   ---- Set up an Access mdb in R.  
#' db.file <- "C:/yourPath/CAMP.mdb"                # The location of CAMP.mdb
#' db <- get( "db.file", envir=.GlobalEnv )           # Assign the mdb string.
#' ch <- odbcConnectAccess(db)                      # Connect R and Access.
#' 
#' #   ---- Conduct a good query.
#' ans <- sqlQuery( ch, "SELECT * FROM luRun" )     # Select all rows.
#' F.sql.error.check(ans)                           # Check for validity.  
#'                                                  #  --> Results in FALSE.
#' 
#' #   ---- Conduct a bad query.
#' ans <- sqlQuery( ch, "SELECT * FROM fakeTable" ) # Select all rows.
#' F.sql.error.check(ans)                           # Check for validity.  
#'                                                  #  --> Results in error message.  
#' }
#' @export

F.sql.error.check <- function( obj ){

  #   ---- Results from a bad call to sqlQuery lead to a character vector with the 
  #   ---- word "ERROR".  Look for these;  if none found, let the function complete.
  
  if( is.vector(obj) & length(obj) >= 2 ){
    if( length(grep("ERROR", obj[2])) > 0 ){
      #   An error has occurred.
      stop(paste(obj,"\n"))
    }
  }
  FALSE
}

