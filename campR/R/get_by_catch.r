#' @export F.getByCatch
#' 
#' @title F.getByCatch 
#' 
#' @description Retreive all non-Chinook catch data between two dates at a site.
#' 
#' @param site The identification number of the site for which estimates are 
#'   required.
#' @param min.date The start date for data to include. This is a text string in 
#'   the format \code{\%Y-\%m-\%d}, or \code{YYYY-MM-DD}.
#' @param max.date The end date for data to include.  Same format as 
#'   \code{min.date}.
#'   
#' @return A \code{csv} with all non-Chinook records between the two dates
#'   specified at the specified site.
#'   
#' @details Function \code{F.getByCatch} utilizes query sequence By Catch 
#' to obtain all non-Chinook records within a catch.  See \code{F.run.sqlFile}
#' for details.  
#'   
#' @seealso \code{F.run.sqlFile}
#' 
#' @author WEST Inc.  
#' 
#' @examples
#' \dontrun{
#' df <- F.getByCatch(57000,"2013-01-16","2013-06-08")
#' }
F.getByCatch <- function( site, min.date, max.date ){

  # site <- 7000
  # min.date <- "2010-01-01"
  # max.date <- "2010-05-30"
  
  #   ---- Open ODBC channel.
  db <- get( "db.file", envir=.GlobalEnv )
  ch <- odbcConnectAccess(db)

  cat("SQL to retrieve BY-CATCH records between ")
  cat(paste(min.date, "and", max.date, "\n"))

  #   ---- Execute the final SQL statement.
  catch <- F.run.sqlFile( ch, "QryByCatch.sql" )
  odbcClose(ch)
  
  cat(paste(nrow(catch), "records retrieved.\n\n"))

  if(nrow(catch) >= 10) {
    cat("First 10 records...\n")
    print(catch[1:10,])
  } else {
    cat("All records...\n")
    print(catch)
  }

  catch

}
