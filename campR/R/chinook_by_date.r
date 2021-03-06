#' @export
#' 
#' @title F.chinookByDate.table
#' 
#' @description Summarize chinook salmon by date between two dates.
#' 
#' @param site The identification number of the site for which estimates are 
#'   required.
#' @param min.date The start date for data to include. This is a text string in 
#'   the format \code{\%Y-\%m-\%d}, or \code{YYYY-MM-DD}.
#' @param max.date The end date for data to include.  Same format as 
#'   \code{min.date}.
#' @param output.file The name of the file prefix under which output is to be 
#'   saved. 
#'   
#' @return A \code{csv} containing Chinook salmon catch information.  A data
#'   frame containing the same information is also returned, but invisibly.
#'   
#' @details As its name implies, function \code{F.chinookByDate.table} retrieves
#'   only Chinook salmon records, as specified by setting variable 
#'   \code{taxon=161980} within the appropriate query series.  The function
#'   utilizes helper function \code{F.get.all.catch.data} to handle the Sum
#'   Chinook by Trap query series.  See section Structured Query Language (SQL)
#'   Queries in function \code{F.run.sqlFile} for more information. Note that
#'   function \code{F.get.catch.data}, utilized in the estimation of passage,
#'   differs from \code{F.get.all.catch.data}.
#'   
#' @seealso \code{F.get.catch.data}
#'   
#' @examples 
#' \dontrun{
#' #   ---- Get all Chinook on the American.
#' # site <- 57000
#' # min.date <- "2013-01-01"
#' # max.date <- "2013-06-01"
#' # output.file <- NA
#' 
#' F.chinookByDate.table(site,min.date,max.date,output.file)
#' }
F.chinookByDate.table <- function(site, min.date, max.date, output.file ){

  # site <- 7000
  # min.date <- "2010-01-01"
  # max.date <- "2010-05-30"
  # output.file <- NA

  #   ---- Make sure we have all temp tables.
  tableChecker()
  
  taxon <- 161980
  catch <- F.get.all.catch.data( site, taxon, min.date, max.date )

  #   ---- Write out the resulting data set.
  out.fn <- paste(output.file, "_chinookByDate.csv", sep="")
  write.table( catch, file=out.fn, sep=",", col.names=T, row.names=F )  

  tableDeleter()
  
  #   ---- Send messages back to the interface.
  cat("SUCCESS - F.chinookByDate.table\n\n")
  cat(paste("Working directory:", getwd(), "\n"))
  cat(paste("R data frames saved in file:", "<no RData saved>", "\n\n"))
  cat("Number of files created in working directory = 1\n")
  cat(paste(out.fn, "\n"))
  cat("\n")
   
  invisible(catch)

}
