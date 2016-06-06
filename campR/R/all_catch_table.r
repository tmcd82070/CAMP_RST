#' @export
#' 
#' @title F.allCatch.table
#' 
#' @description Lists all catch, regardless of species.  
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
#' @return A \code{csv} containing catch information for all species.  A data
#'   frame containing the same information is also returned, but invisibly.
#'   
#' @details This function utilizes the All-Catch Series query to find and identify
#' all pertinent records within a catch.  See 
#'
#' @seealso \code{F.chinookByDate.table}
#' 
#' @examples  
#' 
#' #requires an mdb.
#'
F.allCatch.table <- function( site, min.date, max.date, output.file ){

  # site <- 7000
  # min.date <- "2010-01-01"
  # max.date <- "2010-05-30"
  # output.file <- NA

  #   ---- Build table in Access file containing trap visits we want.
  nvisits <- F.buildReportCriteria( site, min.date, max.date )

  if( nvisits == 0 ){
    warning("Your criteria returned no trapVisit table records.")
    return()
  }

  #   ---- Retrieve all catch records between two dates, regardless of species, taxon, etc.
  df <- F.get.all.fish.data( site, min.date, max.date )
  cat("Frequency table for all species found:\n")
  print(table(df$commonName))

  #   ---- Write out the data set.
  out.fn <- paste(output.file, "_all_catch.csv", sep="")

  write.table( df, file=out.fn, sep=",", col.names=T, row.names=F ) 

  #   ---- Send messages back to the interface.
  cat("SUCCESS - F.allCatch.table\n\n")
  cat(paste("Working directory:", getwd(), "\n"))
  cat(paste("R data frames saved in file:", "<no RData saved>", "\n\n"))
  cat("Number of files created in working directory = 1\n")
  cat(paste(out.fn, "\n"))
  cat("\n")
 
  invisible(df)

}

