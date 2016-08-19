#' @export F.get.all.fish.data
#'   
#' @title F.get.all.fish.data
#'   
#' @description Fetch all fish data, regardless of taxon, from an Access data 
#'   base between two dates.
#'   
#' @param site The identification number of the site for which estimates are 
#'   required.
#' @param min.date The start date for data to include. This is a text string in 
#'   the format \code{\%Y-\%m-\%d}, or \code{YYYY-MM-DD}.
#' @param max.date The end date for data to include.  Same format as 
#'   \code{min.date}.
#'   
#' @details This is a generalization of \code{F.get.indiv.fish.data} which
#'   returns records for a single taxon.  Function \code{F.get.all.fish.data}
#'   utilizes the All Catch query series in order to obtain the proper data.
#'   See section Structured Query Language (SQL) Queries in function 
#'   \code{F.run.sqlFile}.  
#'   
#' @return A data frame containing all catch, regardless of taxon, between the 
#'   specified dates.
#'   
#' @author WEST Inc.
#'   
#' @seealso \code{F.get.indiv.fish.data}
#'   
#' @examples
#' \dontrun{
#' #   ---- Obtain all fish on the American between the specified dates. 
#' site <- 57000
#' min.date <- "2013-01-01"
#' max.date <- "2013-06-01" 
#' F.get.all.fish.data(site,min.date,max.date)
#' }
F.get.all.fish.data <- function( site, min.date, max.date ){
  
  # site <- 57000
  # min.date <- "2014-01-01"
  # max.date <- "2014-06-30"

  #   ---- Open ODBC channel.
  db <- get( "db.file", envir=.GlobalEnv ) 
  ch <- odbcConnectAccess(db)

  #   ---- Communicate to the Console and/or out file.  
  cat("SQL to retrieve ALL catch records between ")
  cat(paste(min.date, "and", max.date, "\n"))

  #   ---- Execute the final SQL statement
  catch <- F.run.sqlFile( ch, "QryAllCatch.sql" )

  #   ---- Communicate to the Console and/or out file. 
  cat(paste(nrow(catch), "records retrieved.\n\n"))
  if(nrow(catch) >= 10) {cat("First 10 records...\n"); print(catch[1:10,])} else {cat("Catch records...\n"); print(catch)}

  #   ---- Check for missing catches.
  if( any( is.na(catch$n) )){
    cat("Number of fish is missing for the following records:\n")
    print( catch[ is.na(catch$n), ] )
    stop("There are missing catches. Make sure at least 0 is entered for every count. ")
  }

  #   ---- Communicate to the Console and/or out file.  
  cat("Subsites found...\n")
  subSites.found <- sort(unique(catch$trapPositionID))
  print(subSites.found)
  subsite.string <- paste(subSites.found, collapse="+")

  odbcClose(ch)
  catch

}