#' @export F.get.indiv.fish.data
#' 
#' @title F.get.indiv.fish.data
#' 
#' @description Fetch the fish data for a single taxon from an Access data base. The resulting data 
#'    set has one line per group of fish with the same forklength. 
#'    
#' @param site The identification number of the site for which estimates are 
#'   required.
#' @param taxon The species identifier indicating the type of fish of interest. 
#'   This is always \code{161980}; i.e., Chinook Salmon.
#' @param run The text seasonal identifier.  This is a one of \code{"Spring"}, \code{"Fall"},
#' \code{"Late Fall"}, or \code{"Winter"}.
#' @param min.date The start date for data to include. This is a text string in 
#'   the format \code{\%Y-\%m-\%d}, or \code{YYYY-MM-DD}.  
#' @param max.date The end date for data to include.  Same format as 
#'   \code{min.date}.
#' @param by A text string indicating the temporal unit over which daily
#'   estimated catch is to be summarized.  Can be one of \code{day},
#'   \code{week}, \code{month}, \code{year}.
#' @param  keep="unmarked"  A text string specifying the type of fish to retain.
#'   \code{keep="unmarked"} keeps all fish without efficiency trail marks, i.e.,
#'   all fish not in efficiency trial.  \code{keep="marked"} keeps only fish
#'   that were involved in an efficiency trial. \code{keep="all"}, i.e.,
#'   anything else, will keep all fish records --- both marked and unmarked.
#' 
#' @details To be included in the catch data, a record has to be from the site, 
#'    of the correct taxon, of the correct run, and between min and max date. F.size.by.date
#' 
#' @return 
#' 
#' @author WEST Inc.
#' 
#' @seealso \code{F.get.catch.data}, \code{F.sql.error.check}
#' 
#' @examples
#' \dontrun{
#' <insert examples>
#' 
#' }
F.get.indiv.fish.data <- function( site, taxon, run, min.date, max.date, keep="unmarked" ){
  
  # site <- 57000
  # taxon <- 161980
  # run <- "Fall"
  # min.date <- "2014-01-01"
  # max.date <- 2014-06-01"

  #   ---- Retrieve database file name and table names and any other constants.
  No.code <- get("No.code", pos=.GlobalEnv)
  Yes.code <- get("Yes.code", pos=.GlobalEnv)
  tables <- get( "table.names", envir=.GlobalEnv )
  db <- get( "db.file", envir=.GlobalEnv ) 

  #   ---- Save the start and stop dates;  use these to determine and filter visits.
  strt.dt <- as.POSIXct( min.date, format="%Y-%m-%d" )
  end.dt <- as.POSIXct( max.date, format="%Y-%m-%d" )
  run.season <- data.frame( start=strt.dt, end=end.dt )

  #   ---- Open ODBC channel.
  ch <- odbcConnectAccess(db)

  #   ---- Retreive common names for the site.  
  sites <- sqlQuery( ch, paste("SELECT siteName, siteAbbreviation, siteID, streamName FROM", tables["sites"], 
           "WHERE (siteID =", site, ")" ))
  F.sql.error.check(sites)        
  site.stream <- as.character(sites$streamName)
  site.abbr <- as.character(sites$siteAbbreviation)
  site.name <- as.character(sites$siteName)

  #   ---- Fetch subsite names.
  subsite.names <- sqlQuery( ch, paste("SELECT subSiteID, subSiteName FROM", tables["subsites"], 
          "WHERE (siteID =", site, ")" ))
  F.sql.error.check(subsite.names)
  subsite.names$subSiteName <- as.character(subsite.names$subSiteName)        

  #   ---- Fetch species name.
  sp.codes <- sqlQuery(ch, paste("SELECT taxonID, commonName FROM", tables["species.codes"]))
  F.sql.error.check(sp.codes)
  sp.commonName <- as.character(sp.codes$commonName[ sp.codes$taxonID %in% taxon ])

  #   ---- Fetch run name.
  runs <- sqlQuery(ch, paste( "SELECT run, runID FROM", tables["run.codes"] ))
  F.sql.error.check(runs)
  run.name <- as.character(runs$run[ runs$runID == run ])


  #   ---- Report useful info to the Console and/or out file.
  cat( paste(site.name, site.abbr, site.stream,  sep=":") )
  cat("\n")
  cat( paste(sp.codes$taxonID[ sp.codes$taxonID %in% taxon ], sp.commonName, run.name, sep=":") )
  cat("\n")
  
  #   ---- Fetch catch data.  
  tmp.df <- F.get.catch.data( site, taxon, min.date, max.date  )
  catch <- tmp.df$catch

  #   ---- Report useful info to the Console and/or out file.  
  if(nrow(catch) >= 20) {cat("First 20 catch records...\n"); print(catch[1:20,])} else {cat("Catch records...\n"); print(catch)}
  cat(paste(nrow(catch), "total records in catch table.\n")) 


  #   ---- Now, subset to run. We cannot do this in the SQL above because we need unknown 
  #   ---- runs in order to expand for plus counts.
  catch <- catch[ !is.na(catch$FinalRun) & catch$FinalRun == run.name, ]

  #   ---- Report useful information to the Console and/or out file.  
  cat("Subsites found...\n")
  print(unique(catch$TrapPosition))

  #   ---- Store values of run.season as attribute for convienance. 
  attr(catch, "site") <- site
  attr(catch, "site.name" ) <- site.name
  attr(catch, "site.abbr") <- site.abbr
  attr(catch, "site.stream") <- site.stream
  attr(catch, "species.name") <- sp.commonName
  attr(catch, "runID") <- run
  attr(catch, "run.name") <- run.name
  attr(catch, "run.season") <- run.season

  #   ---- If there are none of the particular taxon caught, there are no records in data frame catch. 
  #   ---- This is correct, just be sure to check for nrow(catch) > 0 in any routines that use these data.

  odbcClose(ch)

  catch

}