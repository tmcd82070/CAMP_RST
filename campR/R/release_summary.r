#' @export F.release.summary
#' 
#' @title F.release.summary
#'   
#' @description Summarize efficiency trials, or releases, for a site, run, and
#'   taxon, between specified dates.
#'   
#' @param site The identification number of the site for which estimates are 
#'   required.
#'   
#' @param taxon The species identifier indicating the type of fish of interest. 
#'   This is always \code{161980}; i.e., Chinook Salmon.
#'   
#' @param run The biologist-assigned seasonal run.
#' 
#' @param min.date The start date for data to include. This is a text string in 
#'   the format \code{\%Y-\%m-\%d}, or \code{YYYY-MM-DD}.
#'   
#' @param max.date The end date for data to include.  Same format as 
#'   \code{min.date}.
#'   
#' @param output.file The name of the file prefix under which output is to be 
#'   saved.  Set to NA to plot to the Plot window.
#'   
#' @return A \code{csv} file summarizing efficiency trials for the specified 
#'   criteria.  A data frame containing the same information is also returned, 
#'   but invisibly.
#'   
#' @details Function \code{F.release.summary} calls helper functions \code{F.get.release.data}
#' and \code{F.summarize.releases} to fetch, and then process, recorded releases.  
#' The results returned from \code{F.summarize.releases} are then cleaned up for 
#' output via a \code{csv}.   
#' 
#' @seealso \code{F.get.release.data}, \code{F.summarize.releases}
#' 
#' @examples
#' \dontrun{
#' #   ---- Summarize releases for all efficiency trials involving Chinook
#' #   ---- salmon on the American River between Jan 16, 2013 and June 8, 2013.
#' site <- 57000
#' taxon <- 161980
#' min.date <- "2013-01-16"
#' max.date <- "2013-06-08"
#' output.file <- "American River"
#' 
#' df <- F.release.summary(site,taxon,min.date,max.date,output.file)
#' }
F.release.summary <- function(site,taxon,run,min.date,max.date,output.file){
  
  # site <- 57000
  # taxon <- 161980
  # run <- "Fall"
  # min.date <- "2010-01-01"
  # max.date <- "2010-05-30"
  # output.file <- NA

  #   ---- Make sure we have all temp tables.
  tableChecker()
  
  #   ---- Check that taxon is Chinook salmon.  
  if( taxon != 161980 ) stop("Cannot specify any species other than Chinook salmon, code 161980.")
  
  #   ---- Fetch efficiency data.
  release.df <- F.get.release.data.light( site, taxon, min.date, max.date )

  #   ---- Get a couple of variables from quickie lookups.
  db <- get( "db.file", envir=.GlobalEnv ) 
  ch <- RODBC::odbcConnectAccess(db)
  siteTable <- RODBC::sqlFetch( ch, "site" )[,c('siteName','siteAbbreviation','siteID')]
  runTable <- RODBC::sqlFetch( ch, "luRun" )   
  close(ch) 
  
  #   ---- Summarize the releases.
  if(is.null(release.df)){
    
    #   ---- In the case we have nothing, output as such, so the program finishes nicely.  
    out.fn <- paste(output.file, "_release_summary.csv", sep="")
    rs <- paste( format(as.POSIXlt(min.date,tz=""), "%d-%b-%Y"), "to", format(as.POSIXlt(max.date,tz=""), "%d-%b-%Y"))
    utils::write.csv(data.frame(Site=site,
                                "Site Abbreviation"=siteTable[siteTable$siteID == site,]$siteAbbreviation,
                                "Run season"=rs,
                                Run=runTable[runTable$runID == run,]$run,
                                Releases="No releases found."),
                     out.fn,row.names=FALSE)
    
    tableDeleter()
    
    #   ---- Send messages back to the interface.
    cat("SUCCESS - F.release.summary\n\n")
    cat(paste("Working directory:", getwd(), "\n"))
    cat(paste("R data frames saved in file:", "<No RData saved>", "\n\n"))
    cat("Number of files created in working directory = 1\n")
    cat(paste(out.fn, "\n"))
    cat("\n")
    
  } else {
    
    release.sum <- F.summarize.releases( release.df )

    #   ---- Write summary to CSV file.
    out.fn <- paste(output.file, "_release_summary.csv", sep="")
    rs <- paste( format(as.POSIXlt(min.date,tz=""), "%d-%b-%Y"), "to", format(as.POSIXlt(max.date,tz=""), "%d-%b-%Y"))
    nms <- names(release.sum)[1]
    for( i in 2:length(names(release.sum))) nms <- paste(nms, ",", names(release.sum)[i], sep="")
  
    sink(out.fn)
    cat(paste("Site=,", release.df$siteName[1], "\n", sep=""))
    cat(paste("Site abbreviation=,", siteTable[siteTable$siteID == site,]$siteAbbreviation, "\n", sep=""))
    cat(paste("Site ID=,", attr(release.df,"siteID"), "\n", sep=""))
    if(taxon == 161980){
      cat(paste("Species=,", "Chinook Salmon", "\n", sep=""))
    } else if(taxon == 161989){
      cat(paste("Species=,", "Steelhead / rainbow trout", "\n", sep=""))
    }
    cat(paste("Species ID=,", attr(release.df,"taxonID"), "\n", sep=""))
    cat(paste("Run=,", runTable[runTable$runID == run,]$run, "\n", sep=""))
    cat(paste("Run ID=,", run, "\n", sep=""))
    cat(paste("Run season=,", rs, "\n", sep=""))
    cat("\n")
    cat(nms)
    cat("\n")
    sink()
    utils::write.table( release.sum, file=out.fn, sep=",", append=TRUE, row.names=FALSE, col.names=FALSE)
  
    tableDeleter()
    
    #   ---- Send messages back to the interface.
    cat("SUCCESS - F.release.summary\n\n")
    cat(paste("Working directory:", getwd(), "\n"))
    cat(paste("R data frames saved in file:", "<No RData saved>", "\n\n"))
    cat("Number of files created in working directory = 1\n")
    cat(paste(out.fn, "\n"))
    cat("\n")
  
    invisible(release.sum)
  }

}
