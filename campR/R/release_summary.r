#' @export F.release.summary
#' 
#' @title F.release.summary
#' 
#' @description
#' 
#'    This is the outer wrapper function to summarize releases at a particular site, for a particular run, for a particular taxon for a particular year.
#' 
#'    Inputs:
#'    site = site number
#'    taxon = taxon number
#'    run = run number
#'    year = year number
#'    output.file = name of .CSV file to put passage estimates into.  Full path is allowed.
#' 
#' table.names <- data(table.names)  # Names of table in data base is stored in the campass package.  Eventually do this, then delete 'get' statement
#' table.names <- get( "table.names", env=.GlobalEnv )
#' db.name <- get( "db.file", env=.GlobalEnv ) 
#' 
#' 
#'    ---- Fetch efficiency data
#' 
#' @param site <describe argument>
#' @param taxon <describe argument>
#' @param run <describe argument>
#' @param min.date <describe argument>
#' @param max.date <describe argument>
#' @param output.file <describe argument>
#' 
#' @details <other comments found in file>
#'  get a couple of variables from quickie lookups
#'    ---- Or, could write summary to CSV file
#'      rs <- attr(release.df,"run.season")
#'      rs <- paste( format(rs[1], "%d-%b-%Y"), "to", format(rs[2], "%d-%b-%Y"))
#'    ---- Send messages back to the interface
#' 
#' @return <describe return value>
#' 
#' @author WEST Inc.
#' 
#' @seealso \code{\link{<related routine>}}, \code{\link{<related routine>}}
#' 
#' @examples
#' <insert examples>
#' 
F.release.summary <- function(site,taxon,run,min.date,max.date,output.file){
#
#   This is the outer wrapper function to summarize releases at a particular site, for a particular run, for a particular taxon for a particular year.
#
#   Inputs:
#   site = site number
#   taxon = taxon number
#   run = run number
#   year = year number
#   output.file = name of .CSV file to put passage estimates into.  Full path is allowed.

    #table.names <- data(table.names)  # Names of table in data base is stored in the campass package.  Eventually do this, then delete 'get' statement
    #table.names <- get( "table.names", env=.GlobalEnv )
    #db.name <- get( "db.file", env=.GlobalEnv ) 


    #   ---- Fetch efficiency data
    release.df <- F.get.release.data( site, taxon, min.date, max.date  )

    #   ---- Summarize the releases
    release.sum <- F.summarize.releases( release.df )

    #   ---- Could write to a Latex format table for inclusion in a later report.
    #F.latex.recapSummary(rel.df)

    # get a couple of variables from quickie lookups
    db <- get( "db.file", env=.GlobalEnv ) 
    ch <- odbcConnectAccess(db)
      siteTable <- sqlFetch( ch, "site" )[,c('siteName','siteAbbreviation','siteID')]
      runTable <- sqlFetch( ch, "luRun" )   
    close(ch) 
    
    
    #   ---- Or, could write summary to CSV file
    out.fn <- paste(output.file, "_release_summary.csv", sep="")
#     rs <- attr(release.df,"run.season")
#     rs <- paste( format(rs[1], "%d-%b-%Y"), "to", format(rs[2], "%d-%b-%Y"))
    rs <- paste( format(as.POSIXlt(min.date,tz=""), "%d-%b-%Y"), "to", format(as.POSIXlt(max.date,tz=""), "%d-%b-%Y"))
    nms <- names(release.sum)[1]
    for( i in 2:length(names(release.sum))) nms <- paste(nms, ",", names(release.sum)[i], sep="")

    sink(out.fn)
    cat(paste("Site=,", release.df$siteName[1], "\n", sep=""))
    cat(paste("Site abbreviation=,", siteTable[siteTable$siteID == site,]$siteAbbreviation, "\n", sep=""))
    cat(paste("Site ID=,", attr(release.df,"siteID"), "\n", sep=""))
    cat(paste("Species=,", "Chinook Salmon", "\n", sep=""))
    cat(paste("Species ID=,", attr(release.df,"taxonID"), "\n", sep=""))
    cat(paste("Run=,", runTable[runTable$runID == run,]$run, "\n", sep=""))
    cat(paste("Run ID=,", run, "\n", sep=""))
    cat(paste("Run season=,", rs, "\n", sep=""))
    cat("\n")
    cat(nms)
    cat("\n")
    sink()

    write.table( release.sum, file=out.fn, sep=",", append=TRUE, row.names=FALSE, col.names=FALSE)

    #   ---- Send messages back to the interface
    cat("SUCCESS - F.release.summary\n\n")
    cat(paste("Working directory:", getwd(), "\n"))
    cat(paste("R data frames saved in file:", "<No RData saved>", "\n\n"))
    cat("Number of files created in working directory = 1\n")
    cat(paste(out.fn, "\n"))
    cat("\n")


    invisible(release.sum)
}
