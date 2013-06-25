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
    release.df <- F.get.release.data( site, run, min.date, max.date  )

    #   ---- Summarize the releases
    release.sum <- F.summarize.releases( release.df )

    #   ---- Could write to a Latex format table for inclusion in a later report.
    #F.latex.recapSummary(rel.df)

    #   ---- Or, could write summary to CSV file
    out.fn <- paste(output.file, "_release_summary.csv", sep="")
    rs <- attr(release.df,"run.season")
    rs <- paste( format(rs[1], "%d-%b-%Y"), "to", format(rs[2], "%d-%b-%Y"))
    nms <- names(release.sum)[1]
    for( i in 2:length(names(release.sum))) nms <- paste(nms, ",", names(release.sum)[i], sep="")

    sink(out.fn)
    cat(paste("Site=,", attr(release.df,"site.name"), "\n", sep=""))
    cat(paste("Site abbreviation=,", attr(release.df,"site.abbr"), "\n", sep=""))
    cat(paste("Site ID=,", attr(release.df,"siteID"), "\n", sep=""))
    cat(paste("Species=,", attr(release.df,"species.name"), "\n", sep=""))
    cat(paste("Species ID=,", attr(release.df,"taxonID"), "\n", sep=""))
    cat(paste("Run=,", attr(release.df,"run.name"), "\n", sep=""))
    cat(paste("Run ID=,", attr(release.df,"runID"), "\n", sep=""))
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
