F.weekly.passage <- function( site, taxon, run, min.date, max.date, output.file, ci=TRUE ){
#
#   This is the outer wrapper function to estimate passage at a particular site, for a particular run, for a particular taxon for a particular year.
#
#   Inputs:
#   site = site number
#   taxon = taxon number
#   run = run number
#   min.date and max.date = minimum and maximum dates of visits to include in the analysis. This is a text
#           string in the format %Y-%m-%d, or YYYY-MM-DD.
#   output.file = name of .CSV file to put passage estimates into.  Full path is allowed.
#
#   NOTE: because we want to estimate GLM models by year, this routine should only estimate
#   passage for at most one year.  Check this.


    #   ---- Compute passage "by" week
    pass <- F.passage( site, taxon, run, min.date, max.date, "week", output.file, ci )


    invisible(pass)
}
