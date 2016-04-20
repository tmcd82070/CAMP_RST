#' @export F.allCatch.table
#' 
#' @title F.allCatch.table
#' 
#' @description
#' 
#'    List all catches, regardless of species, etc. 
#' 
#'    Input:
#'    site = site ID of the place we want, trap locaton 
#' 
#'    Output:
#'    The take table is output to a CSV file
#' 
#' 
#'    Build table in Access file containing trap visits we want
#' 
#' @param  site <describe argument>
#' @param  min.date <describe argument>
#' @param  max.date <describe argument>
#' @param  output.file  <describe argument>
#' 
#' @details <other comments found in file>
#'    ---- Write out the data set
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
F.allCatch.table <- function( site, min.date, max.date, output.file ){
#
#   List all catches, regardless of species, etc. 
#
#   Input:
#   site = site ID of the place we want, trap locaton 
#
#   Output:
#   The take table is output to a CSV file
#

#   Build table in Access file containing trap visits we want
nvisits <- F.buildReportCriteria( site, min.date, max.date )

if( nvisits == 0 ){
    warning("Your criteria returned no trapVisit table records.")
    return()
}


#   ---- Retrieve all catch records between two dates, regardless of species, taxon, etc.
df <- F.get.all.fish.data( site, min.date, max.date )  # this does the cross-walk to CAMP lifestages


cat("Frequency table for all species found:\n")
print(table(df$commonName))

#   ---- Write out the data set
out.fn <- paste(output.file, "_all_catch.csv", sep="")

write.table( df, file=out.fn, sep=",", col.names=T, row.names=F )  # careful, this overwrites any old file


#   ---- Send messages back to the interface
cat("SUCCESS - F.allCatch.table\n\n")
cat(paste("Working directory:", getwd(), "\n"))
cat(paste("R data frames saved in file:", "<no RData saved>", "\n\n"))
cat("Number of files created in working directory = 1\n")
cat(paste(out.fn, "\n"))
cat("\n")
 
invisible(df)


}
