#' @export F.chinookByDate.table
#' 
#' @title F.chinookByDate.table
#' 
#' @description
#' 
#'    Summarize chinook by date between two dates
#' 
#' 
#' 
#'    ---- Retrieve all chinook records between two dates. 
#'         run does not work at this point
#' 
#' @param site <describe argument>
#' @param  min.date <describe argument>
#' @param  max.date <describe argument>
#' @param  output.file  <describe argument>
#' 
#' @details <other comments found in file>
#' NA
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
F.chinookByDate.table <- function(site, min.date, max.date, output.file ){
#
#   Summarize chinook by date between two dates
#


#   ---- Retrieve all chinook records between two dates. 
#        run does not work at this point
taxon <- 161980
catch <- F.get.all.catch.data( site, taxon, min.date, max.date )


#   ---- Write out the data set
out.fn <- paste(output.file, "_chinookByDate.csv", sep="")

write.table( catch, file=out.fn, sep=",", col.names=T, row.names=F )  # careful, this overwrites any old file


#   ---- Send messages back to the interface
cat("SUCCESS - F.chinookByDate.table\n\n")
cat(paste("Working directory:", getwd(), "\n"))
cat(paste("R data frames saved in file:", "<no RData saved>", "\n\n"))
cat("Number of files created in working directory = 1\n")
cat(paste(out.fn, "\n"))
cat("\n")
 
invisible(catch)


}
