#' @export F.byCatch.table
#' 
#' @title F.byCatch.table
#' 
#' @description
#' 
#'    List all non-chinook catches,
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
#' @param  site describe argument
#' @param  min.date describe argument
#' @param  max.date describe argument
#' @param  output.file  describe argument
#' 
#' @details other comments found in file
#'    ---- Write out the data set
#'    ---- Produce a pie chart and write out the data for it
#'    ---- Open PNG device
#'    ---- Send messages back to the interface
#' 
#' @return describe return value
#' 
#' @author WEST Inc.
#' 
#' @seealso \code{\link{related routine}}, \code{\link{related routine}}
#' 
#' @examples
#' #insert examples
#' 
F.byCatch.table <- function( site, min.date, max.date, output.file ){
#
#   List all non-chinook catches,
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
df <- F.getByCatch( site, min.date, max.date )  # this does the cross-walk to CAMP lifestages



#   ---- Write out the data set
out.fn <- paste(output.file, "_by_catch.csv", sep="")
write.table( df, file=out.fn, sep=",", col.names=T, row.names=F )  # careful, this overwrites any old file

#   ---- Produce a pie chart and write out the data for it
tmp2 <- rep(df$Taxon, df$n)
tmp3 <- sort(table(tmp2))
tmp.pct <- tmp3/sum(tmp3)
tmp4 <- tmp3[ tmp.pct > 0.01 ]
tmp4 <- c( sum(tmp3[tmp.pct <= 0.01]), tmp4)
names(tmp4)[1] <- paste("Other (", sum(tmp.pct <= 0.01), " species)", sep="")
names(tmp4) <- paste0(names(tmp4)," (",tmp4,") ")   # update 12/22/2015 to add counts to species in chart.

out.fn <- c(out.fn, paste(output.file, "_piechart.csv", sep=""))
tmp2 <- data.frame( Species=names(tmp4), Count=tmp4, Proportion=tmp4/sum(tmp4) )
write.table( tmp2, file=out.fn[2], sep=",", col.names=T, row.names=F )  # careful, this overwrites any old file

if( !is.na(output.file) ){
    #   ---- Open PNG device
    out.fn <- c(out.fn, paste(output.file, "_byCatch_pie.png", sep=""))
    if(file.exists(out.fn[3])){
        file.remove(out.fn[3])
    }
    tryCatch({png(file=out.fn[3],width=7,height=7,units="in",res=600)}, error=function(x){png(file=out.fn[3])})  # produces hi-res graphs unless there's an error, then uses default png settings
    
}

pie(tmp4, col=rainbow(length(tmp4)), radius=.65, cex=0.75)

if( !is.na(output.file) ){
    dev.off(dev.cur())
}

#   ---- Send messages back to the interface
cat("SUCCESS - F.byCatch.table\n\n")
cat(paste("Working directory:", getwd(), "\n"))
cat(paste("R data frames saved in file:", "<no RData saved>", "\n\n"))
cat("Number of files created in working directory =", length(out.fn), "\n")
cat(paste(out.fn, "\n"))
cat("\n")

invisible(df)


}
