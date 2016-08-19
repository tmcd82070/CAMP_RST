#' @export
#' 
#' @title F.byCatch.table
#' 
#' @description List all non-Chinook catch.
#'    
#' @param site The identification number of the site for which estimates are 
#'   required.
#' @param min.date The start date for data to include. This is a text string in 
#'   the format \code{\%Y-\%m-\%d}, or \code{YYYY-MM-DD}.
#' @param max.date The end date for data to include.  Same format as 
#'   \code{min.date}.
#' @param output.file The name of the file prefix under which output is to be 
#'   saved.  Set to NA to plot to the Plot window.
#'   
#' @return A \code{csv} file containing all non-Chinook catch, along with a
#'   \code{png} pie chart detailing underlying counts and proportions of caught
#'   species.
#'   
#' @details Query sequence Build Report Criteria builds the appropriate time
#'   frame of interest while query series By Catch intemize the non-Chinook
#'   catch.  See section Structured Query Language (SQL) Queries in
#'   \code{F.run.sqlFile} for details.
#'   
#'   Function \code{F.byCatch.table} calls helper function \code{F.getByCatch} 
#'   to obtain the non-Chinook catch.
#'   
#' @seealso \code{F.run.sqlFile}, \code{F.getByCatch}
#' 
#' @author WEST Inc.
#' 
#' @examples
#' \dontrun{
#' #   ---- Find all non-Chinook catch resulting from fishing the American
#' #   ---- River, from Jan. 16, 2013 through June 8, 2103.  
#' F.byCatch.table(57000,"2013-01-16","2013-06-08",output.file="American River")
#' }
F.byCatch.table <- function( site, min.date, max.date, output.file ){
  
  # site <- 57000
  # min.date <- "2013-01-16
  # max.date <- "2013-06-08

  #   ---- Build table in Access file containing trap visits we want.
  nvisits <- F.buildReportCriteria( site, min.date, max.date )
  
  if( nvisits == 0 ){
    warning("Your criteria returned no trapVisit table records.")
    return()
  }
  
  #   ---- Retrieve all catch records between two dates, regardless of species, taxon, etc.
  df <- F.getByCatch( site, min.date, max.date )  
  
  #   ---- Write out the data set.
  out.fn <- paste(output.file, "_by_catch.csv", sep="")
  write.table( df, file=out.fn, sep=",", col.names=T, row.names=F )  
  
  #   ---- Produce a pie chart and write out the data for it.
  tmp2 <- rep(df$Taxon, df$n)
  tmp3 <- sort(table(tmp2))
  tmp.pct <- tmp3/sum(tmp3)
  tmp4 <- tmp3[ tmp.pct > 0.01 ]
  tmp4 <- c( sum(tmp3[tmp.pct <= 0.01]), tmp4)
  names(tmp4)[1] <- paste("Other (", sum(tmp.pct <= 0.01), " species)", sep="")
  names(tmp4) <- paste0(names(tmp4)," (",tmp4,") ")   
  
  out.fn <- c(out.fn, paste(output.file, "_piechart.csv", sep=""))
  tmp2 <- data.frame( Species=names(tmp4), Count=tmp4, Proportion=tmp4/sum(tmp4) )
  write.table( tmp2, file=out.fn[2], sep=",", col.names=T, row.names=F )  
  
  if( !is.na(output.file) ){
    
    #   ---- Open PNG device.
    out.fn <- c(out.fn, paste(output.file, "_byCatch_pie.png", sep=""))
    if(file.exists(out.fn[3])){
      file.remove(out.fn[3])
    }
      
    #   ---- Produce high-resolutions graphs unless there's an error, at 
    #   ---- which point, then use default png settings.
    tryCatch({png(filename=out.fn[3],width=7,height=7,units="in",res=600)}, 
              error=function(x){png(filename=out.fn[3])})  
  }
  
  pie(tmp4, col=rainbow(length(tmp4)), radius=.65, cex=0.75)
  
  if( !is.na(output.file) ){
    dev.off(dev.cur())
  }
  
  #   ---- Send messages back to the interface.  
  cat("SUCCESS - F.byCatch.table\n\n")
  cat(paste("Working directory:", getwd(), "\n"))
  cat(paste("R data frames saved in file:", "<no RData saved>", "\n\n"))
  cat("Number of files created in working directory =", length(out.fn), "\n")
  cat(paste(out.fn, "\n"))
  cat("\n")
  
  invisible(df)

}