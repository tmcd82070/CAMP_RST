#' @export 
#'   
#' @title F.size.by.date - Plot fork lengths through time.
#'   
#' @description Plot fork length (mm) of fish by date of catch for a particular
#'   site and year.
#'  
#' @param site The identification number of the site for which estimates are 
#'   required.
#'   
#' @param taxon The species identifier indicating the type of fish of interest. 
#'   This is always \code{161980}; i.e., Chinook Salmon.
#'   
#' @param run The run code.  This is an integer value that corresponds to 
#' a run listed in table \code{luRun} which is housed inside the Access database. 
#' At the time of this writing, the \code{luRun} table translated run codes as:
#' \itemize{
#'   \item 1 = Spring
#'   \item 2 = Summer
#'   \item 3 = Fall
#'   \item 4 = Winter
#'   \item 5 = Late fall
#'   \item 6 = Mixed
#' }
#' Normally, the translation from text (e.g., "fall") to code (e.g., 3) is done 
#' by the camp interface. 
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
#' @return A graphical \code{png} entitled via parameter \code{output.file}, displaying 
#' fork length in millimeters as a function of time.  Also, an accompanying \code{csv}
#' containing the data used in plotting.  
#' 
#' @section Details:
#' This routine compiles fish from the CAMP Access dBase using function
#'   \code{F.get.indiv.fish.data}.  Fish are included from both "valid" and 
#'   "invalid" trapping instances where "valid" means \code{includeCatchID = TRUE}.  Thus, 
#'   resulting graphs can display both valid and invalid fishing instances if 
#'   both types of trapping instances took place between
#'   \code{min.date} and \code{max.date.}
#'   
#'   Observations in the output graph are jittered slightly when plotted to show 
#'   the number of measurements on each date.  
#'   
#'   When no valid catch records are found, a blank \code{png} is returned.  
#'   
#' @seealso \code{F.get.indiv.fish.data}
#' 
#' @section Author:
#'  Trent McDonald and Jason Mitchell
#'   
#' @examples
#' \dontrun{
#' # Create a plot of fork length by data for the American.
#' site <- 57000
#' taxon <- 161980
#' run <- 3
#' min.date <- "2014-01-01"
#' max.date <- "2014-06-01"
#' output.file <- "American"
#' 
#' F.size.by.date(site,taxon,run,min.date,max.date,output.file)
#' 
#' # Testing on Trent's local machine
#' db.file <- file.path("C:/Users/trent/Documents/Projects/1200-RSTPlatform/ThePlatform",
#'          "CAMP_RST20220103-campR2.0.14/Data/TestingDBs/newStanislausCAMP_20Sept2018/CAMP.mdb")
#' output.file <- file.path("C:/Users/trent/Documents/Projects/1200-RSTPlatform/ThePlatform",
#'          "CAMP_RST20220103-campR2.0.14/Outputs/size.by.date_ST004X_2022-03-21_14-33-45")
#' F.size.by.date(site,taxon,run,min.date,max.date,output.file)
#' 
#' }
F.size.by.date <- function( site, taxon, run, min.date, max.date, output.file ){

  # site <- 57000
  # taxon <- 161980
  # run <- "Fall"
  # min.date <- "2014-01-01"
  # max.date <- "2014-06-30"
  # output.file <- "American"

  #   ---- Check that taxon is Chinook salmon.  
  if( taxon != 161980 ) stop("Cannot specify any species other than Chinook salmon, code 161980.")

  # Because retrieval of catch for fork length information is the same in 
  # multiple CAMP routines (e.g., size_by_date and length_freq) we call 
  # a separate routine to get the data.
  catch.df <- getCatchForkLenth( site, taxon, run, min.date, max.date )

  #   ---- Open a png graphics device.
  if( !is.na(output.file) ){
    out.graphs <- paste0(output.file, ".png")
    if(file.exists(out.graphs)){
      file.remove(out.graphs)
    }
    tryCatch({png(filename=out.graphs,width=7,height=7,units="in",res=600)}, error=function(x){png(filename=out.graphs)})
  }
  
  
  #   ---- It could be the case that we get a small number of records that make it through
  #   ---- to the drop stage above, but end up getting thrown out, most likely due to the
  #   ---- lifeStage being Unassigned, and making it through the plus-count algorithm 
  #   ---- non-sliced and -diced.  This happens on occasion.  This means, there is now no
  #   ---- data to plot.  So, repeat the "no data to plot" code here for this contingency. 
  if( nrow(catch.df) == 0 ){
    plot( c(0,1), c(0,1), xaxt="n", yaxt="n", type="n", xlab="", ylab="")
    text( .5,.5, "All Zero's\nCheck dates\nCheck that finalRunID is assigned to >=1 fish per visit\nCheck sub-Site were operating between dates")
    dev.off(dev.cur())
    cat("FAILURE - F.size.by.date\n\n")
    cat(paste("Working directory:", getwd(), "\n"))
    cat(paste("R data frames saved in file:", "<no RData saved>", "\n\n"))
    cat("Number of files created in working directory = 1\n")
    cat(paste(out.graphs, "\n"))
    cat("\n")        
    return(catch.df)
  }

  
  #   ---- Define some quantities useful for plotting.  
  x <- catch.df$EndTime
  y <- catch.df$forkLength
  lstage <- catch.df$lifeStage
  n <- catch.df$Unmarked
  valid <- catch.df$includeCatchID
  
  #   ---- Repeat the values for number of fish of that particular length.
  x <- rep(x, n)
  y <- rep(y, n)
  lstage <- rep(lstage, n)
  valid <- rep(valid, n)

  #   ---- Construct the main plot.
  plot( x, y, type="n", xlab="", ylab="", xaxt="n" )
  title( xlab="Date", cex.lab=1.5)
  title( ylab="Fork Length (mm)", cex.lab=1.5, line=2.5 )
  dts <- pretty(x)
  axis( side=1, at=dts, labels=format(dts, "%d%b%y") )

  #   ---- Determine the number of individual lifeStages present, and define
  #   ---- plotting colors accordingly.  
  life.stages <- sort(unique( lstage ))
  cat(paste("Lifestages plotted:", paste(life.stages, collapse=", "), "\n"))
  if( length(life.stages) == 3 ){
    mycol <- c("red", "orange", "blue")
  } else {
    mycol <- rainbow( length(life.stages) )
  }
  mypch <- rev(1:(0+length(life.stages)))

  #   ---- Without jittering, the volume of fish, given a fork length size, is 
  #   ---- difficult to determine for an individual date.  So jitter them.  
  ans.pts <- NULL
  for( l.s in life.stages ){
    ind <- l.s == lstage
    xx <- x[ind]
    yy <- y[ind]
    jitx <- rnorm(sum(ind), sd=60*60*3)
    jity <- 0
    validv <- ifelse(valid[ind]==1,'Fishing was successful.','Fishing was not successful.')
    points( xx + jitx, yy + jity, col=mycol[ which(l.s == life.stages)], pch=mypch[ which(l.s == life.stages)] )
    ans.pts <- rbind( ans.pts, data.frame(lifestage=l.s, date=xx, fork.length.mm=yy, date.jittered=xx+jitx, fork.length.mm.jittered=yy+jity, fishing.status=validv ))
  }

  #   ---- Sort.  
  ans.pts <- ans.pts[order(ans.pts$date, ans.pts$lifestage, ans.pts$date.jittered, ans.pts$fork.length.mm.jittered),]


  #   Add quantile lines
  xx <- as.numeric(x) 
  x.bs <- splines::bs( xx, df=6 )

  #   ---- Sometimes the quantile regression can be singular, due to problems 
  #   ---- in finding an inverse.  Capture this behavior.  
  rq.fit <- tryCatch(quantreg::rq( y ~ x.bs, tau=c(0.05, 0.95) ), error = function(e) e)
  xpred <- seq(quantile(xx,.01),quantile(xx,.99),length=200)
  xp.bs <- splines::bs( xpred, knots=attr(x.bs,"knots"), Boundary.knots=attr(x.bs,"Boundary.knots") )
  if(rq.fit[1] == 'Singular design matrix'){  
    
    #   ---- Put back to a POSIXct class for output.  
    class(xpred) <- class(x)
  } else {
    ypred <- cbind(1,xp.bs) %*% coef(rq.fit)
    lines( xpred, ypred[,1], col="black", lwd=3, lty=2 )
    lines( xpred, ypred[,2], col="black", lwd=3, lty=2 )
    
    #   ---- Put back to a POSIXct class for output.      
    class(xpred) <- class(x)  
    ans.qr <- data.frame( date=xpred, q.05=ypred[,1], q.95=ypred[,2] )
  }
  
  #   ---- Put together the main title for the plot.
  title( main=attr(catch.df, "site.name"), line=2, cex.main=2 )
  sp.string <- attr(catch.df, "species.name") 
  if( !is.na(attr(catch.df, "runID")) ){
    sp.string <- paste( sp.string, ", ", attr(catch.df, "run.name"), " run", sep="")
  }
  disc <- attr(catch.df, "disc")
  dts <- attr(catch.df, "run.season")
  dts <- paste( format(dts$start, "%d%b%Y"), "to", format(dts$end, "%d%b%Y") )
  sp.string <- paste( sp.string, ", ", dts, sep="")
  title( main=sp.string, line=1, cex.main=1 )
  title( main=disc, line=0.25, cex.main=0.70)

  #   ---- Construct the legend.  
  myleg <- attr(catch.df, "legendEntries")
  if(rq.fit[1] == 'Singular design matrix'){  
    legend( "topleft", legend=myleg, col=c(mycol,"black"), pch=c(mypch,NA), lty=c(rep(NA,length(mycol))), lwd=c(rep(NA,length(mycol))) )  
  } else {
    myleg <- c(myleg, "90% bounds")
    legend( "topleft", legend=myleg, col=c(mycol,"black"), pch=c(mypch,NA), lty=c(rep(NA,length(mycol)), 2), lwd=c(rep(NA,length(mycol)), 3) )
  }

  #   ---- Close the graphics file.
  dev.off()
  search()
  
  #   ---- Write out the csv housing the data used to make the graph.  
  out.pts <- paste(output.file, "_points.csv", sep="")
  write.table( ans.pts, file=out.pts, sep=",", row.names=FALSE)
  if(rq.fit[1] != 'Singular design matrix'){  
    out.qr <- paste(output.file, "_quantlines.csv", sep="")
    write.table( ans.qr, file=out.qr, sep=",", row.names=FALSE)
  }

  tableDeleter()
  
  #   ---- Send messages back to the interface
  nFiles <- 2 + (rq.fit[1] != 'Singular design matrix')
  cat("SUCCESS - F.size.by.date\n\n")
  cat(paste("Working directory:", getwd(), "\n"))
  cat(paste("R data frames saved in file:", "<no RData saved>", "\n\n"))
  cat(paste("Number of files created in working directory =", nFiles, "\n"))
  cat(paste(out.graphs, "\n"))
  cat(paste(out.pts, "\n"))
  if(rq.fit[1] != 'Singular design matrix'){  
    cat(paste(out.qr, "\n"))
  }
  cat("\n")
  
  invisible(catch.df)

}