#' @export F.size.by.date
#'   
#' @title F.size.by.date
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
#' @param run The text seasonal identifier.  This is a one of \code{"Spring"}, 
#'   \code{"Fall"}, \code{"Late Fall"}, or \code{"Winter"}.
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
#' @return A graphical \code{png} entitled via parameter \code{output.file}, displyaing 
#' fork length in millimeters as a function of time.  Also, an accompanying \code{csv}
#' containing the data used in plotting.  
#' 
#' @details Function \code{F.size.by.date} first compiles fish data via function
#'   \code{F.get.indiv.fish.data}.  Trapping instances include both valid and
#'   invalid catch, as determined via variable \code{includeCatchID}.  Thus, 
#'   resulting graphs may display both valid and invalid fishing instances, if 
#'   both trappining instances took place within the time period specified 
#'   via \code{min.date} and \code{max.date.}
#'   
#'   All observations are jittered when plotted, so as to ensure the volume of 
#'   any one particular date and fork length combination can be visualized.  
#'   
#'   In the case when no valid catch records are found, and hence no fish can 
#'   contribute to the analysis, a blank \code{png} stating as such is returned.  
#'   
#' @seealso \code{F.get.indiv.fish.data}
#' 
#' @author WEST Inc.
#'   
#' @examples
#' \dontrun{
#' # Create a plot of fork length by data for the American.
#' site <- 57000
#' taxon <- 161980
#' run <- "Fall"
#' min.date <- "2014-01-01"
#' max.date <- "2014-06-01"
#' output.file <- "American"
#' 
#' F.size.by.date(site,taxon,run,min.date,max.date,output.file)
#' }
F.size.by.date <- function( site, taxon, run, min.date, max.date, output.file ){

  # site <- 57000
  # taxon <- 161980
  # run <- "Fall"
  # min.date <- "2014-01-01"
  # max.date <- "2014-06-30"
  # output.file <- "American"

  #   ---- Get global environment stuff.
  db.file <- get("db.file",envir=.GlobalEnv)
  table.names <- get("table.names",envir=.GlobalEnv)
  
  #   ---- Check that taxon is Chinook salmon.  
  if( taxon != 161980 ) stop("Cannot specify any species other than Chinook salmon, code 161980.")

  #   ---- Open a png graphics device.
  if( !is.na(output.file) ){
    out.graphs <- paste(output.file, "_size_by_date.png", sep="")
    if(file.exists(out.graphs)){
      file.remove(out.graphs)
    }
    tryCatch({png(filename=out.graphs,width=7,height=7,units="in",res=600)}, error=function(x){png(filename=out.graphs)})
  }

  #   ---- Open an ODBC channel and retrieve lifestage labels.
  ch <- odbcConnectAccess(db.file)
  CAMP.life.stage <- sqlFetch(ch, table.names["CAMP.life.stages"])
  rst.life.stage <- sqlFetch(ch, table.names["life.stages"])
  close(ch)

  #   ---- Retrieve basic data set, with one line per fish or group of fish of same length.
  catch.df  <- F.get.indiv.fish.data( site, taxon, run, min.date, max.date, keep="unmarked" )

  #   ---- Remove unnecessary variables.  
  catch.df$preUnmarked <-
  catch.df$halfConeAssignedCatch <-
  catch.df$oldtrapPositionID <-
  catch.df$halfConeUnassignedCatch <-
  catch.df$assignedCatch <-
  catch.df$unassignedCatch <-
  catch.df$modUnassignedCatch <- catch.df$modAssignedCatch <- NULL

  #   ---- When catch.df has no data, it doesn't get batchDates added.  Add this.
  if(nrow(catch.df) == 0){
    names(catch.df)[names(catch.df) == 'SampleDate'] <- 'batchDate'
  }

  #   ---- Grab non-valid catch, while preserving the attributes from the catch query.
  attributesSafe <- attributes(catch.df)
  db <- get( "db.file", envir=.GlobalEnv ) 
  ch <- odbcConnectAccess(db)

  F.run.sqlFile( ch, "QryNonValidFishing.sql", R.TAXON=taxon )   
  nvCatch <- sqlFetch( ch, "TempSumUnmarkedByTrap_Run_X_final" )       
  F.sql.error.check(nvCatch)

  #   ---- Fetch run name for use in reporting and query restrictions.
  tables <- get( "table.names", envir=.GlobalEnv )
  runs <- sqlQuery(ch, paste( "SELECT run, runID FROM", tables["run.codes"] ))
  F.sql.error.check(runs)
  run.name <- as.character(runs$run[ runs$runID == run ])
  
  close(ch)

  #   ---- Construct the data frames needed to plot the data.  In other words, 
  #   ---- subset the catches to just positives.  Toss the 0 catches.
  nvCatch <- nvCatch[ (nvCatch$Unmarked > 0) & nvCatch$FinalRun == run.name, ] 
  
  #   ---- Check if there is any non-valid catch.  
  if(nrow(nvCatch) > 0){
    nvCatch$Unassd <- nvCatch$lifeStage
    nvCatch2 <- F.expand.plus.counts( nvCatch )
    nvCatch2$includeCatchID <- 2
    nvCatch3 <- F.assign.batch.date( nvCatch2 )
    nvCatch.df <- nvCatch3[,names(catch.df)]
    
    #   ---- Check if we also have valid catch.  
    if(nrow(catch.df) > 0 & nrow(nvCatch.df) > 0){
      catch.df <- rbind(catch.df,nvCatch.df)
      attributes(catch.df) <- attributesSafe
      disc <- 'Plotted fork lengths include data from both successful and unsuccessful fishing.'
      
    #   ---- Check if we have non-valid catch alone.  
    } else if(nrow(catch.df) == 0 & nrow(nvCatch.df) > 0){
      catch.df <- nvCatch.df
      
      #   ---- In this case, there are no attributes to bring in, so do it now.  We need the 
      #   ---- row.names attribute to be something, or else catch.df goes back to zero data.
      attributesSafe$row.names <- rownames(nvCatch.df)   
      attributes(catch.df) <- attributesSafe
      disc <- 'Plotted fork lengths include data from only unsuccessful fishing.'    
    } 
    
  #   ---- Check if we only have valid catch data, and no non-valid data.  
  } else if(nrow(catch.df) > 0 & nrow(nvCatch) == 0){
    catch.df <- catch.df
    attributes(catch.df) <- attributesSafe
    disc <- 'Plotted fork lengths include data from only successful fishing.'        
  } 

  #   ---- Deal with the situation when no records ever found.
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

  #   ---- In the case when lifeStage is a factor, convert to character.  
  if(class(catch.df$lifeStage) == 'factor'){catch.df$lifeStage <- as.character(droplevels(catch.df$lifeStage))}   


  #   ---- Define some quantities useful for plotting.  
  x <- catch.df$EndTime
  y <- catch.df$forkLength
  lstage <- catch.df$lifeStage
  n <- catch.df$Unmarked
  valid <- catch.df$includeCatchID

  #   ---- Prevent records from plotting if any critical data are missing.  This means
  #   ---- limit the lifestages to fry, parr, and smolt.
  drop <- is.na(x) | is.na(y) | is.na(lstage) | is.na(n)
  if( (length(taxon) == 1) & (taxon == 161980) ){
    drop <- drop | !(lstage %in% c('Fry','Parr','Smolt'))  
  }  
  x <- x[!drop]
  y <- y[!drop]
  lstage <- lstage[!drop]
  n <- n[!drop]
  valid <- valid[!drop]
  
  #   ---- It could be the case that we get a small number of records that make it through
  #   ---- to the drop stage above, but end up getting thrown out, most likely due to the
  #   ---- lifeStage being Unassigned, and making it through the plus-count algorithm 
  #   ---- non-sliced and -diced.  This happens on occasion.  This means, there is now no
  #   ---- data to plot.  So, repeat the "no data to plot" code here for this contingency. 
  if( length(valid) == 0 ){
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
  x.bs <- bs( xx, df=6 )

  #   ---- Sometimes the quantile regression can be singular, due to problems 
  #   ---- in finding an inverse.  Capture this behavior.  
  rq.fit <- tryCatch(rq( y ~ x.bs, tau=c(0.05, 0.95) ), error = function(e) e)
  xpred <- seq(quantile(xx,.01),quantile(xx,.99),length=200)
  xp.bs <- bs( xpred, knots=attr(x.bs,"knots"), Boundary.knots=attr(x.bs,"Boundary.knots") )
  if(rq.fit[1] == 'Singular design matrix'){  
    
    #   ---- Put back to a POSIXct class for output.  
    class(xpred) <- class(x)
  } else {
    ypred <- cbind(1,xp.bs)%*%coef(rq.fit)
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
  dts <- attr(catch.df, "run.season")
  dts <- paste( format(dts$start, "%d%b%Y"), "to", format(dts$end, "%d%b%Y") )
  sp.string <- paste( sp.string, ", ", dts, sep="")
  title( main=sp.string, line=1, cex.main=1 )
  title( main=disc, line=0.25, cex.main=0.70)

  #   ---- Construct the legend.  
  if(rq.fit[1] == 'Singular design matrix'){  
    myleg <- as.character(CAMP.life.stage$lifeStageCAMP[ CAMP.life.stage$lifeStageCAMP %in% life.stages ])
    legend( "topleft", legend=myleg, col=c(mycol,"black"), pch=c(mypch,NA), lty=c(rep(NA,length(mycol))), lwd=c(rep(NA,length(mycol))) )  
  } else {
    myleg <- as.character(CAMP.life.stage$lifeStageCAMP[ CAMP.life.stage$lifeStageCAMP %in% life.stages ])
    myleg <- c(myleg, "90% bounds")
    legend( "topleft", legend=myleg, col=c(mycol,"black"), pch=c(mypch,NA), lty=c(rep(NA,length(mycol)), 2), lwd=c(rep(NA,length(mycol)), 3) )
  }

  #   ---- Close the graphics file.
  dev.off()
  search()
  
  #   ---- Write out the csv housing the data used to make the graph.  
  out.pts <- paste(output.file, "_size_by_date_points.csv", sep="")
  write.table( ans.pts, file=out.pts, sep=",", row.names=FALSE)
  if(rq.fit[1] != 'Singular design matrix'){  
    out.qr <- paste(output.file, "_size_by_date_quantlines.csv", sep="")
    write.table( ans.qr, file=out.qr, sep=",", row.names=FALSE)
  }

  #   ---- Send messages back to the interface
  cat("SUCCESS - F.size.by.date\n\n")
  cat(paste("Working directory:", getwd(), "\n"))
  cat(paste("R data frames saved in file:", "<no RData saved>", "\n\n"))
  cat("Number of files created in working directory = 3\n")
  cat(paste(out.graphs, "\n"))
  cat(paste(out.pts, "\n"))
  if(rq.fit[1] != 'Singular design matrix'){  
    cat(paste(out.qr, "\n"))
  }
  cat("\n")
  
  invisible(catch.df)

}