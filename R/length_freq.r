#' @export
#' 
#' @title F.length.frequency - Histogram of fork lengths
#'   
#' @description Plot frequency distribution of fork lengths over life stages
#' or by life stage
#' 
#' @inheritParams F.size.by.date
#' 
#' @param  by.lifestage  When set to \code{TRUE}, distributions are reported for 
#'   individual life stages.  Otherwise, they are collapsed and reported for all
#'   fish.
#'   
#' @inheritSection F.size.by.date Details
#' 
#'   
#' @return Either a single histogram, if variable \code{by.lifestage} is set to
#'   \code{FALSE}. Otherwise, a histogram for each individual life stage present
#'   within the data.
#'   
#' @inheritSection F.size.by.date Author
#'   
#' @seealso \code{F.get.indiv.fish.data} 
#'   
#' @examples
#' \dontrun{
#' #   ---- Obtain graphical histograms for the American. 
#' site <- 57000
#' taxon <- 161980
#' run <- 3
#' min.date <- "2014-01-01"
#' max.date <- "2014-06-06"
#' output.file <- "American"
#' by.lifestage <- TRUE
#' 
#' F.length.frequency(site,taxon,run,min.date,max.date,output.file,by.lifestage)
#' 
#' # Testing on Trent's local machine
#' db.file <- file.path("C:/Users/trent/Documents/Projects/1200-RSTPlatform/ThePlatform",
#'          "CAMP_RST20220103-campR2.0.14/Data/TestingDBs/newStanislausCAMP_20Sept2018/CAMP.mdb")
#' output.file <- file.path("C:/Users/trent/Documents/Projects/1200-RSTPlatform/ThePlatform",
#'          "CAMP_RST20220103-campR2.0.14/Outputs/length.frequency_ST004X_2022-03-21_14-33-45")
#' F.length.frequency(site,taxon,run,min.date,max.date,output.file,FALSE)
#' output.file <- paste0(output.file, "_lstage")
#' F.length.frequency(site,taxon,run,min.date,max.date,output.file,TRUE)
#'          
#' }
F.length.frequency <- function( site, taxon, run, min.date, max.date, output.file, by.lifestage ){

  #   ---- Check that taxon is Chinook salmon.  
  if( taxon != 161980 ) stop("Cannot specify any species other than Chinook salmon, code 161980.")

  #   ---- Retrieve catch data with fork length attached. 
  catch.df <- getCatchForkLenth( site, taxon, run, min.date, max.date )
  
  #   ---- Open a graphics device.
  if( !is.na(output.file) ){
    
    #   ---- Open PNG device.
    out.graphs <- paste(output.file, ".png", sep="")
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
  y <- catch.df$forkLength
  n <- catch.df$Unmarked  
  if( by.lifestage ){
    lstage <- catch.df$lifeStage
  } else {
    lstage <- rep(0, length(y))
  }
  
  #   ---- Repeat the values for number of fish of that particular length.
  y <- rep(y, n)
  lstage <- rep(lstage, n)

  #   ---- An internal function to compute common break points.
  f.breaks <- function(x, near=10, width=2){

    #   ---- Rounds down to nearest 'near' number.
    lolim <- trunc( min(x)/near ) * near  
    
    #   ---- Rounds up to nearest 'near' number.
    hilim <- ceiling( max(x)/near ) * near
    bks <- seq(lolim, hilim, by=width)
    bks
  }

  #   ---- An internal function to compute common y-axes.
  f.max.bar.hgt <- function(x, bks){
    h <- hist(x, breaks=bks, plot=F )
    max(h$counts)
  }

  #   ---- An internal function to draw one length frequency plot.
  f.len.freq <- function(x, bks, col, last=F, max.y, stage){
  
    #   ---- Plot the bars.
    if(last) {
      xa <- "s" 
      xl <- "Forklength (mm)"
    } else {
      xa <- "n"
      xl <- ""
    }
    
    #   ---- Get counts so can set ylim correctly.
    h <- hist(x, breaks=bks, plot=F )  
    y.at <- pretty(h$counts)

    h <- hist(x, breaks=bks, freq=T, xlab=xl, ylab="", main="", ylim=range(y.at),
        density=-1, col=col, cex.lab=2, cex.axis=1.25, yaxt="n", xlim=range(bks), xaxt="n" )
    
    #   ---- When the last lifestage, plot x-axis.
    if(last){     
      if((length(bks) %% 2) == 1){
        
        #   ---- Odd ticks.
        bksL <- bks[c(TRUE,FALSE)]  
      } else {
        
        #   ---- Even ticks.
        bksL <- c(bks[c(TRUE,FALSE)],bks[length(bks)])  
      }
      axis( 1, at=bksL, labels=formatC(bksL, big.mark=","),cex.axis=0.85)
    }
     
    #   ---- Add axis information.  
    axis( 2, at=y.at, labels=formatC(y.at, big.mark=",") )


    #   ---- Add a legend.
    n.str <- paste( "n (un-inflated)=", formatC(sum(h$counts), big.mark=",") )
    top <- legend( "topright", legend=c(stage,n.str), plot=F, cex=2  )
    text( max(bks), top$text$y[1], stage, cex=2, col=col, adj=1 )
    text( max(bks), top$text$y[2], n.str, cex=1, col="black", adj=1 )
    
    h
  }

  #   ---- Set the main titles.
  disc <- attr(catch.df, "disc")
  main.l1 <- attr(catch.df, "site.name")
  main.l2 <- attr(catch.df, "species.name") 
  if( !is.na(attr(catch.df, "runID")) ){
    main.l2 <- paste( main.l2, ", ", attr(catch.df, "run.name"), " run", sep="")
  }
  dts <- attr(catch.df, "run.season")
  dts <- paste( format(dts$start, "%d%b%Y"), "to", format(dts$end, "%d%b%Y") )
  main.l2 <- paste( main.l2, ", ", dts, sep="")

  #   ---- Plot by lifestage or not.
  if( by.lifestage ){

    #   ---- Plot by lifestage.
    life.stages <- sort(unique( lstage ))
    if( length(life.stages) == 3 ){
      mycol <- c("red", "orange", "blue")
    } else {
      mycol <- rainbow( length(life.stages) )
    }

    nl <- length(life.stages)
    layout.mat <- rbind( c(nl+2,nl+2),cbind( nl+1, 1:nl ))
    layout.widths <- c(.075,.925)
    layout.heights<- c(nl*.1, rep(1,nl-1), 1+nl*.1)
    layout( layout.mat, widths=layout.widths, heights=layout.heights )

    #   ---- Get common breaks.
    bks <- f.breaks( y, 10, 2 )

    #   ---- Get max count over all lifestages in any one bin.
    max.y <- 0
    for( i in 1:nl ){
        ind <- life.stages[i] == lstage
        yy <- y[ind]
        max.y <- max( max.y, f.max.bar.hgt(yy, bks))
    }
    
    #   ---- Plot histograms.
    myleg <- attr(catch.df, "legendEntries")
    for( i in 1:nl ){
      ind <- life.stages[i] == lstage
      yy <- y[ind]
      stage.name <- life.stages[i] 
      if( i == nl ){
        
        #   ---- This is the bottom panel.  Make room for x-axis ticks and label.
        par(mar=c(5.1,2.1,.5,2.1))
      } else {
        par(mar=c(0,2.1,.5,2.1))
      }
        
      cnts <- f.len.freq(yy, bks, mycol[ i ], i == nl, max.y, stage.name)

      if( i == 1 ){
        ans <- data.frame( bin.mid.mm=cnts$mids, cnt=cnts$counts )
      } else {
        ans <- cbind( ans, cnt = cnts$counts )
      }
      names(ans)[ names(ans) == "cnt" ] <- paste0(casefold(stage.name), ".frequency")
    }
    
    #   ---- Plot outer y-axis label.
    par(mar=c(0,0,0,0))
    plot(c(0,1), c(0,1), type="n", axes=F )
    text( .5, .5, "Frequency", adj=.5, srt=90, cex=2 )

    #   ---- Plot outer title. 
    str.hgt <- strheight(main.l1, units="user", cex=2)  * 0.8 / .1
    plot(c(0,1), c(0,1), type="n", axes=F )

    text( .5, 1 - str.hgt    ,  main.l1, adj=.5, cex=1.5 )
    text( .5, 1 - 2.4*str.hgt,  main.l2, adj=.5, cex=1.1 )
    text( .5, 1 - 3.3*str.hgt,     disc, adj=.5, cex=0.75)

  } else {

    #   ---- Plot only one histogram, much easier.
    bks <- f.breaks( y, 10, 2 )
    cnts <- f.len.freq(y, bks, "orange", last=TRUE, max.y=f.max.bar.hgt(y, bks), stage="")

    #   ---- Main title.
    title( main=main.l1, line=3, cex.main=1.5)
    title( main=main.l2, line=2, cex.main=1 )
    title( main=disc   , line=1, cex.main=0.5)
    title( main="All life stages", line=0, cex.main=.85)
    
    #   ---- Add y-axis label.
    title( ylab = "Frequency", cex.lab=2, line=2.5 )
    
    #   ---- Fix up the output.
    ans <- data.frame( bin.mid.mm=cnts$mids, frequency=cnts$counts )
  }

  #   ---- Close the graphics file.
  dev.off()

  #   ---- Write the CSV file.
  out.csv <- paste(output.file, ".csv", sep="")
  write.table( ans, file=out.csv, sep=",", row.names=F )

  tableDeleter()
  
  #   ---- Send messages back to the interface.
  cat("SUCCESS - F.length.frequency\n\n")
  cat(paste("Working directory:", getwd(), "\n"))
  cat(paste("R data frames saved in file:", "<no RData saved>", "\n\n"))
  cat("Number of files created in working directory = 2\n")
  cat(paste(out.graphs, "\n"))
  cat(paste(out.csv, "\n"))
  cat("\n")

  invisible(catch.df)

}