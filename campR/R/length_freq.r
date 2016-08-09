#' @export F.length.frequency
#'   
#' @title F.length.frequency
#'   
#' @description Plot frequency distribution of fork lengths.
#'   
#'   Input: db = full path and name of the Access data base to retrieve data
#'   from site = site ID of the place we want, trap locaton taxon = taxon number
#'   (from luTaxon) to retrieve run = run ID of fish we want to do estimates
#'   for. by.lifestage = if TRUE, produce histograms by lifestage, otherwise,
#'   lump all fish.
#'   
#'   Output: A graph, in "file".
 
#' @param site The identification number of the site for which estimates are 
#'   required.
#' @param taxon The species identifier indicating the type of fish of interest. 
#'   This is always \code{161980}; i.e., Chinook Salmon.
#' @param run The text seasonal identifier.  This is a one of \code{"Spring"}, 
#'   \code{"Fall"}, \code{"Late Fall"}, or \code{"Winter"}.
#' @param min.date The start date for data to include. This is a text string in 
#'   the format \code{\%Y-\%m-\%d}, or \code{YYYY-MM-DD}.
#' @param max.date The end date for data to include.  Same format as 
#'   \code{min.date}.
#' @param output.file The name of the file prefix under which output is to be 
#'   saved.  Set to NA to plot to the Plot window.
#' @param  by.lifestage  When set to \code{TRUE}, distributions are reported for 
#'   individual life stages.  Otherwise, they are collapsed and reported for all
#'   fish.
#'   
#' @details Function \code{F.length.freq} utilizes function \code{F.get.indiv.fish.data} 
#' to obtain individual fish fork length data.  
#'   
#' @return Either a single histogram, if variable \code{by.lifestage} is set to
#'   \code{FALSE}. Otherwise, a histogram for each individual life stage present
#'   within the data.
#'   
#' @author WEST Inc.
#'   
#' @seealso \code{\link{<related routine>}}, \code{\link{<related routine>}}
#'   
#' @examples
#' \dontrun{
#' <insert examples>
#' 
#' }
F.length.frequency <- function( site, taxon, run, min.date, max.date, output.file, by.lifestage ){

  # site <_ 57000
  # taxon <- 161980
  # run <- "Fall"
  # min.date <- "2014-01-01"
  # max.date <- "2014-06-06"
  # output.file <- "American"
  # by.lifestage <- TRUE

  #   ---- Get global environment stuff.
  db.file <- get("db.file",envir=.GlobalEnv)
  table.names <- get("table.names",envir=.GlobalEnv)

  #   ---- Open a graphics device.
  if( !is.na(output.file) ){
    
    #   ---- Open PNG device.
    out.graphs <- paste(output.file, "_len_freq.png", sep="")
    if(file.exists(out.graphs)){
      file.remove(out.graphs)
    }
    tryCatch({png(filename=out.graphs,width=7,height=7,units="in",res=600)}, error=function(x){png(filename=out.graphs)})
  }

  #   ---- If breaking out by life stage, get their labels.  
  if( by.lifestage ){
    ch <- odbcConnectAccess(db.file)
    CAMP.life.stage <- sqlFetch(ch, table.names["CAMP.life.stages"])
    rst.life.stage <- sqlFetch(ch, table.names["life.stages"])
    close(ch)
  }

  #   ---- Retrieve basic data set, one line per fish or group of fish of same length.
  catch.df   <- F.get.indiv.fish.data( site, taxon, run, min.date, max.date, keep="unmarked" )

  #   ---- Remove unnecessary variables.  
  catch.df$includeCatchID <-
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
    #catch.df <-  data.frame(catch.df,batchDate=integer(0))
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
  if(nrow(catch.df) == 0){
    plot( c(0,1), c(0,1), xaxt="n", yaxt="n", type="n", xlab="", ylab="")
    text( .5,.5, "All Zero's\nCheck dates\nCheck that finalRunID is assigned to >=1 fish per visit\nCheck sub-Site were operating between dates")
    dev.off(dev.cur())
    ans <- out.graphs
    cat("FAILURE - F.length.frequency\n\n")
    cat(paste("Working directory:", getwd(), "\n"))
    cat(paste("R data frames saved in file:", "<no RData saved>", "\n\n"))
    cat("Number of files created in working directory = 1\n")
    cat(paste(out.graphs, "\n"))
    cat("\n")    
    return(catch.df)
  }
  
  #   ---- In the case when lifeStage is a factor, convert to character.  
  if(class(catch.df$lifeStage) == 'factor'){catch.df$lifeStage <- as.character(droplevels(catch.df$lifeStage))}   # jason add

  #   ---- Define some quantities useful for plotting.  
  y <- catch.df$forkLength
  n <- catch.df$Unmarked  #catch.df$n
  if( by.lifestage ){
    lstage <- catch.df$lifeStage
  } else {
    lstage <- rep(0, length(y))
  }

  #   ---- Prevent records from plotting if any critical data are missing.  This means
  #   ---- limit the lifestages to fry, parr, and smolt.
  drop <-  is.na(y) | is.na(lstage) | is.na(n)
  if( (length(taxon) == 1) & (taxon == 161980) & by.lifestage == TRUE ){  
    drop <- drop | !(lstage %in% c('Fry','Parr','Smolt'))  
  }
  y <- y[!drop]
  lstage <- lstage[!drop]
  n <- n[!drop]

  #   ---- Repeat the values for number of fish of that particular length.
  y <- rep(y, n)
  lstage <- rep(lstage, n)

  #   ---- Formulate an internal function to compute common break points.
  f.breaks <- function(x, near=10, width=2){

    # x <- y
    # near <- 10
    # width <- 2
    
    #   ---- Rounds down to nearest 'near' number.
    lolim <- trunc( min(x)/near ) * near  
    
    #   ---- Rounds up to nearest 'near' number.
    hilim <- ceiling( max(x)/near ) * near
    bks <- seq(lolim, hilim, by=width)
    bks
  }

  #   ---- Formulate an internal function to compute common y-axes.
  f.max.bar.hgt <- function(x, bks){
    h <- hist(x, breaks=bks, plot=F )
    max(h$counts)
  }

  #   ---- Formulate an internal function to draw one length frequency plot.
  f.len.freq <- function(x, bks, col, last=F, max.y, stage){
  
    # x <- yy
    # bks <- bks
    # col <- mycol[ i ]
    # last <- TRUE
    # max.y <- max.y
    # stage <- stage.name

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
    for( i in 1:nl ){
      ind <- life.stages[i] == lstage
      yy <- y[ind]
      stage.name <- CAMP.life.stage$lifeStageCAMP[ CAMP.life.stage$lifeStageCAMP == life.stages[i] ]
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
  out.csv <- paste(output.file, "_len_freq.csv", sep="")
  write.table( ans, file=out.csv, sep=",", row.names=F )

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