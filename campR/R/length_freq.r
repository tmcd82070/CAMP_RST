#' @export F.length.frequency
#' 
#' @title F.length.frequency
#' 
#' @description
#' 
#'    Plot frequency distribution of lengths.
#' 
#'    Input:
#'    db = full path and name of the Access data base to retrieve data from
#'    site = site ID of the place we want, trap locaton 
#'    taxon = taxon number (from luTaxon) to retrieve
#'    run = run ID of fish we want to do estimates for. 
#'    by.lifestage = if TRUE, produce histograms by lifestage, otherwise, lump all fish. 
#' 
#'    Output:
#'    A graph, in "file". 
#' 
#' 
#' 
#'   Open a graphics device
#' 
#' @param  site <describe argument>
#' @param  taxon <describe argument>
#' @param  run <describe argument>
#' @param  min.date <describe argument>
#' @param  max.date <describe argument>
#' @param  output.file <describe argument>
#' @param  by.lifestage  <describe argument>
#' 
#' @details <other comments found in file>
#'    Open ODBC channel and retrieve lifestage labels
#'    ********
#'    Retrieve basic data set, one line per fish or group of fish of same length.
#'  jason 3/25/2016 -- drop oldTrapPositionID here.  code assumes that var not here
#'  if catch.df has no data, it doesn't get batchDates added.  
#' catch.df <-  data.frame(catch.df,batchDate=integer(0))
#'   grab non-valid Catch
#'    Fetch run name
#'  no attributes to bring in -- do it now
#'    ********
#'    Now plot
#'    Define plotting variables
#'      lstage <- catch.df$lifeStageID
#'    Drop obs if any critical data is missing
#'    If we are talking salmon here, limit the lifestages to fry, parr, and smolt
#'      drop <- drop | (lstage > 8)
#'    --------------------- Convert from lifestages the traps used to life stages that CAMP uses.  The conversion is in table rst.life.stages
#'  JASON OBSOLETE -- QUERY THAT GETS CATCH WORKS ON DESCRIPTORS INSTEAD OF IDS -- 1/26/2015
#'  if( by.lifestage ){
#'      u.l.s <- sort(unique(lstage))
#'      for( l.s in u.l.s ){
#'          camp.l.s <- rst.life.stage$lifeStageCAMPID[ rst.life.stage$lifeStageID == l.s ]
#'          lstage[ lstage == l.s ] <- camp.l.s
#'      }
#'  }
#'    -------------------- Rep the values for number of fish of that particular length
#'    -------------------- An internal function to compute common break points 
#'  x <- y
#'  near <- 10
#'  width <- 2
#'    -------------------- An internal function to compute common y axes
#'    -------------------- An internal function to draw one length frequency plot
#'  x <- yy
#'  bks <- bks
#'  col <- mycol[ i ]
#'  last <- TRUE
#'  max.y <- max.y
#'  stage <- stage.name
#'    Plot the bars
#'    Uncomment the following line to plot everything on same y axis
#' h <- hist(x, breaks=bks, freq=T, xlab=xl, ylab="", main="", ylim=c(0,max.y),
#'     density=-1, col=col, xaxt=xa, cex.lab=2, cex.axis=1.25 )
#'      h <- hist(x, breaks=bks, freq=T, xlab=xl, ylab="", main="", ylim=range(y.at),
#'                density=-1, col=col, xaxt=xa, cex.lab=2, cex.axis=1.25, yaxt="n" )
#'  ----- jason update 12/16/2015 -------------------------------------------------------------------------------------------
#'  ----- jason update 12/16/2015 -------------------------------------------------------------------------------------------
#'    Smoothed density - If you want it
#' require(MASS)
#' pretty.bks <- pretty(bks)
#' axis(side=1, at=pretty.bks )
#' sm <- density( x, adjust=1.5, bw="SJ-dpi" )
#' sm$y <- sm$y * (h$breaks[2] - h$breaks[1]) * sm$n
#' lines( sm, col="black", lwd=2 )
#'    Legend
#'    ---------------------- Set main titles
#'    ---------------------- Plot by lifestage or not
#'    Plot by lifestage
#' layout.show(nl+2)
#'    Get common breaks
#'    Get max count overall lifestages in any one bin
#'    Plot histograms 
#'  This is the bottom panel, make room for x-axis ticks and label
#'    Plot outer y axis label
#'    Plot outer title 
#'    ---- Plot only one histogram, much easier
#'    Main title
#'    Y label
#'    Fix up the output
#'    ---- Close the graphics file
#'    ---- Write the CSV file 
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
F.length.frequency <- function( site, taxon, run, min.date, max.date, output.file, by.lifestage ){
#
#   Plot frequency distribution of lengths.
#
#   Input:
#   db = full path and name of the Access data base to retrieve data from
#   site = site ID of the place we want, trap locaton 
#   taxon = taxon number (from luTaxon) to retrieve
#   run = run ID of fish we want to do estimates for. 
#   by.lifestage = if TRUE, produce histograms by lifestage, otherwise, lump all fish. 
#
#   Output:
#   A graph, in "file". 
#


#  Open a graphics device
if( !is.na(output.file) ){
    #   ---- Open PNG device
    out.graphs <- paste(output.file, "_len_freq.png", sep="")
    if(file.exists(out.graphs)){
        file.remove(out.graphs)
    }
    tryCatch({png(file=out.graphs,width=7,height=7,units="in",res=600)}, error=function(x){png(file=out.graphs)})
}

if( by.lifestage ){
    #   *******
    #   Open ODBC channel and retrieve lifestage labels
    ch <- odbcConnectAccess(db.file)
    
    CAMP.life.stage <- sqlFetch(ch, table.names["CAMP.life.stages"])
    rst.life.stage <- sqlFetch(ch, table.names["life.stages"])
    
    close(ch)
}

#   ********
#   Retrieve basic data set, one line per fish or group of fish of same length.

catch.df   <- F.get.indiv.fish.data( site, taxon, run, min.date, max.date, keep="unmarked" )

# jason 3/25/2016 -- drop oldTrapPositionID here.  code assumes that var not here
catch.df$includeCatchID <-
  catch.df$preUnmarked <-
  catch.df$halfConeAssignedCatch <-
  catch.df$oldtrapPositionID <-
  catch.df$halfConeUnassignedCatch <-
  catch.df$assignedCatch <-
  catch.df$unassignedCatch <-
  catch.df$modUnassignedCatch <- catch.df$modAssignedCatch <- NULL

# if catch.df has no data, it doesn't get batchDates added.  
if(nrow(catch.df) == 0){
  names(catch.df)[names(catch.df) == 'SampleDate'] <- 'batchDate'
  #catch.df <-  data.frame(catch.df,batchDate=integer(0))
}

#  grab non-valid Catch
attributesSafe <- attributes(catch.df)
db <- get( "db.file", env=.GlobalEnv ) 
ch <- odbcConnectAccess(db)

F.run.sqlFile( ch, "QryNonValidFishing.sql", R.TAXON=taxon )   
nvCatch <- sqlFetch( ch, "TempSumUnmarkedByTrap_Run_X_final" )        #   Now, fetch the result -- nvCatch = non-Valid Catch
F.sql.error.check(nvCatch)

#   Fetch run name
tables <- get( "table.names", env=.GlobalEnv )
runs <- sqlQuery(ch, paste( "SELECT run, runID FROM", tables["run.codes"] ))
F.sql.error.check(runs)
run.name <- as.character(runs$run[ runs$runID == run ])

close(ch)

nvCatch <- nvCatch[ (nvCatch$Unmarked > 0) & nvCatch$FinalRun == run.name, ]   #  Subset the catches to just positives.  Toss the 0 catches.
if(nrow(nvCatch) > 0){
  nvCatch$Unassd <- nvCatch$lifeStage                                   #  jason add to ID the unassigned lifeStage -- necessary to separate measured vs caught.
  nvCatch2 <- F.expand.plus.counts( nvCatch )                           #  Expand the Plus counts
  nvCatch2$includeCatchID <- 2                                          #  make this df match the catch.df
  nvCatch3 <- F.assign.batch.date( nvCatch2 )                           #  clean up dates
  nvCatch.df <- nvCatch3[,names(catch.df)]                              #  get both dfs lined up correctly
  if(nrow(catch.df) > 0 & nrow(nvCatch.df) > 0){
    catch.df <- rbind(catch.df,nvCatch.df)                                #  use a new catch.df with non-valid fishing included
    attributes(catch.df) <- attributesSafe
    disc <- 'Plotted fork lengths include data from both successful and unsuccessful fishing.'
  } else if(nrow(catch.df) == 0 & nrow(nvCatch.df) > 0){
    catch.df <- nvCatch.df
    # no attributes to bring in -- do it now
    attributesSafe$row.names <- rownames(nvCatch.df)    # need row.names attr to be something, or else catch.df goes back to zero data
    attributes(catch.df) <- attributesSafe
    disc <- 'Plotted fork lengths include data from only unsuccessful fishing.'    
  } 
} else if(nrow(catch.df) > 0 & nrow(nvCatch) == 0){
  catch.df <- catch.df
  attributes(catch.df) <- attributesSafe
  disc <- 'Plotted fork lengths include data from only successful fishing.'     
} # nrow(catch.df) == 0 condition below will catch situation when no records ever found.

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

if(class(catch.df$lifeStage) == 'factor'){catch.df$lifeStage <- as.character(droplevels(catch.df$lifeStage))}   # jason add



#   ********
#   Now plot

#   Define plotting variables
y <- catch.df$forkLength
n <- catch.df$Unmarked  #catch.df$n
if( by.lifestage ){
#     lstage <- catch.df$lifeStageID
    lstage <- catch.df$lifeStage
} else {
    lstage <- rep(0, length(y))
}

#   Drop obs if any critical data is missing
#   If we are talking salmon here, limit the lifestages to fry, parr, and smolt
drop <-  is.na(y) | is.na(lstage) | is.na(n)
if( (length(taxon) == 1) & (taxon == 161980) & by.lifestage == TRUE ){   # jason add the by.lifetage condition.  only evaluate if lstage setup to vary
#     drop <- drop | (lstage > 8)
  drop <- drop | !(lstage %in% c('Fry','Parr','Smolt'))  
}
y <- y[!drop]
lstage <- lstage[!drop]
n <- n[!drop]

#   --------------------- Convert from lifestages the traps used to life stages that CAMP uses.  The conversion is in table rst.life.stages
# JASON OBSOLETE -- QUERY THAT GETS CATCH WORKS ON DESCRIPTORS INSTEAD OF IDS -- 1/26/2015
# if( by.lifestage ){
#     u.l.s <- sort(unique(lstage))
#     for( l.s in u.l.s ){
#         camp.l.s <- rst.life.stage$lifeStageCAMPID[ rst.life.stage$lifeStageID == l.s ]
#         lstage[ lstage == l.s ] <- camp.l.s
#     }
# }

#   -------------------- Rep the values for number of fish of that particular length
y <- rep(y, n)
lstage <- rep(lstage, n)

#   -------------------- An internal function to compute common break points 
f.breaks<-function(x, near=10, width=2){

  # x <- y
  # near <- 10
  # width <- 2
    lolim <- trunc( min(x)/near ) * near   # rounds down to nearest 'near' number, eg., near = 5, rounds down to nearest multiple of 5
    hilim <- ceiling( max(x)/near ) * near # rounds up to nearest 'near' number.
    bks <- seq(lolim, hilim, by=width)
    bks
}

#   -------------------- An internal function to compute common y axes
f.max.bar.hgt <- function(x, bks){
    h <- hist(x, breaks=bks, plot=F )
    max(h$counts)
}


#   -------------------- An internal function to draw one length frequency plot

f.len.freq<-function(x, bks, col, last=F, max.y, stage){
  
  # x <- yy
  # bks <- bks
  # col <- mycol[ i ]
  # last <- TRUE
  # max.y <- max.y
  # stage <- stage.name

    #   Plot the bars
    if(last) {
        xa <- "s" 
        xl <- "Forklength (mm)"
    } else {
        xa <- "n"
        xl <- ""
    }
    
    #   Uncomment the following line to plot everything on same y axis
    #h <- hist(x, breaks=bks, freq=T, xlab=xl, ylab="", main="", ylim=c(0,max.y),
    #    density=-1, col=col, xaxt=xa, cex.lab=2, cex.axis=1.25 )

    h <- hist(x, breaks=bks, plot=F )  # get counts so can set ylim correctly
    y.at <- pretty(h$counts)

#     h <- hist(x, breaks=bks, freq=T, xlab=xl, ylab="", main="", ylim=range(y.at),
#               density=-1, col=col, xaxt=xa, cex.lab=2, cex.axis=1.25, yaxt="n" )
   
     
# ----- jason update 12/16/2015 -------------------------------------------------------------------------------------------
    h <- hist(x, breaks=bks, freq=T, xlab=xl, ylab="", main="", ylim=range(y.at),
        density=-1, col=col, cex.lab=2, cex.axis=1.25, yaxt="n", xlim=range(bks), xaxt="n" )
    
    if(last){     # last lifestage, so plot x-axis.
      if((length(bks) %% 2) == 1){
        bksL <- bks[c(TRUE,FALSE)]   # odd ticks
      } else {
        bksL <- c(bks[c(TRUE,FALSE)],bks[length(bks)])  # even ticks
      }
      axis( 1, at=bksL, labels=formatC(bksL, big.mark=","),cex.axis=0.85)
    }
# ----- jason update 12/16/2015 -------------------------------------------------------------------------------------------

     
    axis( 2, at=y.at, labels=formatC(y.at, big.mark=",") )

    #   Smoothed density - If you want it
    #require(MASS)
    #pretty.bks <- pretty(bks)
    #axis(side=1, at=pretty.bks )
    #sm <- density( x, adjust=1.5, bw="SJ-dpi" )
    #sm$y <- sm$y * (h$breaks[2] - h$breaks[1]) * sm$n
    #lines( sm, col="black", lwd=2 )

    #   Legend
    n.str <- paste( "n (un-inflated)=", formatC(sum(h$counts), big.mark=",") )
    top <- legend( "topright", legend=c(stage,n.str), plot=F, cex=2  )
    text( max(bks), top$text$y[1], stage, cex=2, col=col, adj=1 )
    text( max(bks), top$text$y[2], n.str, cex=1, col="black", adj=1 )
    
    h
}

#   ---------------------- Set main titles
main.l1 <- attr(catch.df, "site.name")
main.l2 <- attr(catch.df, "species.name") 
if( !is.na(attr(catch.df, "runID")) ){
    main.l2 <- paste( main.l2, ", ", attr(catch.df, "run.name"), " run", sep="")
}
dts <- attr(catch.df, "run.season")
dts <- paste( format(dts$start, "%d%b%Y"), "to", format(dts$end, "%d%b%Y") )
main.l2 <- paste( main.l2, ", ", dts, sep="")

#   ---------------------- Plot by lifestage or not
if( by.lifestage ){

    #   Plot by lifestage
    life.stages <- sort(unique( lstage ))
    if( length(life.stages) == 3 ){
        mycol <- c("red", "orange", "blue")
    } else {
        mycol <- rainbow( length(life.stages) )
    }

    nl <- length(life.stages)
    layout.mat <- rbind( c(nl+2,nl+2),
                         cbind( nl+1, 1:nl ))
    layout.widths <- c(.075,.925)
    layout.heights<- c(nl*.1, rep(1,nl-1), 1+nl*.1)
    layout( layout.mat, widths=layout.widths, heights=layout.heights )
    #layout.show(nl+2)

    #   Get common breaks
    bks <- f.breaks( y, 10, 2 )

    #   Get max count overall lifestages in any one bin
    max.y <- 0
    for( i in 1:nl ){
        ind <- life.stages[i] == lstage
        yy <- y[ind]
        max.y <- max( max.y, f.max.bar.hgt(yy, bks))
    }
    
    #   Plot histograms 
    for( i in 1:nl ){
        ind <- life.stages[i] == lstage
        yy <- y[ind]
        stage.name <- CAMP.life.stage$lifeStageCAMP[ CAMP.life.stage$lifeStageCAMP == life.stages[i] ]
        if( i == nl ){
            # This is the bottom panel, make room for x-axis ticks and label
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
    
    #   Plot outer y axis label
    par(mar=c(0,0,0,0))
    plot(c(0,1), c(0,1), type="n", axes=F )
    text( .5, .5, "Frequency", adj=.5, srt=90, cex=2 )

    #   Plot outer title 
    str.hgt <- strheight(main.l1, units="user", cex=2)  * 0.8 / .1
    plot(c(0,1), c(0,1), type="n", axes=F )

    text( .5, 1 - str.hgt    ,  main.l1, adj=.5, cex=1.5 )
    text( .5, 1 - 2.4*str.hgt,  main.l2, adj=.5, cex=1.1 )
    text( .5, 1 - 3.3*str.hgt,     disc, adj=.5, cex=0.75)

} else {

    
    #   ---- Plot only one histogram, much easier
    bks <- f.breaks( y, 10, 2 )
    cnts <- f.len.freq(y, bks, "orange", last=TRUE, max.y=f.max.bar.hgt(y, bks), stage="")

    #   Main title
    title( main=main.l1, line=3, cex.main=1.5)
    title( main=main.l2, line=2, cex.main=1 )
    title( main=disc   , line=1, cex.main=0.5)
    title( main="All life stages", line=0, cex.main=.85)
    
    #   Y label
    title( ylab = "Frequency", cex.lab=2, line=2.5 )
    
    #   Fix up the output
    ans <- data.frame( bin.mid.mm=cnts$mids, frequency=cnts$counts )
}


#   ---- Close the graphics file
dev.off()

#   ---- Write the CSV file 
out.csv <- paste(output.file, "_len_freq.csv", sep="")
write.table( ans, file=out.csv, sep=",", row.names=F )

#   ---- Send messages back to the interface
cat("SUCCESS - F.length.frequency\n\n")
cat(paste("Working directory:", getwd(), "\n"))
cat(paste("R data frames saved in file:", "<no RData saved>", "\n\n"))
cat("Number of files created in working directory = 2\n")
cat(paste(out.graphs, "\n"))
cat(paste(out.csv, "\n"))
cat("\n")

invisible(catch.df)

}