#' @export F.size.by.date
#' 
#' @title F.size.by.date
#' 
#' @description
#' 
#'    Plot fork length of fish by date of catch ro a particular site and year. 
#' 
#'    Input:
#'    db = full path and name of the Access data base to retrieve data from
#'    site = site ID of the place we want, trap locaton 
#'    taxon = taxon number (from luTaxon) to retrieve
#'    run = run ID of fish we want to do estimates for. 
#' 
#'    Output:
#'    A graph, in "file" of the length by date data. 
#' 
#' 
#' 
#' @param  site <describe argument>
#' @param  taxon <describe argument>
#' @param  run <describe argument>
#' @param  min.date <describe argument>
#' @param  max.date <describe argument>
#' @param  output.file  <describe argument>
#' 
#' @details <other comments found in file>
#'    ---- Open PNG device
#'    *******
#'    Open ODBC channel and retrieve lifestage labels
#'    ********
#'    Retrieve basic data set, one line per fish or group of fish of same length.
#'  don't drop includeCatchID here.  this is a contrast with length_freq.r
#'  jason 3/25/2016 -- drop oldTrapPositionID here.  code assumes that var not here
#'  if catch.df has no data, it doesn't get batchDates added.  
#' catch.df <-  data.frame(catch.df,batchDate=integer(0))
#'   grab non-valid Catch
#'    Fetch run name
#'  no attributes to bring in -- do it now
#'    ********
#'    Now plot
#'    Define plotting variables
#'  x <- catch.df$visitTime
#'  n <- catch.df$n
#'  lstage <- catch.df$lifeStageID
#'    Drop record if any critical data is missing
#'    If we are talking salmon here, limit the lifestages to fry, parr, and smolt
#'      drop <- drop | (lstage > 8)
#'    Convert from lifestages the traps used to life stages that CAMP uses.  The conversion is in table rst.life.stages
#'  JASON OBSOLETE -- QUERY THAT GETS CATCH WORKS ON DESCRIPTORS INSTEAD OF IDS -- 1/26/2015
#'  u.l.s <- sort(unique(lstage))
#'  for( l.s in u.l.s ){
#'      camp.l.s <- rst.life.stage$lifeStageCAMPID[ rst.life.stage$lifeStageID == l.s ]
#'      lstage[ lstage == l.s ] <- camp.l.s
#'  }
#'    Rep the values for number of fish of that particular length
#'    Main plot
#' jity <- rnorm(sum(ind), sd=diff(range(y))/100)
#' 95, 108, 122, 151, 152, 155
#'    Add quantile lines
#'  jason add.  this can sometimes be singular, apparently.  leads to problems in finding an inverse. 
#'      ans.qr <- data.frame( date=xpred, q.05=ypred[,1], q.95=ypred[,2] )
#'    Main title
#'    Legend
#' print(myleg)
#' print(myleg)
#'    ---- Close the graphics file
#'    ---- Write out the csv's
#'    ---- Send messages back to the interface
#' 
#' @return <describe return value>
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
F.size.by.date <- function( site, taxon, run, min.date, max.date, output.file ){
#
#   Plot fork length of fish by date of catch ro a particular site and year. 
#
#   Input:
#   db = full path and name of the Access data base to retrieve data from
#   site = site ID of the place we want, trap locaton 
#   taxon = taxon number (from luTaxon) to retrieve
#   run = run ID of fish we want to do estimates for. 
#
#   Output:
#   A graph, in "file" of the length by date data. 
#

  #   ---- Get global environment stuff.
  db.file <- get("db.file",envir=.GlobalEnv)
  table.names <- get("table.names",envir=.GlobalEnv)

#   Open a graphics device
if( !is.na(output.file) ){
    #   ---- Open PNG device
    out.graphs <- paste(output.file, "_size_by_date.png", sep="")
    if(file.exists(out.graphs)){
        file.remove(out.graphs)
    }
tryCatch({png(filename=out.graphs,width=7,height=7,units="in",res=600)}, error=function(x){png(filename=out.graphs)})
}

#   *******
#   Open ODBC channel and retrieve lifestage labels
ch <- odbcConnectAccess(db.file)

CAMP.life.stage <- sqlFetch(ch, table.names["CAMP.life.stages"])
rst.life.stage <- sqlFetch(ch, table.names["life.stages"])

close(ch)


#   ********
#   Retrieve basic data set, one line per fish or group of fish of same length.

catch.df  <- F.get.indiv.fish.data( site, taxon, run, min.date, max.date, keep="unmarked" )

# don't drop includeCatchID here.  this is a contrast with length_freq.r
# jason 3/25/2016 -- drop oldTrapPositionID here.  code assumes that var not here
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
db <- get( "db.file", envir=.GlobalEnv ) 
ch <- odbcConnectAccess(db)

F.run.sqlFile( ch, "QryNonValidFishing.sql", R.TAXON=taxon )   
nvCatch <- sqlFetch( ch, "TempSumUnmarkedByTrap_Run_X_final" )        #   Now, fetch the result -- nvCatch = non-Valid Catch
F.sql.error.check(nvCatch)

#   Fetch run name
tables <- get( "table.names", envir=.GlobalEnv )
runs <- sqlQuery(ch, paste( "SELECT run, runID FROM", tables["run.codes"] ))
F.sql.error.check(runs)
run.name <- as.character(runs$run[ runs$runID == run ])

close(ch)


nvCatch <- nvCatch[ (nvCatch$Unmarked > 0) & nvCatch$FinalRun == run.name, ]        #  Subset the catches to just positives.  Toss the 0 catches.
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

if(class(catch.df$lifeStage) == 'factor'){catch.df$lifeStage <- as.character(droplevels(catch.df$lifeStage))}   # jason add

#   ********
#   Now plot

#   Define plotting variables
# x <- catch.df$visitTime
# n <- catch.df$n
# lstage <- catch.df$lifeStageID
x <- catch.df$EndTime
y <- catch.df$forkLength
lstage <- catch.df$lifeStage
n <- catch.df$Unmarked
valid <- catch.df$includeCatchID

#   Drop record if any critical data is missing
#   If we are talking salmon here, limit the lifestages to fry, parr, and smolt
drop <- is.na(x) | is.na(y) | is.na(lstage) | is.na(n)
if( (length(taxon) == 1) & (taxon == 161980) ){
  #     drop <- drop | (lstage > 8)
  drop <- drop | !(lstage %in% c('Fry','Parr','Smolt'))  
}
x <- x[!drop]
y <- y[!drop]
lstage <- lstage[!drop]
n <- n[!drop]
valid <- valid[!drop]

#   Convert from lifestages the traps used to life stages that CAMP uses.  The conversion is in table rst.life.stages
# JASON OBSOLETE -- QUERY THAT GETS CATCH WORKS ON DESCRIPTORS INSTEAD OF IDS -- 1/26/2015
# u.l.s <- sort(unique(lstage))
# for( l.s in u.l.s ){
#     camp.l.s <- rst.life.stage$lifeStageCAMPID[ rst.life.stage$lifeStageID == l.s ]
#     lstage[ lstage == l.s ] <- camp.l.s
# }

#   Rep the values for number of fish of that particular length
x <- rep(x, n)
y <- rep(y, n)
lstage <- rep(lstage, n)
valid <- rep(valid, n)


#   Main plot

plot( x, y, type="n", xlab="", ylab="", xaxt="n" )
title( xlab="Date", cex.lab=1.5)
title( ylab="Fork Length (mm)", cex.lab=1.5, line=2.5 )
dts <- pretty(x)
axis( side=1, at=dts, labels=format(dts, "%d%b%y") )

life.stages <- sort(unique( lstage ))
cat(paste("Lifestages plotted:", paste(life.stages, collapse=", "), "\n"))
if( length(life.stages) == 3 ){
    mycol <- c("red", "orange", "blue")
} else {
    mycol <- rainbow( length(life.stages) )
}
mypch <- rev(1:(0+length(life.stages)))

ans.pts <- NULL
for( l.s in life.stages ){
    ind <- l.s == lstage
    xx <- x[ind]
    yy <- y[ind]
    jitx <- rnorm(sum(ind), sd=60*60*3)
    #jity <- rnorm(sum(ind), sd=diff(range(y))/100)
    jity <- 0
    validv <- ifelse(valid[ind]==1,'Fishing was successful.','Fishing was not successful.')
    points( xx + jitx, yy + jity, col=mycol[ which(l.s == life.stages)], pch=mypch[ which(l.s == life.stages)] )
    ans.pts <- rbind( ans.pts, data.frame(lifestage=l.s, date=xx, fork.length.mm=yy, date.jittered=xx+jitx, fork.length.mm.jittered=yy+jity, fishing.status=validv ))
}

ans.pts <- ans.pts[order(ans.pts$date, ans.pts$lifestage, ans.pts$date.jittered, ans.pts$fork.length.mm.jittered),]

#95, 108, 122, 151, 152, 155

#   Add quantile lines
xx <- as.numeric(x) # no longer a POSIXct
x.bs <- bs( xx, df=6 )

# jason add.  this can sometimes be singular, apparently.  leads to problems in finding an inverse. 
rq.fit <- tryCatch(rq( y ~ x.bs, tau=c(0.05, 0.95) ), error = function(e) e)

  xpred <- seq(quantile(xx,.01),quantile(xx,.99),length=200)
  xp.bs <- bs( xpred, knots=attr(x.bs,"knots"), Boundary.knots=attr(x.bs,"Boundary.knots") )
  if(rq.fit[1] == 'Singular design matrix'){  
    class(xpred) <- class(x)  # back to a POSIXct for output
#     ans.qr <- data.frame( date=xpred, q.05=ypred[,1], q.95=ypred[,2] )
  } else {
    ypred <- cbind(1,xp.bs)%*%coef(rq.fit)
    lines( xpred, ypred[,1], col="black", lwd=3, lty=2 )
    lines( xpred, ypred[,2], col="black", lwd=3, lty=2 )
    class(xpred) <- class(x)  # back to a POSIXct for output
    ans.qr <- data.frame( date=xpred, q.05=ypred[,1], q.95=ypred[,2] )
  }
  
#   Main title
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

#   Legend
if(rq.fit[1] == 'Singular design matrix'){  
  myleg <- as.character(CAMP.life.stage$lifeStageCAMP[ CAMP.life.stage$lifeStageCAMP %in% life.stages ])
  #print(myleg)
  legend( "topleft", legend=myleg, col=c(mycol,"black"), pch=c(mypch,NA), lty=c(rep(NA,length(mycol))), lwd=c(rep(NA,length(mycol))) )  
} else {
  myleg <- as.character(CAMP.life.stage$lifeStageCAMP[ CAMP.life.stage$lifeStageCAMP %in% life.stages ])
  #print(myleg)
  myleg <- c(myleg, "90% bounds")
  legend( "topleft", legend=myleg, col=c(mycol,"black"), pch=c(mypch,NA), lty=c(rep(NA,length(mycol)), 2), lwd=c(rep(NA,length(mycol)), 3) )
}

#   ---- Close the graphics file
dev.off()
search()
#   ---- Write out the csv's
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
