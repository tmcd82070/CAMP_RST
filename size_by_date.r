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



library(quantreg)
library(splines)

#   Open a graphics device
if( !is.na(output.file) ){
    #   ---- Open PNG device
    out.graphs <- paste(output.file, "_size_by_date.png", sep="")
    if(file.exists(out.graphs)){
        file.remove(out.graphs)
    }
    tryCatch({png(file=out.graphs,width=7,height=7,units="in",res=600)}, error=function(x){png(file=out.graphs)})
}

#   *******
#   Open ODBC channel and retrieve lifestage labels
ch <- odbcConnectAccess(db.file)

CAMP.life.stage <- sqlFetch(ch, table.names["CAMP.life.stages"])
rst.life.stage <- sqlFetch(ch, table.names["life.stages"])

close(ch)


#   ********
#   Retrieve basic data set, one line per fish or group of fish of same length.

catch.df   <- F.get.indiv.fish.data( site, taxon, run, min.date, max.date, keep="unmarked" )

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

#   Main plot

plot( x, y, type="n", xlab="", ylab="", xaxt="n" )
title( xlab="Date", cex.lab=1.5)
title( ylab="Fork Length (mm)", cex.lab=1.5, line=2.5 )
dts <- pretty(x)
axis( side=1, at=dts, labels=format(dts, "%b%y") )

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
    points( xx + jitx, yy + jity, col=mycol[ which(l.s == life.stages)], pch=mypch[ which(l.s == life.stages)] )
    ans.pts <- rbind( ans.pts, data.frame(lifestage=l.s, date=xx, fork.length.mm=yy, date.jittered=xx+jitx, fork.length.mm.jittered=yy+jity ))
}

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
