F.plot.eff.model <- function( fit, df, file ){
#
#   Plot trap efficiencies
#   values.
#
#   input:
#   fit = for now, a data frame containing date and efficiency values.
#   df = data frame input to F.eff.model
#   file = file to write plot to
#
#   Output:
#   A time series plot of catch by date.
#


if( !is.na(file) ){
    #   Shut down all graphics devices
    for(i in dev.list()) dev.off(i)

    #   ---- Open PNG device
    out.pass.graphs <- paste(file, "_eff.png", sep="")
    if(file.exists(out.pass.graphs)){
        file.remove(out.pass.graphs)
    }
    tryCatch({png(file=out.pass.graphs,width=7,height=7,units="in",res=600)}, error=function(x){png(file=out.pass.graphs)})
}


imputed <- df$imputed.eff == "Yes"
pts <- !is.na(df$nReleased)

pred.y <- rep(NA, nrow(df))
pred.y[!imputed] <- df$efficiency[!imputed]

#if(length(dev.list()) > 1) windows()


eff <- df$nCaught / df$nReleased

plot( range(df$batchDate,na.rm=T), range(eff,na.rm=T), type="n", xlab="Date", 
    ylab="Efficiency", xaxt="n" )
    
lab.x.at <- pretty(df$batchDate)
axis( side=1, at=lab.x.at, label=format(lab.x.at, "%d%b%y"))


#   Draw points and lines
traps <- sort(unique(df$trapPositionID))
my.colors <- rainbow(length(traps))
my.pch <- 15 + 1:length(traps)
for( i in 1:length(traps) ){

    #   indicators for season and such
    ind.pts <- df$trapPositionID == traps[i] & pts
    ind.line <- df$trapPositionID == traps[i] & imputed
    
    # The following plots period between trial
#    strt <- min(df$batchDate[ind.pts])
#    end <- max(df$batchDate[ind.pts])
    # The following plots all time
    strt <- min(df$batchDate)
    end <- max(df$batchDate)
    
    trials.season <- (strt <= df$batchDate[ind.line]) & (df$batchDate[ind.line] <= end)


    #   Draw lines
    lines( df$batchDate[ ind.line ][trials.season], df$efficiency[ ind.line ][trials.season], lwd=3, col=my.colors[i] )

    #   Draw points
    eff <- df$nCaught[ind.pts] / df$nReleased[ ind.pts ] 
    points( df$batchDate[ ind.pts ], eff, pch=my.pch[i], col="black" )


}

#   Draw legend
for( i in 1:length(traps)){
    if( i == 1 ){
        tmp <- legend( "topleft", title=paste("Trap", traps[i]), legend=c("Observed","Predicted"), pch=c(my.pch[i],NA),
                    col=c("black", my.colors[i]), lty=c(NA, 1), cex=.85)
    } else {
        tmp <- legend( tmp$rect$left, tmp$rect$top - tmp$rect$h, title=paste("Trap", traps[i]), legend=c("Observed","Predicted"), pch=c(my.pch[i],NA),
                    col=c("black", my.colors[i]), lty=c(NA, 1), cex=.85)
    }        
}

#   Add title
mtext( side=3, at=max(df$batchDate), text=paste(attr(df,"site.abbr"), ", ", attr(df,"species.name"), ", ", attr(df,"run.name"), " run", sep=""), adj=1, cex=1.5, line=1.25 )


if( !is.na(file) ){
    dev.off(dev.cur())
    ans <- out.pass.graphs
} else {
    ans <- NULL
}

ans
}
