F.plot.catch.model <- function( df, file=NA ){
#
#   Plot the catch GAM model, showing observed and imputed
#   values.
#
#   input:
#   df = data frame input to F.catch.model.  GAM fit an attribute of the df. 
#   file = If file is not NA, save the graph to this file name.
#
#   Output:
#   A time series plot of catch by date.
#



#   If file=NA, a pdf graphing device is assumed to be open already.
if( !is.na(file) ){
    #   Shut down all graphics devices
    for(i in dev.list()) dev.off(i)

    #   ---- Open PNG device
    out.pass.graphs <- paste(file, "_catch.png", sep="")
    if(file.exists(out.pass.graphs)){
        file.remove(out.pass.graphs)
    }
    tryCatch({png(file=out.pass.graphs,width=7,height=7,units="in",res=600)}, error=function(x){png(file=out.pass.graphs)})  # produces hi-res graphs unless there's an error, then uses default png settings

}


imputed <- df$imputed.catch > 0


plot( range(df$batchDate), range(df$catch[ df$catch < Inf], na.rm=T), type="n", xlab="Date", 
    ylab="Daily Raw (un-inflated) catch", xaxt="n" )
    
lab.x.at <- pretty(df$batchDate)
axis( side=1, at=lab.x.at, label=format(lab.x.at, "%d%b%y"))


traps <- sort(unique(df$trapPositionID))
my.colors <- rainbow(length(traps))
my.pch <- 15 + 1:length(traps)
for( i in 1:length(traps) ){

#   This adds the smoothed interpolation model to the plot.  I think it's ugly.
#    ind <- df$trapPositionID == traps[i]
#    lines( df$batchDate[ind], df$catch[ind], lwd=1, lty=1 )

    ind <- df$trapPositionID == traps[i] & imputed
    points( df$batchDate[ ind ], df$catch[ ind ], pch=my.pch[i], col=my.colors[i], cex=1 )

    ind <- df$trapPositionID == traps[i] & !imputed
    points( df$batchDate[ ind ], df$catch[ ind ], pch=my.pch[i], col="black" )

}


for( i in 1:length(traps)){
    if( i == 1 ){
        tmp <- legend( "topright", title=paste("Trap", traps[i]), legend=c("Observed","Imputed"), pch=c(my.pch[i],my.pch[i]),
                    col=c("black", my.colors[i]), cex=.85)
    } else {
        tmp <- legend( tmp$rect$left, tmp$rect$top - tmp$rect$h, title=paste("Trap", traps[i]), legend=c("Observed","Imputed"), 
                    pch=c(my.pch[i],my.pch[i]), col=c("black", my.colors[i]), cex=.85)
    }        
}

#   Add title
mtext( side=3, at=max(df$batchDate), text=paste(attr(df,"site.abbr"), ", ", attr(df,"species.name"), ", ", attr(df,"run.name"), " run", sep=""), adj=1, cex=1.5, line=1.25 )

if( !is.na(file) ){
    dev.off(dev.cur())
    ans <-  out.pass.graphs
} else {
    ans <- NULL
}

ans

}
