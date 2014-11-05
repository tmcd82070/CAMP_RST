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

rng.y <- range(df$catch[ df$catch < Inf], na.rm=T)
plot( range(df$batchDate), rng.y, type="n", xlab="Date", ylab="Daily Raw (un-inflated) catch", xaxt="n", yaxt="n" )
    
lab.x.at <- pretty(df$batchDate)
axis( side=1, at=lab.x.at, label=format(lab.x.at, "%d%b%y"))

lab.y.at <- pretty(rng.y)
axis( side=2, at=lab.y.at, label=formatC(lab.y.at, big.mark=",") )

traps <- sort(unique(df$trapPositionID))
if( length(traps) == 1 ){
    my.colors <- "red"
} else if (length(traps) == 2){
    my.colors <- c("red","blue")
} else {
    my.colors <- rainbow(length(traps))
}
my.pch <- 15 + 1:length(traps)
for( i in 1:length(traps) ){

#   This adds the smoothed interpolation model to the plot.  
    ind <- df$trapPositionID == traps[i]
    lines( supsmu(df$batchDate[ind], df$catch[ind]), lwd=2, lty=1, col=my.colors[i] )

    ind <- df$trapPositionID == traps[i] & imputed
    points( df$batchDate[ ind ], df$catch[ ind ], pch=my.pch[i]-15, col=my.colors[i], cex=1 )

    ind <- df$trapPositionID == traps[i] & !imputed
    points( df$batchDate[ ind ], df$catch[ ind ], pch=my.pch[i], col=my.colors[i] )

}


catch.df.sites <- unique(df[,c('trapPositionID','TrapPosition')])       # jason add
colnames(catch.df.sites) <- c('subSiteID','subSiteName')                         # jason add
subsite.name <- catch.df.sites
subsite.name$subSiteName <- as.character(subsite.name$subSiteName)
# subsite.name <- attr(df, "subsites")


mx.len.name <- which.max( nchar(subsite.name$subSiteName) )
tmp <- legend( "topright", title=subsite.name$subSiteName[mx.len.name], legend=c("Observed","Imputed"), pch=rep(my.pch[1],2), cex=.85, plot=FALSE ) # don't plot, need the left coordinate here
tmp$rect$top <- tmp$rect$top + tmp$rect$h
for( i in 1:length(traps)){
    trap.name <- subsite.name$subSiteName[ subsite.name$subSiteID == traps[i] ]
    tmp <- legend( c(tmp$rect$left,tmp$rect$left + tmp$rect$w), c(tmp$rect$top - tmp$rect$h, tmp$rect$top - 2*tmp$rect$h) , title=trap.name, 
                    legend=c("Observed","Imputed"), 
                    pch=c(my.pch[i],my.pch[i]-15), col=c(my.colors[i], my.colors[i]), cex=.85, pt.cex=1.25)
}

#   Add title
mtext( side=3, at=max(df$batchDate), text=attr(df,"site.name"), adj=1, cex=1.5, line=2 )
mtext( side=3, at=max(df$batchDate), text= paste(attr(df,"species.name"), ", ", attr(df,"run.name"), " run, ", attr(df,"life.stage"), sep=""), adj=1, cex=.75, line=1 )

if( !is.na(file) ){
    dev.off(dev.cur())
    ans <-  out.pass.graphs
} else {
    ans <- NULL
}

ans

}
