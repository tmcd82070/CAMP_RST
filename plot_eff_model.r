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
#   df <- ans
#   file <- plot.file


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

# get subsite Names
ch <- odbcConnectAccess(db.file)
SubSites <- sqlFetch(ch, "SubSite")
close(ch)

# bring in subsite info on the data
SubSites <- SubSites[,names(SubSites) %in% c('subSiteName','subSiteID')]
names(SubSites)[names(SubSites) == 'subSiteID'] <- 'trapPositionID'
attrA <- attr(df,"subsites")     # merge drops attributes...
attrB <- attr(df,"site.name")
df <- merge(df,SubSites,by=c('trapPositionID'),all.x=TRUE)
df <- df[,c('trapPositionID','subSiteName','batchDate','nReleased','nCaught','efficiency','imputed.eff')]
attr(df,"subsites") <- attrA     # ...so now, put them back in
attr(df,"site.name") <- attrB


# write out estimates.
out.pass.graphs.eff <- paste(file, "_effTable.csv", sep="")
sink(paste0(file,'_effTable.csv'))
write.table( df, file=paste0(file,'_effTable.csv'), sep=",", append=FALSE, row.names=FALSE, col.names=TRUE)
sink()


plot( range(df$batchDate,na.rm=T), range(eff,na.rm=T), type="n", xlab="Date", 
    ylab="Efficiency proportion", xaxt="n" )
    
lab.x.at <- pretty(df$batchDate)
axis( side=1, at=lab.x.at, label=format(lab.x.at, "%d%b%y"))


#   Draw points and lines
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

  
    #   indicators for season and such
    ind.pts <- df$trapPositionID == traps[i] & pts
    ind.line <- df$trapPositionID == traps[i] & imputed
    
    # The following plots period between trial
    strt.pts <- min(df$batchDate[ind.pts])
    end.pts <- max(df$batchDate[ind.pts])

    pre.season <-  df$batchDate[ind.line] < strt.pts
    post.season <- end.pts < df$batchDate[ind.line]
    during.season <- (strt.pts <= df$batchDate[ind.line]) & (df$batchDate[ind.line] <= end.pts)
    
    # The following plots all time
    strt <- min(df$batchDate)
    end <- max(df$batchDate)
    
    trials.season <- (strt <= df$batchDate[ind.line]) & (df$batchDate[ind.line] <= end)

   
    #   Draw lines
    lines( df$batchDate[ ind.line ][trials.season & pre.season], df$efficiency[ ind.line ][trials.season & pre.season], lwd=3, col=my.colors[i] )
    lines( df$batchDate[ ind.line ][trials.season & during.season], df$efficiency[ ind.line ][trials.season & during.season], lwd=3, col=my.colors[i] )
    lines( df$batchDate[ ind.line ][trials.season & post.season], df$efficiency[ ind.line ][trials.season & post.season], lwd=3, col=my.colors[i] )

    #   Draw points
    eff <- df$nCaught[ind.pts] / df$nReleased[ ind.pts ] 
    points( df$batchDate[ ind.pts ], eff, pch=my.pch[i], col=my.colors[i] )


}

#   Draw legend
subsite.name <- attr(df, "subsites")
mx.len.name <- which.max( nchar(subsite.name$subSiteName) )
tmp <- legend( "topleft", title=subsite.name$subSiteName[mx.len.name], legend=c("Observed","Predicted"), pch=rep(my.pch[1],2), cex=.85, pt.cex=1.25, lty=c(NA,1), plot=FALSE ) # don't plot, need the left coordinate here
tmp$rect$top <- tmp$rect$top + tmp$rect$h  # + goes up, - goes down

traps <- as.numeric(traps)
for( i in 1:length(traps)){
    trap.name <- subsite.name$subSiteName[ subsite.name$subSiteID == traps[i] ]
    tmp <- legend( c(tmp$rect$left,tmp$rect$left + tmp$rect$w), c(tmp$rect$top - tmp$rect$h, tmp$rect$top - 2*tmp$rect$h) , title=trap.name, 
                    legend=c("Observed","Predicted"), 
                    pch=c(my.pch[i],NA), col=c(my.colors[i], my.colors[i]), lty=c(NA,1), cex=.85, pt.cex=1.25 )
}

#   Add title
mtext( side=3, at=max(df$batchDate), text=attr(df,"site.name"), adj=1, cex=1.5, line=2 )
mtext( side=3, at=max(df$batchDate), text= "Efficiency Trials", adj=1, cex=.75, line=1 )



if( !is.na(file) ){
    dev.off(dev.cur())
    ans <- c(out.pass.graphs,out.pass.graphs.eff)
} else {
    ans <- NULL
}

ans
}
