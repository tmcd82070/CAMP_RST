F.plot.passage <- function( df, out.file="passage.png" ){
#
#   Plot a time series of passage estimates.  This can be called from
#   Sweave files to put a figure in a report.
#
#   This file produces a bar graph of the passage estimates in df. 
#
#   input:
#   df = passage estimate data frame from F.est.passage.  This is passage summarized by "day", "week", "month" or "year".
#       It is assumed that the percentage of the estimate that was imputed is available in df$pct.imputed.catch. 
#

if( !is.na(out.file) ){
    #   ---- Open PNG device
    out.pass.graphs <- paste(out.file, "_passage.png", sep="")
    if(file.exists(out.pass.graphs)){
        file.remove(out.pass.graphs)
    }
    tryCatch({png(file=out.pass.graphs,width=7,height=7,units="in",res=600)}, error=function(x){png(file=out.pass.graphs)})  # produces hi-res graphs unless there's an error, then uses default png settings
}


#   Construct matrix of bar heights
pass <- matrix( c(df$passage * df$pct.imputed.catch, df$passage * (1 - df$pct.imputed.catch)), ncol=nrow(df), byrow=T )
dimnames(pass) <- list(NULL, df$date)



#   Compute extent of y axis
hgts <- colSums( pass )
lab.y.at <- pretty( hgts )


#   GRaph using barplot
mp <- barplot( pass, beside=FALSE, space=0, col=c("lightblue","darkorange"), 
    legend.text=F, ylab="", xaxt="n", yaxt="n", ylim=range(c(hgts, lab.y.at)), 
    xlab=capwords(attr(df,"summarized.by")), cex.lab=1.5 )


mtext( "Number of fish", side=2, line=2.25, cex=1.5)

#   add a smoother line here. 
lines( supsmu( mp, hgts), lwd=2, col="darkblue" )

#   Add x axis labels
dt <- format(df$date, "%m-%y")
dt <- dt[ !duplicated(dt) ]
dt <- as.POSIXct( strptime( paste( "1", dt, sep="-" ), format="%d-%m-%y"))

ind <- as.numeric(cut( dt, df$date ))
dt <- dt[ !is.na(ind) ]
ind <- ind[!is.na(ind)]   # because first of some month may be less than first date

lab.x.at <- mp[ind]
lab.x.lab <- format(dt, "%d%b%y")

axis( side=1, at=lab.x.at, label=rep("", length(lab.x.at)) )
#axis( side=1, at=lab.x.at, label=lab.x.lab)

#   Do the following if you want perpendicular tick labels
for( i in 1:length(lab.x.at) ){
    mtext( side=1, at=lab.x.at[i], text=lab.x.lab[i], las=0, line=1 )
}

#   Add y axis labels
lab.y.lab <- format( lab.y.at, scientific=F, big.mark=",", trim=T )
axis( side=2, at=lab.y.at, label=lab.y.lab )


#   Add total passage
N <- round(sum( df$passage ))
mtext( side=3, at=max(mp),  text=paste("N =", format(N, scientific=F, big.mark="," )), adj=1, cex=1 )

#   Add title
mtext( side=3, at=max(mp), text=paste(attr(df,"site.abbr"), ", ", attr(df,"species.name"), ", ", attr(df,"run.name"), " run", sep=""), adj=1, cex=1.5, line=1.25 )

#plot( df$datetime, df$passage ,type="b", xlab="Date", ylab="Number of fish", xaxt="n", cex.lab=1.5)
#mtext(side=3, text=attr(df, "site.name"), cex=1, line=.75 )
#mtext(side=3, text=paste(attr(df, "species.name"), ", ", attr(df, "run.name"), " run", sep=""), line=2, cex=2 )
#
#imp.ind <- df$pct.imputed.catch > 0   # if data summarized by 'week' 'month' or 'year', df$imputed is
#                            # a percentage of the values in that period that were missing.
#
#
#points( df$datetime[imp.ind], df$passage[imp.ind], col="red", pch=1 )
#points( df$datetime[!imp.ind], df$passage[!imp.ind], col="black", pch=16 )
#
legend( "topright", legend=c("Observed","Imputed"),
    fill=c("darkorange", "lightblue"), cex=1)

if( !is.na(out.file) ){
    dev.off(dev.cur())
    ans <- out.pass.graphs
} else {
    ans <- NULL
}

ans

}
