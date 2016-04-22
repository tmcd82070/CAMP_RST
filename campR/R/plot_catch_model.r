#' @export F.plot.catch.model
#' 
#' @title F.plot.catch.model
#' 
#' @description
#' 
#'    Plot the catch GAM model, showing observed and imputed
#'    values.
#' 
#'    input:
#'    df = data frame input to F.catch.model.  GAM fit an attribute of the df. 
#'    file = If file is not NA, save the graph to this file name.
#' 
#'    Output:
#'    A time series plot of catch by date.
#' 
#' 
#' 
#'  df <- masterCatch
#'   file <- plot.file
#' 
#' 
#' 
#' @param  df <describe argument>
#' @param  file=NA  <describe argument>
#' 
#' @details <other comments found in file>
#'    If file=NA, a pdf graphing device is assumed to be open already.
#'    Shut down all graphics devices
#'    ---- Open PNG device
#'  impute the imputed number of fish into unassd catch days.  awkward, b/c imputed catch
#'  based on plus-counted fish, but unassd based on measured fish (non-plus-counted).
#' df$unassdcatch <- ifelse(df$imputed.catch > 0,df$catch,df$unassdcatch)
#'  jason add 4/20/2015 -- plotting only measured values means that many days in df have NA.
#'  we need to pull those out before we plot--sometimes, a trap could have caught many fish,
#'  none of which were measured.  so, these traps end up not having anything to plot, even
#'  though the code thinks data for that plot is there.  this causes errors.
#' df <- df[ df$imputed.catch != 0 | !is.na(df$unassdcatch) ,]
#'  jason turn on -- have to be creative -- imputed and unassd catch generally on
#'  different scales, since imputed based on plus-counts, but unassigned on raw catches.
#'    This adds the smoothed interpolation model to the plot.  
#'      ind <- df$trapPositionID == traps[i]          # jason turns off
#'      lines( supsmu(df$batchDate[ind], df$catch[ind]), lwd=2, lty=1, col=my.colors[i] )
#'  jason 4/20/3015 -- use new var indOld?
#' catch.df.sites <- unique(df[,c('trapPositionID','TrapPosition')])       # jason add   # delete later?  added by github merge process.
#'  subsite.name <- attr(df, "subsites")
#'    Add title
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


# df <- masterCatch
#  file <- plot.file


  
  
# jason -- 6/4/2015.  doug wants measured for now, so assdCatch always s/b plotted.
# jason -- 6/15/2015. doug changed his mind. back to totalCatch
# if( attr(df,"life.stage") == 'All'){
  catchMetric <- 'totalEstimatedCatch'  #'totalCatch'
# } else {
#   catchMetric <- 'modAssignedCatch'   #'assdCatch'
# }


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

# impute the imputed number of fish into unassd catch days.  awkward, b/c imputed catch
# based on plus-counted fish, but unassd based on measured fish (non-plus-counted).
#df$unassdcatch <- ifelse(df$imputed.catch > 0,df$catch,df$unassdcatch)

# jason add 4/20/2015 -- plotting only measured values means that many days in df have NA.
# we need to pull those out before we plot--sometimes, a trap could have caught many fish,
# none of which were measured.  so, these traps end up not having anything to plot, even
# though the code thinks data for that plot is there.  this causes errors.
#df <- df[ df$imputed.catch != 0 | !is.na(df$unassdcatch) ,]

imputed <- df$imputed.catch > 0 & !is.na(df$imputed.catch)

# jason turn on -- have to be creative -- imputed and unassd catch generally on
# different scales, since imputed based on plus-counts, but unassigned on raw catches.

if(catchMetric == 'modAssignedCatch'){#'assdCatch'){
  rng.y <- range(df$modAssignedCatch[ df$modAssignedCatch < Inf], na.rm=T) #range(df$assdCatch[ df$assdCatch < Inf], na.rm=T) 
} else {
  rng.y <- range(df$totalEstimatedCatch[ df$totalEstimatedCatch < Inf], na.rm=T)#range(df$totalCatch[ df$totalCatch < Inf], na.rm=T)
}

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
if( length(traps) > 10){
  my.pch <- c(15 + 1:10,seq(1,length(traps) - 10))
} else {
  my.pch <- 15 + 1:length(traps)
}

for( i in 1:length(traps) ){

#   This adds the smoothed interpolation model to the plot.  
#     ind <- df$trapPositionID == traps[i]          # jason turns off
#     lines( supsmu(df$batchDate[ind], df$catch[ind]), lwd=2, lty=1, col=my.colors[i] )
  
    # jason 4/20/3015 -- use new var indOld?
  if(catchMetric == 'totalCatch'){
    
    ind <- df$trapPositionID == traps[i] & !is.na(df$modAssignedCatch)#ind <- df$trapPositionID == traps[i] & !is.na(df$assdCatch)
    lines( supsmu(df$batchDate[ind], df$totalEstimatedCatch[ind]), lwd=2, lty=1, col=my.colors[i] )  #lines( supsmu(df$batchDate[ind], df$totalCatch[ind]), lwd=2, lty=1, col=my.colors[i] )    
    
    ind <- df$trapPositionID == traps[i] & imputed
    points( df$batchDate[ ind ], df$imputedCatch[ ind ], pch=my.pch[i]-15, col=my.colors[i], cex=1 )    # jason 4/17/2015 - jason changes catch to imputedCatch

    ind <- df$trapPositionID == traps[i] & !imputed
    points( df$batchDate[ ind ], df$totalEstimatedCatch[ ind ], pch=my.pch[i], col=my.colors[i] )#points( df$batchDate[ ind ], df$totalCatch[ ind ], pch=my.pch[i], col=my.colors[i] )              # jason 4/17/2015 - jason changes catch to assdCatch
    
  } else {
    
    ind <- df$trapPositionID == traps[i] & !is.na(df$modAssignedCatch)#ind <- df$trapPositionID == traps[i] & !is.na(df$assdCatch)
    lines( supsmu(df$batchDate[ind], df$modAssignedCatch[ind]), lwd=2, lty=1, col=my.colors[i] )  #lines( supsmu(df$batchDate[ind], df$assdCatch[ind]), lwd=2, lty=1, col=my.colors[i] )    
    
    ind <- df$trapPositionID == traps[i] & imputed
    points( df$batchDate[ ind ], df$imputedCatch[ ind ], pch=my.pch[i]-15, col=my.colors[i], cex=1 )    # jason 4/17/2015 - jason changes catch to imputedCatch
    
    ind <- df$trapPositionID == traps[i] & !imputed
    points( df$batchDate[ ind ], df$modAssignedCatch[ ind ], pch=my.pch[i], col=my.colors[i] )  #points( df$batchDate[ ind ], df$assdCatch[ ind ], pch=my.pch[i], col=my.colors[i] )              # jason 4/17/2015 - jason changes catch to assdCatch
    
  }

}


#catch.df.sites <- unique(df[,c('trapPositionID','TrapPosition')])       # jason add   # delete later?  added by github merge process.
catch.df.sites <- unique(na.omit(df[,c('trapPositionID','TrapPosition')]))       # jason add  
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
