#' @export
#' 
#' @title F.plot.catch.model
#'   
#' @description Plot smoothed, and possibly imputed, catch data over days per trap.
#'   
#' @param df A data frame that includes variables \code{trapPositionID},
#'   \code{TrapPosition}, \code{batchDate}, \code{imputed.catch},
#'   \code{modAssignedCatch}, and \code{imputedCatch}.
#' @param file The name of the file prefix under which output is to be saved. 
#'   Set to \code{NA} to plot to the Plot window.
#'   
#' @return A \code{png} graphical display of Daily Raw (uninflated) catch, per 
#'   trap, by day.
#'   
#' @details The input data frame usually has other catch variables, although 
#'   these currently serve no graphical purpose.  Variable \code{trapPositionID}
#'   contains the numeric code of a trap, while variable \code{TrapPosition}
#'   contains its text description.  Variable \code{imputed.catch} holds the
#'   proportion of time, for that day, for which imputed occurred;  as such, its
#'   values are between zero and one, inclusive.  Variable \code{imputedCatch}
#'   contains the total number of imputed fish for the day recorded via
#'   \code{batchDate}. Variable \code{modAssignedCatch} contains the total
#'   number of caught fish for that \code{batchDate} and incorporates any extra
#'   fish arising from half-cone operations.  The value of \code{imputedCatch}
#'   is necessarily equal to or less than the value of \code{modAssignedCatch}.
#'   
#'   The imputation curve displayed in the graph is obtained via function 
#'   \code{supersmu}, and serves as a reasonable approximation to the actual 
#'   spline used in the imputation procedure contained in function 
#'   \code{F.catch.model}.
#'   
#' @seealso \code{F.catch.model}
#' 
#' @author WEST Inc.
#'
#' @examples 
#' \dontrun{
#' #   ---- Create a data frame.
#' batchDates <- as.POSIXct(strftime(seq(from=c(ISOdate(2014,1,1)),
#'   by="day",length.out=80),format="%F"),tz="America/Los_Angeles")
#' df <- data.frame(trapPositionID=c(rep(12345,80),rep(98765,80)),
#'                  TrapPosition=c(rep('Left Bank',80),rep('Right Bank',80)),
#'                  batchDate=batchDates,
#'                  imputed.catch=rep(0,160),
#'                  modAssignedCatch=20*rnorm(length(batchDates))+
#'                  624.26*exp(-(as.numeric(batchDates)/86400 - 16120)^2/260),
#'                  imputedCatch=rep(0,160))
#' 
#' df$modAssignedCatch <- ifelse(df$trapPositionID == 98765,
#'   15*rnorm(nrow(df))+0.50*df$modAssignedCatch,df$modAssignedCatch)
#' df$modAssignedCatch[df$modAssignedCatch < 0] <- 0
#' df$imputedCatch[120:124] <- df$modAssignedCatch[120:124]
#' df$imputed.catch[120:124] <- rep(1,5)
#' 
#' attr(df,"site.name") <- as.factor(c("A River in the Central Valley"))
#' attr(df,"species.name") <- "Chinook Salmon"
#' attr(df,"run.name") <- "Fall"
#' attr(df,"life.stage") <- "Fry"
#' 
#' #   ---- Plot results to plot window.
#' F.plot.catch.model( df, file=NA )
#' }
F.plot.catch.model <- function( df, file=NA ){
#
# df   <- masterCatch     #df <- example
# file <- plot.file

  #   ---- If file=NA, a pdf graphing device is assumed to be open already.
  if( !is.na(file) ){
    
    #   ---- Shut down all graphics devices.
    for(i in dev.list()) dev.off(i)

    #   ---- Open PNG device.
    out.pass.graphs <- paste(file, "_catch.png", sep="")
    if(file.exists(out.pass.graphs)){
        file.remove(out.pass.graphs)
    }
    
    #   ---- Produces hi-res graphs unless there's an error, then uses default png setting.
    tryCatch({png(filename=out.pass.graphs,width=7,height=7,units="in",res=600)}, error=function(x){png(filename=out.pass.graphs)})
  }

  imputed <- df$imputed.catch > 0 & !is.na(df$imputed.catch)
  rng.y <- range(df$totalEstimatedCatch[ df$totalEstimatedCatch < Inf], na.rm=T)

  #   ---- Set up the plot space.
  plot( range(df$batchDate), rng.y, type="n", xlab="Date", ylab="Daily Raw (un-inflated) catch", xaxt="n", yaxt="n" )
    
  #   ---- Add in the x-axis.
  lab.x.at <- pretty(df$batchDate)
  axis( side=1, at=lab.x.at, labels=format(lab.x.at, "%d%b%y"))

  #   ---- Add in the y-axis. 
  lab.y.at <- pretty(rng.y)
  axis( side=2, at=lab.y.at, labels=formatC(lab.y.at, big.mark=",") )

  #   ---- Set up the colors and pch symbols for use with different trapPositionIDs.  
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


  #   ---- Insert the data points and smoothed values trap-by-trap.
  for( i in 1:length(traps) ){
    
    #   ---- Insert a "supersmoother" of the modified assigned catch (adjusted for half-cone counts).
    # ind <- df$trapPositionID == traps[i] & !is.na(df$totalEstimatedCatch)
    # print(lines( supsmu(df$batchDate[ind], df$totalEstimatedCatch[ind]), lwd=2, lty=1, col=my.colors[i] ))

    #   ---- Insert the imputed data points.
    ind <- df$trapPositionID == traps[i] & imputed
    print(points( df$batchDate[ind], df$imputedCatch[ind], pch=my.pch[i]-15, col=my.colors[i], cex=1 ))

    #   ---- Insert the observed data points.
    ind <- df$trapPositionID == traps[i] & !imputed
    print(points( df$batchDate[ind], df$totalEstimatedCatch[ind], pch=my.pch[i], col=my.colors[i] ))
    
  }
  #   ---- Set-up trapPosition names in lieu of their IDs. 
  catch.df.sites <- unique(na.omit(df[,c('trapPositionID','TrapPosition')]))
  colnames(catch.df.sites) <- c('subSiteID','subSiteName') 
  subsite.name <- catch.df.sites
  subsite.name$subSiteName <- as.character(subsite.name$subSiteName)

  #   ---- Build up the legend, so each trap gets an entry.  
  mx.len.name <- which.max( nchar(subsite.name$subSiteName) )
  
  #   ---- Get the left coordinate here;  note that we don't plot.
  tmp <- legend( "topright",title=subsite.name$subSiteName[mx.len.name],legend=c("Observed","Imputed"),pch=rep(my.pch[1],2),cex=.85,plot=FALSE ) 
  tmp$rect$top <- tmp$rect$top + tmp$rect$h
  for( i in 1:length(traps)){
    trap.name <- subsite.name$subSiteName[ subsite.name$subSiteID == traps[i] ]
    tmp <- legend( c(tmp$rect$left,tmp$rect$left + tmp$rect$w), c(tmp$rect$top - tmp$rect$h, tmp$rect$top - 2*tmp$rect$h) , title=trap.name, 
                    legend=c("Observed","Imputed"), 
                    pch=c(my.pch[i],my.pch[i]-15), col=c(my.colors[i], my.colors[i]), cex=.85, pt.cex=1.25)
  }

  #   ---- Add title.
  mtext( side=3, at=max(df$batchDate), text=attr(df,"site.name"), adj=1, cex=1.5, line=2 )
  mtext( side=3, at=max(df$batchDate), text= paste(attr(df,"species.name"), ", ", attr(df,"run.name"), " run, ", attr(df,"life.stage"), sep=""), adj=1, cex=.75, line=1 )
  
  
  #   ---- Output file with programmatically set name. 
  if( !is.na(file) ){
    dev.off(dev.cur())
    ans <-  out.pass.graphs
  } else {
    ans <- NULL
  }

ans

}
