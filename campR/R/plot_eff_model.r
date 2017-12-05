#' @export
#' 
#' @title F.plot.eff.model
#'   
#' @description Plot per-trap efficiency values for all days between specified
#'   dates.
#'   
#' @param df A data frame that includes variables \code{TrapPositionID}, 
#'   \code{batchDate}, \code{nReleased}, \code{nCaught}, \code{efficiency}, 
#'   \code{imputed.eff}, and \code{trapPositionID}.  Currently variables 
#'   \code{TrapPositionID} and \code{trapPositionID} appear to be the same, 
#'   although both are called in subsequent points in the code.
#' @param file The name of the file prefix under which output is to be saved. 
#'   Set to \code{NA} to plot to the Plot window.
#'   
#' @return Output includes a \code{png} graphical display of estimated 
#'   efficiencies, per trap, by day, as well as a \code{csv} of the data points 
#'   utilized in making the graph.  The \code{csv} includes both the estimated 
#'   values from the specified efficiency model fit by function 
#'   \code{F.efficiency.model}, as well as the observed counts of fish deriving 
#'   from efficiency trials.
#'   
#' @seealso \code{F.efficiency.model}
#'   
#' @author WEST Inc.
#'   
#' @examples 
#' \dontrun{
#' #   ---- Plot efficiency results housed in data frame 
#' #   ---- df to the plot window.
#' F.plot.eff.model( df, file=NA )
#' }

#   ---- Currently, the code for this functions queries an Access db to get 
#   ---- site names;  thus, example code won't currently work.  Save for 
#   ---- possibly future incorporation.  
# # Create a data frame.
# df <- data.frame(trapPositionID=c(rep(12345,80),rep(98765,80)),
#                  batchDate=as.POSIXct(strftime(seq(from=c(ISOdate(2014,1,1)),by="day",length.out=80),format="%F"),tz="America/Los_Angeles"),
#                  nReleased=c(rep(NA,40),1050,rep(NA,15),844,rep(NA,23),rep(NA,40),1050,rep(NA,15),844,rep(NA,23)),
#                  nCaught=c(rep(NA,40),52,rep(NA,15),10,rep(NA,23),rep(NA,40),71,rep(NA,15),15,rep(NA,23)),
#                  efficiency=c(rep(0.03273495,80),rep(0.04540655,80)),
#                  imputed.eff=c(rep("Yes",160)))
# subsites <- data.frame(subSiteName=c('Left Bank','Right Bank'),subSiteID=c(12345,98765))
# subsites$subSiteName <- as.character(droplevels(subsites$subSiteName))
# attr(df,"subsites") <- subsites
# attr(df,"site.name") <- as.factor(c("A River in the Central Valley"))

F.plot.eff.model <- function( df, file ){
  #
  #   df <- ans    df <- example
  #   file <- plot.file
  
  #   ---- Get global environment information. 
  db.file <- get("db.file",envir=.GlobalEnv)
  forkLengthCutPoints <- get("forkLengthCutPoints",envir=.GlobalEnv)
  
  #   ---- Report filename string at this point.  Helps with checking.
  cat(paste0(file,"\n"))
  
  #   ---- Compute results from efficiency trials.  
  imputed <- df$imputed.eff == "Yes"
  pts <- !is.na(df$nReleased)
  
  pred.y <- rep(NA, nrow(df))
  pred.y[!imputed] <- df$efficiency[!imputed]
  
  eff <- df$nCaught / df$nReleased
  
  #   ---- Get subsite names.
  ch <- odbcConnectAccess(db.file)
  SubSites <- sqlFetch(ch, "SubSite")
  close(ch)
  
  #   ---- Bring in subsite info on the data.
  SubSites <- SubSites[,names(SubSites) %in% c('subSiteName','subSiteID')]
  names(SubSites)[names(SubSites) == 'subSiteID'] <- 'trapPositionID'
  attrA <- attr(df,"subsites")     # merge drops attributes...
  attrB <- attr(df,"site.name")
  df <- merge(df,SubSites,by=c('trapPositionID'),all.x=TRUE)
  df <- df[,c('trapPositionID','subSiteName','batchDate','nReleased','nCaught','efficiency','imputed.eff')]
  attr(df,"subsites") <- attrA     # ...so now, put them back in
  attr(df,"site.name") <- attrB
  
  #   ---- We want to reduce the output.  "file" up to this point already has season name 
  #   ---- incorporated.  Find which options ends up with the shortest new file (file2),
  #   ---- and take that as the new file name.  
  s <- c("-Spring","-Fall","-Late fall","-Winter",paste0(rep("-",4),forkLengthCutPoints$lifeStage,rep("Fall",4)),
         "-FryFall","-SmoltFall","-ParrFall","-YearlingFall","-FryLate fall","-SmoltLate fall","-ParrLate fall","-YearlingLate fall",
         "-FryWinter","-SmoltWinter","-ParrWinter","-YearlingWinter","-FrySpring","-SmoltSpring","-ParrSpring","-YearlingSpring",
         "-SmallFall","-SmallLate fall","-SmallWinter","-SmallSpring",
         "-MediumFall","-MediumLate fall","-MediumWinter","-MediumSpring",
         "-LargeFall","-LargeLate fall","-LargeWinter","-LargeSpring",
         "-FL1 leq 41mmFall","-FL1 leq 41mmLate fall","-FL1 leq 41mmWinter","-FL1 leq 41mmSpring",
         "-FL2 42-72mmFall","-FL2 42-72mmLate fall","-FL2 42-72mmWinter","-FL2 42-72mmSpring",
         "-FL3 73-110mmFall","-FL3 73-110mmLate fall","-FL3 73-110mmWinter","-FL3 73-110mmSpring",
         "-FL4 geq 111mmFall","-FL4 geq 111mmLate fall","-FL4 geq 111mmWinter","-FL4 geq 111mmSpring")
  file2 <- sapply(seq_along(s), function(x) gsub(s[x], "", file,fixed=TRUE))
  file3 <- file2[nchar(file2) == min(nchar(file2))][1]
  
  #   ---- If efficiency files exist, remove them for overwriting.
  if(file.exists(paste0(file3,"_eff.png"))){file.remove(paste0(file3,"_eff.png"))}
  if(file.exists(paste0(file3,"_effTable.csv"))){file.remove(paste0(file3,"_effTable.csv"))}
  
  #   ---- Write out estimates.
  if( !is.na(file) & sum(grepl(paste(file3,"_effTable.csv"),dir(dirname(file)),fixed=TRUE)) == 0){
    out.pass.graphs.eff <- paste(file3, "_effTable.csv", sep="")
    df2 <- df[!is.na(df$nReleased),]
    #sink(paste0(file3,'_effTable.csv'))
    #write.table( df2, file=paste0(file3,'_effTable.csv'), sep=",", append=FALSE, row.names=FALSE, col.names=TRUE)
    write.csv( df2, row.names=FALSE, file=paste0(file3,'_effTable.csv'))
    #sink()
  }
  
  #   ---- Start making the plot.
  #   ---- Prepare for plotting, but only if the plot doesn't already exist.
  if( !is.na(file) & sum(grepl(paste0(file3,"_eff.png"),dir(dirname(file)),fixed=TRUE)) == 0 ){
    
    #   ---- Shut down all graphics devices.
    for(i in dev.list()) dev.off(i)
    
    #   ---- Open PNG device.
    out.pass.graphs <- paste(file3, "_eff.png", sep="")
    if(file.exists(out.pass.graphs)){
      file.remove(out.pass.graphs)
    }
    
    out.pass.graphs <- paste0(file3,"_eff.png")
    
    tryCatch({png(filename=out.pass.graphs,width=7,height=7,units="in",res=600)}, error=function(x){png(filename=out.pass.graphs)})

    plot( range(df$batchDate,na.rm=T), range(eff,na.rm=T), type="n", xlab="Date", 
          ylab="Efficiency proportion", xaxt="n" )
    lab.x.at <- pretty(df$batchDate)
    axis( side=1, at=lab.x.at, labels=format(lab.x.at, "%d%b%y"))
  
    #   ---- Set up points and lines.
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
    
    #  ---- Draw points and lines, based on run, etc.
    for( i in 1:length(traps) ){
      
      #   ---- Indicators for season and such.
      ind.pts <- df$trapPositionID == traps[i] & pts
      ind.line <- df$trapPositionID == traps[i] & imputed
      
      #   ---- The following plots period between trial.
      strt.pts <- min(df$batchDate[ind.pts])
      end.pts <- max(df$batchDate[ind.pts])
      
      pre.season <-  df$batchDate[ind.line] < strt.pts
      post.season <- end.pts < df$batchDate[ind.line]
      during.season <- (strt.pts <= df$batchDate[ind.line]) & (df$batchDate[ind.line] <= end.pts)
      
      #   ---- The following plots all time.  
      strt <- min(df$batchDate)
      end <- max(df$batchDate)
      
      trials.season <- (strt <= df$batchDate[ind.line]) & (df$batchDate[ind.line] <= end)
      
      #   ---- Draw lines.
      lines( df$batchDate[ ind.line ][trials.season & pre.season], df$efficiency[ ind.line ][trials.season & pre.season], lwd=3, col=my.colors[i] )
      lines( df$batchDate[ ind.line ][trials.season & during.season], df$efficiency[ ind.line ][trials.season & during.season], lwd=3, col=my.colors[i] )
      lines( df$batchDate[ ind.line ][trials.season & post.season], df$efficiency[ ind.line ][trials.season & post.season], lwd=3, col=my.colors[i] )
    
      #   ---- Draw points.
      eff <- df$nCaught[ind.pts] / df$nReleased[ ind.pts ] 
      points( df$batchDate[ ind.pts ], eff, pch=my.pch[i], col=my.colors[i] )
      
    }
    
    #   ---- Draw legend.
    subsite.name <- attr(df, "subsites")
    mx.len.name <- which.max( nchar(subsite.name$subSiteName) )
    tmp <- legend( "topleft", title=subsite.name$subSiteName[mx.len.name], legend=c("Observed","Predicted"), pch=rep(my.pch[1],2), cex=.85, pt.cex=1.25, lty=c(NA,1), plot=FALSE ) # don't plot, need the left coordinate here
    tmp$rect$top <- tmp$rect$top + tmp$rect$h  # + goes up, - goes down
    
    #   ---- Add a legend block / entry for each included trap.
    traps <- as.numeric(traps)
    for( i in 1:length(traps)){
      trap.name <- subsite.name$subSiteName[ subsite.name$subSiteID == traps[i] ]
      tmp <- legend( c(tmp$rect$left,tmp$rect$left + tmp$rect$w), c(tmp$rect$top - tmp$rect$h, tmp$rect$top - 2*tmp$rect$h) , title=trap.name, 
                     legend=c("Observed","Predicted"), 
                     pch=c(my.pch[i],NA), col=c(my.colors[i], my.colors[i]), lty=c(NA,1), cex=.85, pt.cex=1.25 )
    }
  
    #   ---- Add title.
    mtext( side=3, at=max(df$batchDate), text=attr(df,"site.name"), adj=1, cex=1.5, line=2 )
    mtext( side=3, at=max(df$batchDate), text= "Efficiency Trials", adj=1, cex=.75, line=1 )
    
    #   ---- Output file with programmatically set name. 
    if( !is.na(file) ){
      dev.off(dev.cur())
    } 
  }
  
  if( !is.na(file) ){
    ans <- c(out.pass.graphs,out.pass.graphs.eff)
  } else {
    ans <- NULL
  }
  
  ans
}
