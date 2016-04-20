#' @export plot_spline
#' 
#' @title plot_spline
#' 
#' @description
#' 
#' 
#' @param trap <describe argument>
#' @param catch.df <describe argument>
#' @param df.and.fit <describe argument>
#' @param file=NA <describe argument>
#' 
#' @details <other comments found in file>
#'  catch.df <- catch.df
#'  df.and.fit <- df.and.fit
#'  get a per-day record of halfcone status.  traps can have more than one
#'  record per day, so if at least one record on a day is half-cone, just
#'  say the whole day is half-cone, for plotting ease.  
#'  cannot use the df <- df.and.fit$df2 here -- the catch algorithm screws with the
#'  usually easily identifiable non-fishing periods.  so, use original catch.df to get
#'  halfcone status data.
#'  get the goods we need to make a spline plot.
#'  estimate fish counts via the spline model -- use rownames throughout for merging.
#'  merge model info with outcome and date
#'  summarize all observed catch to batchDate and bring in to model-spline df
#'  summarize all imputations of catch to batchDate 
#'  summarize all sums of observed and imputed catch to batchDate and bring in to model-spline df
#'  rename imputed catch and bring in to model-spline df
#'  bring in to model-spline df halfcone operation days
#'  for plotting ease, make dfs specific to different catch quantities
#'  --- now, make the plot. ---
#'    If file=NA, a pdf graphing device is assumed to be open already.
#'    Shut down all graphics devices
#'    ---- Open PNG device
#'  set graphical bounds
#'  natural scale plot -- make an empty plot/palette
#'  draw the vertical segments
#'  draw the exponentiated spline curve
#'  draw the dots of observed and imputed catch
#'  put in x-axis tick and labels
#'  put in a header
#'  make the legends
#'                Full Cone (1st legend)                                  Half Cone (2nd legend)                                        Other Stuff (3rd legend)
#'  ---- end plotting ----
#'  ---- compile graphing statistics and data ----------------------    
#' write.csv(jBaseTable,'C:/Users/jmitchell/Desktop/jBaseTable.csv')
#'         
#'    sum(na.omit(jBaseTable$preCatch))                 # assignedCatch + unassignedCatch
#'    sum(na.omit(as.vector(jBaseTable$imputedCatch)))  # imputedCatch
#'    sum(na.omit(jBaseTable$totalCatch))               # totalCatch
#'    sum(na.omit(allJBaseTable$preCatch))                 # assignedCatch + unassignedCatch
#'    sum(na.omit(as.vector(allJBaseTable$imputedCatch)))  # imputedCatch
#'    sum(na.omit(allJBaseTable$totalCatch))               # totalCatch 
#'  tapply(allJBaseTable[!is.na(allJBaseTable$preCatch),]$preCatch,allJBaseTable[!is.na(allJBaseTable$preCatch),]$siteID,FUN=sum)
#'    # log scale
#'    model2 <- model[model$n.tot != 0,]
#'    plot(model2$EndTime,model2$logTheP,type='l',col='red',ylim=c(min(model2$logTheP,model2$logN.tot),max(model2$logTheP,model2$logN.tot)))
#'    par(new=TRUE)
#'    plot(model2$EndTime,model2$logN.tot,type='p',pch=19,cex=0.7,col='black',ylim=c(min(model2$logTheP,model2$logN.tot),max(model2$logTheP,model2$logN.tot)))
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

plot_spline <- function(trap,catch.df,df.and.fit,file=NA){
  
  # trap <- trap
  # catch.df <- catch.df
  # df.and.fit <- df.and.fit

  df0 <- catch.df[catch.df$trapPositionID == trap,]
  df2 <- df.and.fit$df2
 
  # get a per-day record of halfcone status.  traps can have more than one
  # record per day, so if at least one record on a day is half-cone, just
  # say the whole day is half-cone, for plotting ease.  
  # cannot use the df <- df.and.fit$df2 here -- the catch algorithm screws with the
  # usually easily identifiable non-fishing periods.  so, use original catch.df to get
  # halfcone status data.
  half1 <- unique(df0[df0$halfConeID == 1 & df0$TrapStatus == 'Fishing',c('batchDate','halfConeID')])
  half2 <- unique(df0[df0$halfConeID == 2 & df0$TrapStatus == 'Fishing',c('batchDate','halfConeID')])
  
  half <- merge(half1,half2,by=c('batchDate'),all.x=TRUE,all.y=TRUE)
  half$batchDateC <- as.character(half$batchDate)
  half$batchDate <- NULL
  names(half)[names(half) == 'halfConeID.x'] <- 'halfConeY'
  names(half)[names(half) == 'halfConeID.y'] <- 'halfConeN'



  # get the goods we need to make a spline plot.
  theC <- df.and.fit$fit$coefficients
  theY <- df.and.fit$fit$data$n.tot
  theD <- data.frame(batchDate=seq(min(df.and.fit$df2$batchDate),max(df.and.fit$df2$batchDate),by=60*60*24))   # by day
  
  # estimate fish counts via the spline model -- use rownames throughout for merging.
  theX <- df.and.fit$fit$model                                      # i'm making a design matrix here with a column of 1s for the intercept
  theP <- data.frame(theP=exp( log(24)            + cbind(as.matrix(rep(1,nrow(theX)),nrow=nrow(theX),ncol=1),as.matrix(theX[,-(1:2)])) %*% theC),rownames=rownames(theX))
  
  theX2 <- theX
  theX2$rownames <- rownames(theX2)                                                          # put in identifier
  df2$rownames   <- rownames(df2)                                                            # put in identifier
  
  # merge model info with outcome and date
  model <- merge(theX2,df2[,c('rownames','batchDate','EndTime')],by=c('rownames'),all.x=TRUE)   
  model <- merge(model,theP,by=c('rownames'),all.x=TRUE)
  model <- model[order(model$EndTime),]
  model$logTheP <- log(model$theP)
  model$logN.tot <- log(model$n.tot)
  model$batchDateC <- as.character(model$batchDate)
  
  # summarize all observed catch to batchDate and bring in to model-spline df
  n.totDay <- data.frame(n.totDay=tapply(model$n.tot,model$batchDate,FUN=sum))   # tapply may not need to work on df model???
  n.totDay$batchDateC <- rownames(n.totDay)
  model <- merge(model,n.totDay,by=c('batchDateC'),all.x=TRUE)

  # summarize all imputations of catch to batchDate 
  impDay <- data.frame(n.totDay=tapply(df.and.fit$true.imp$n.tot,df.and.fit$true.imp$batchDate,FUN=sum))
  impDay$batchDateC <- rownames(impDay)

  # summarize all sums of observed and imputed catch to batchDate and bring in to model-spline df
  nEst <- rbind(n.totDay,impDay)
  nEstDay <- data.frame(nEstDay=tapply(nEst$n.totDay,nEst$batchDateC,FUN=sum))
  nEstDay$batchDateC <- rownames(nEstDay)
  model <- merge(model,nEstDay,by=c('batchDateC'),all.x=TRUE)

  # rename imputed catch and bring in to model-spline df
  names(impDay)[names(impDay) == 'n.totDay'] <- 'nImp'
  model <- merge(model,impDay,by=c('batchDateC'),all.x=TRUE,all.y=TRUE)               # all.y=T to get fully imputed days

  # bring in to model-spline df halfcone operation days
  model <- merge(model,half,by=c('batchDateC'),all.x=TRUE)
  
  model$nEstDay <- ifelse(is.na(model$rownames),model$nImp,model$nEstDay)


  # for plotting ease, make dfs specific to different catch quantities
  modelA <- model[!is.na(model$theP),]     # exponentiated spline catch
  modelB <- model[!is.na(model$n.tot),]    # observed catch, summarized via batchDate
  modelC <- model[!is.na(model$nEstDay),]  # observed catch + imputed catch, summarized via batchDate
  modelI <- model[!is.na(model$nImp) & !is.na(model$nEstDay) & (model$nImp != model$nEstDay) | ( !is.na(model$nImp) & !is.na(model$nEstDay) & (!is.na(model$n.tot) & model$n.tot == 0) ),]  # days we imputed something -- observed fish will change
  modelI <- unique(modelI[,c('batchDate','n.totDay','nEstDay','halfConeY')])
  rownames(modelI) <- NULL

  
  # --- now, make the plot. ---
  
  #   If file=NA, a pdf graphing device is assumed to be open already.
  if( !is.na(file) ){
    #   Shut down all graphics devices
    for(i in dev.list()) dev.off(i)
    
    #   ---- Open PNG device
    out.pass.graphs <- paste0(output.file,"_",df0$TrapPosition[1],"_",df0$FinalRun[1],"_spline.png")
    if(file.exists(out.pass.graphs)){
      file.remove(out.pass.graphs)
    }
    tryCatch({png(file=out.pass.graphs,width=14,height=7,units="in",res=600)}, error=function(x){png(file=out.pass.graphs)})  # produces hi-res graphs unless there's an error, then uses default png settings
  }

  # set graphical bounds
  y0 <- 0
  y1 <- max(modelA$theP,modelB$n.tot,modelC$nEstDay)
  x0 <- min(na.omit(model$batchDate))
  x1 <- max(na.omit(model$batchDate))

  # natural scale plot -- make an empty plot/palette
  plot(1,1,xaxt='n',yaxt='n',xlab='',ylab='',xlim=c(x0,x1),ylim=c(y0,y1))

  # draw the vertical segments
  if(nrow(modelI) > 0){
    for(j in 1:nrow(modelI)){
      jrow <- modelI[j,]
      if(is.na(modelI[j,]$halfConeY)){    # draw gray segments for full-cone operations
        graycols <- gray(seq(0.75,0.25,length.out=255)) #floor(jrow$nEstDay - jrow$n.totDay) ))
        bit <- (jrow$nEstDay - jrow$n.totDay) / length(graycols)
        for(i in 1:length(graycols)){
          segments(x0=jrow$batchDate,y0=jrow$n.totDay + ((i - 1)*bit),x1=jrow$batchDate,y1=jrow$n.totDay + (i*bit),col=graycols[i])
        }
      } else {                            # draw red segments for half-cone operations
        redcols <- colorRampPalette(c("lightpink","red2"))(255)#(floor(jrow$nEstDay - jrow$n.totDay))
        bit <- (jrow$nEstDay - jrow$n.totDay) / length(redcols)
        for(i in 1:length(redcols)){
          segments(x0=jrow$batchDate,y0=jrow$n.totDay + ((i - 1)*bit),x1=jrow$batchDate,y1=jrow$n.totDay + (i*bit),col=redcols[i])
        }
      }
    }
  }

  # draw the exponentiated spline curve
  par(new=TRUE)
  plot(modelA$EndTime,modelA$theP,xaxt='n',xlab='Date',ylab='Caught Fish',type='l',col='blue',xlim=c(x0,x1),ylim=c(y0,y1),lwd=3)

  # draw the dots of observed and imputed catch
  for(j in 1:nrow(model)){                                                 # draw the half-cone catch
    if(!is.na(model[j,]$halfConeY) & is.na(model[j,]$halfConeN)){
      par(new=TRUE)
      plot(model[j,]$batchDate,model[j,]$n.totDay,xaxt='n',yaxt='n',xlab='',ylab='',type='p',col='lightpink',xlim=c(x0,x1),ylim=c(y0,y1),pch=16,cex=0.65)
      par(new=TRUE)
      if(model[j,]$n.totDay == model[j,]$nEstDay){
        plot(model[j,]$batchDate,model[j,]$nEstDay ,xaxt='n',yaxt='n',xlab='',ylab='',type='p',col='red2'     ,xlim=c(x0,x1),ylim=c(y0,y1),pch=16,cex=0.65)
      } else {
        plot(model[j,]$batchDate,model[j,]$nEstDay ,xaxt='n',yaxt='n',xlab='',ylab='',type='p',col='red2'     ,xlim=c(x0,x1),ylim=c(y0,y1),pch=15,cex=0.65)
      }
    } else if(is.na(model[j,]$halfConeY) & !is.na(model[j,]$halfConeN)){   # draw the full-cone catch
      par(new=TRUE)
      plot(model[j,]$batchDate,model[j,]$n.totDay,xaxt='n',yaxt='n',xlab='',ylab='',type='p',col='gray'     ,xlim=c(x0,x1),ylim=c(y0,y1),pch=16,cex=0.65)
      par(new=TRUE)
      if(model[j,]$n.totDay == model[j,]$nEstDay){
        plot(model[j,]$batchDate,model[j,]$nEstDay ,xaxt='n',yaxt='n',xlab='',ylab='',type='p',col='black'     ,xlim=c(x0,x1),ylim=c(y0,y1),pch=16,cex=0.65)
      } else {
        plot(model[j,]$batchDate,model[j,]$nEstDay ,xaxt='n',yaxt='n',xlab='',ylab='',type='p',col='black'     ,xlim=c(x0,x1),ylim=c(y0,y1),pch=15,cex=0.65)
      }
    } else if(is.na(model[j,]$halfConeY) & is.na(model[j,]$halfConeN)){    # draw the fully imputed
      par(new=TRUE)
      plot(as.POSIXct(model[j,]$batchDateC,tz=time.zone),model[j,]$nEstDay ,xaxt='n',yaxt='n',xlab='',ylab='',type='p',col='blue' ,xlim=c(x0,x1),ylim=c(y0,y1),pch=22,cex=0.75,bg="white")
    } else if(!is.na(model[j,]$halfConeY) & !is.na(model[j,]$halfConeN)){  # draw the half-cone and full-cone catch on the same day (as half-cone)
      par(new=TRUE)
      plot(model[j,]$batchDate,model[j,]$n.totDay,xaxt='n',yaxt='n',xlab='',ylab='',type='p',col='lightpink',xlim=c(x0,x1),ylim=c(y0,y1),pch=16,cex=0.65)
      par(new=TRUE)
      if(model[j,]$n.totDay == model[j,]$nEstDay){
        plot(model[j,]$batchDate,model[j,]$nEstDay ,xaxt='n',yaxt='n',xlab='',ylab='',type='p',col='red2'     ,xlim=c(x0,x1),ylim=c(y0,y1),pch=16,cex=0.65)
      } else {
        plot(model[j,]$batchDate,model[j,]$nEstDay ,xaxt='n',yaxt='n',xlab='',ylab='',type='p',col='red2'     ,xlim=c(x0,x1),ylim=c(y0,y1),pch=15,cex=0.65)
      }
    }
  }

  # put in x-axis tick and labels
  lab.x.at <- pretty(seq(x0,x1,by=24*60*60) + 12*60*60,n=length(seq(x0,x1,by=24*60*60))/14)
  axis(side=1, at=lab.x.at, label=format(lab.x.at, "%d%b%y"),cex.axis=0.75)#""))

  # put in a header
  mtext( side=3, at=max(df0$batchDate), text=paste0(df2$siteName[1]," - ",df2$TrapPosition[1]), adj=1, cex=1.5, line=2 )
  if(df2$lifeStage[1] == "All"){
    lsLabel <- "All lifestages"
  } else {
    lsLabel <- df2$lifeStage[1]
  }
  mtext( side=3, at=max(df0$batchDate), text=paste("Chinook Salmon", ", ", df0$FinalRun[1], " run, ", lsLabel, sep=""), adj=1, cex=.75, line=1 )

  # make the legends
  #               Full Cone (1st legend)                                  Half Cone (2nd legend)                                        Other Stuff (3rd legend)
  leg.txt <- list(c("Obs Catch Only","Obs - Imp Catch","Obs + Imp Catch"),c("Obs Catch Only","Obs Catch - Imp Catch","Obs + Imp Catch"),c("Exp. Spline","Imp Catch Only"))
  leg.pch <- list(c(16,16,15)                                            ,c(16,16,15)                                                  ,c(NA,22)                         )
  leg.col <- list(c("black","gray","black")                              ,c("red2","lightpink","red2")                                 ,c("blue","blue")                 )
  leg.tit <- list(c("Full Cone")                                         ,"Half Cone"                                                  ,NA                               )
  leg.bgs <- list(c("")                                                  ,c("")                                                        ,c(NA,"white")                    )
  leg.lwd <- list(NA                                                     ,NA                                                           ,c(2,NA )                         )
  leg.bty <- list("n"                                                    ,"n"                                                          ,c("n","n")                       )
  leg.cex <- list(0.65                                                   ,0.65                                                         ,0.65                             )

  tmp <- legend( "topright", title=leg.tit[[1]], legend=leg.txt[[1]], pch=leg.pch[[1]], plot=FALSE ) # don't plot, need the left coordinate here
  tmp$rect$top <- tmp$rect$top + tmp$rect$h
  
  for( i in c(3,1,2)){
    tmp <- legend( c(tmp$rect$left,tmp$rect$left + tmp$rect$w), c(tmp$rect$top - tmp$rect$h, tmp$rect$top - 2*tmp$rect$h),
                   title=leg.tit[[i]],legend=leg.txt[[i]],pch=leg.pch[[i]],col=leg.col[[i]],lwd=leg.lwd[[i]],bty=leg.bty[[i]],cex=leg.cex[[i]])
  }
  
  dev.off()  
  # ---- end plotting ----

       
  # ---- compile graphing statistics and data ----------------------    
  jBaseTable <- unique(model[,c('batchDate','n.totDay','nImp','nEstDay')])
  #write.csv(jBaseTable,'C:/Users/jmitchell/Desktop/jBaseTable.csv')
  jBaseTable$siteID <- rep(df0$TrapPosition[1],nrow(jBaseTable))
  jBaseTable$siteName <- rep(df0$siteName[1],nrow(jBaseTable))
  names(jBaseTable)[names(jBaseTable) == 'n.totDay'] <- 'preCatch'
  names(jBaseTable)[names(jBaseTable) == 'nImp'] <- 'imputedCatch'
  names(jBaseTable)[names(jBaseTable) == 'nEstDay'] <- 'totalCatch'
#        
#   sum(na.omit(jBaseTable$preCatch))                 # assignedCatch + unassignedCatch
#   sum(na.omit(as.vector(jBaseTable$imputedCatch)))  # imputedCatch
#   sum(na.omit(jBaseTable$totalCatch))               # totalCatch
  
#   sum(na.omit(allJBaseTable$preCatch))                 # assignedCatch + unassignedCatch
#   sum(na.omit(as.vector(allJBaseTable$imputedCatch)))  # imputedCatch
#   sum(na.omit(allJBaseTable$totalCatch))               # totalCatch 
  # tapply(allJBaseTable[!is.na(allJBaseTable$preCatch),]$preCatch,allJBaseTable[!is.na(allJBaseTable$preCatch),]$siteID,FUN=sum)
  
#   # log scale
#   model2 <- model[model$n.tot != 0,]
#   plot(model2$EndTime,model2$logTheP,type='l',col='red',ylim=c(min(model2$logTheP,model2$logN.tot),max(model2$logTheP,model2$logN.tot)))
#   par(new=TRUE)
#   plot(model2$EndTime,model2$logN.tot,type='p',pch=19,cex=0.7,col='black',ylim=c(min(model2$logTheP,model2$logN.tot),max(model2$logTheP,model2$logN.tot)))
  jBaseTable 
}
