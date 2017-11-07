
#' @param hourlyQ A dataframe housing hourly flow data resulting from a query
#'   against the Environmental Covariate Database.
#'   
#' @param obs.eff.df A dataframe housing water velocity data obtained on days on
#'   which biologists visited a particular trap.
#' 
#' 
percQ(hourlyQ,){

  
  
  

  
  
  #   ---- DENOMINATOR.  
  
  #   ---- Obtain daily averaged flow, 8 AM - 7 PM of the previous day.  df[[2]] always has this for the 
  #   ---- Sacramento by design (via the xwalk dataframe).  
  hourlyQ <- df[[2]][,c("date","flow_cfs")]
  
  #   ---- Shift times by 8 hours.  This places 8 AM at midnight.  I ignore the fact that Daylight 
  #   ---- Savings screws with this for some months.  
  hourlyQ$hour8 <- as.POSIXlt(strptime(hourlyQ$date + (24 - 8)*60*60,format="%Y-%m-%d %H:%M:%S"),format="%Y-%m-%d %H:%M:%S",tz="Europe/London")
  
  #   ---- Get rid of the first 8 hours that are useless now.  
  hourlyQ <- hourlyQ[9:nrow(hourlyQ),]          
  
  #   ---- Remove annoying NA.  
  hourlyQ <- hourlyQ[!is.na(hourlyQ$flow_cfs),]  
  
  #   ---- Make a character date for use in aggregate.  
  hourlyQ$batchDateC <- format(hourlyQ$hour8,format="%Y-%m-%d")
  
  #   ---- Average over our 8-hour-shifted day.  
  Q <- aggregate(hourlyQ$flow_cfs,list(batchDateC=hourlyQ$batchDateC),function(x) mean(x))
  Q$batchDate <- as.POSIXlt(Q$batchDateC,format="%Y-%m-%d",tz="UTC")
  Q$batchDateC <- NULL
  names(Q)[names(Q) == "x"] <- "flow_cfs"
  Q <- Q[,c("batchDate","flow_cfs")]
  Q$batchDate <- as.POSIXct(Q$batchDate)
  

  
  #   ---- NUMERATOR
  
  #   ---- Restrict to biologist-obtain data.  
  numTrap <- obs.eff.df[!is.na(obs.eff.df$waterVel_fts),c("TrapPositionID","batchDate","waterVel_fts")]
  
  #   ---- Water velocity (ft/s) * 24 (ft^2) * 1 (assumed full-cone) * 43,560 (1 acre / ft^2) * 86,400 (s)
  #   ---- Sum over all the traps working that day.  
  numTrap$qid <- numTrap$waterVel_fts*24.6*1/43560*86400
  q <- aggregate(numTrap[,c("qid")],list(batchDate=numTrap$batchDate),function(x) sum(x))
  names(q)[names(q) == "x"] <- "q"

  
  
  #   ---- OTHER
  
  #   ---- Count the number of traps working each day.  
  count <- aggregate(numTrap[,c("qid")],list(batchDate=numTrap$batchDate),function(x) length(x))
  names(count)[names(count) == "x"] <- "nTraps"
  
  
  
  #   ---- A straight-up merge won't work on these POSIX dates, because I shifted the Q set by 8 hours.  
  #   ---- So, convert to Date, and then put back to POSIX.  
  Q$batchDateC <- format(Q$batchDate)
  q$batchDateC <- format(q$batchDate)

  #   ---- Do the merge.  
  qQ <- merge(q,Q,by=c("batchDateC"),all.x=TRUE)
  
  #   ---- Do some cleanup.  
  qQ$batchDate.x <- qQ$batchDateC <- NULL
  names(qQ)[names(qQ) == "batchDate.y"] <- "batchDate"
  qQ$batchDate <- as.POSIXlt(qQ$batchDate)
  
  #   ---- Account for the two little diversions.  First, identify dates between 5/15 and 11/15.
  qQ$divSeason <- NA
  qQ[  qQ$batchDate$year %in% c(92,96,100,104,108,112,116,120,124,128,132) ,]$divSeason <- ifelse(qQ[qQ$batchDate$year %in% c(92,96,100,104,108,112,116,120,124,128,132),]$batchDate$yday >= 135 
                                                                                                  & qQ[qQ$batchDate$year %in% c(92,96,100,104,108,112,116,120,124,128,132),]$batchDate$yday <= 319,1,0)   # leap years
  qQ[!(qQ$batchDate$year %in% c(92,96,100,104,108,112,116,120,124,128,132)),]$divSeason <- ifelse(qQ[!(qQ$batchDate$year %in% c(92,96,100,104,108,112,116,120,124,128,132)),]$batchDate$yday >= 134 
                                                                                                  & qQ[!(qQ$batchDate$year %in% c(92,96,100,104,108,112,116,120,124,128,132)),]$batchDate$yday <= 318,1,0)   # non leap years
  #   ---- Impute an average flow for the two diversions.  
  #   ---- Diversion 1 = Colusa Canal 
  #   ---- Diversion 2 = Tehama Canal
  qQ$Colusa <- qQ$Tehama <- 0
  qQ[qQ$divSeason == 1 & !is.na(qQ$divSeason),]$Colusa <- 53.15
  qQ[qQ$divSeason == 1 & !is.na(qQ$divSeason),]$Tehama <- 609.84

  #   ---- Account for the flow from the diversions.  (This is minus flow.)
  qQ$TotalFlow <- qQ$flow_cfs - qQ$Colusa - qQ$Tehama
  
  #   ---- Multiply by (86,400 s) and multiply by (1 acre / 43,560 ft^2).
  qQ$TotalQ <- qQ$TotalFlow*86400/43560
  
  #   ---- Calculate percent Q.  
  qQ$percQ <- qQ$q / qQ$TotalQ
  
  #   ---- Non-list POSIX date.
  qQ$batchDate2 <- as.POSIXct(qQ$batchDate)
  
  #   ---- Clean up.  
  qQ <- qQ[,c("batchDate","batchDate2","divSeason","q","flow_cfs","Colusa","Tehama","TotalFlow","TotalQ","percQ")]
  
  
  
  
  
  
  
  #   ---- Read in in Felipe's gold standard.  
  felipe <- read.csv("L:/PSMFC_CampRST/Workspaces/Felipe.csv",stringsAsFactors=FALSE)
  names(felipe)[names(felipe) == "IDDate"] <- "batchDate"
  names(felipe)[names(felipe) == "SumOfAcre_feet"] <- "qf"
  names(felipe)[names(felipe) == "AvgOfQ_SID"] <- "Qf"
  names(felipe)[names(felipe) == "Perc_Q"] <- "percQf"
  felipe <- felipe[,names(felipe)[!(grepl("X",names(felipe)))]]
  felipe$batchDate <- as.POSIXct(strptime(felipe$batchDate,format="%m/%d/%Y"),format="%Y-%m-%d",tz="UTC")
  felipe <- felipe[order(felipe$batchDate),]
  felipe <- felipe[!is.na(felipe$percQf),]
  
  
  #   ---- Bring both together so we can see how they compare.  
  both <- merge(qQ,felipe,by.x=c("batchDate2"),by.y=c("batchDate"))
  
  both$qDiff <- both$qf - both$q
  both$QDiff <- both$Qf - both$TotalQ
  both$percQDiff <- both$percQf - both$percQ
  
  #percQlm <- gls(percQf ~ batchDate,correlation=corAR1(0.5,form=~1),data=felipe)
  #yhat <- predict(percQlm)
  
  xmq <- min(felipe$batchDate)
  xMq <- max(felipe$batchDate)
  ymq <- min(felipe$qf)
  yMq <- max(felipe$qf)
  
  xmQ <- min(felipe$batchDate)
  xMQ <- max(felipe$batchDate)
  ymQ <- min(felipe$Qf)
  yMQ <- max(felipe$Qf)
  
  xmpQ <- min(felipe$batchDate)
  xMpQ <- max(felipe$batchDate)
  ympQ <- min(felipe$percQf)
  yMpQ <- max(felipe$percQf)
  
  
  
  par(mfrow=c(3,3))
  
  #   ---- Felipe numerator.
  plot(felipe$batchDate,felipe$qf,pch=19,cex=0.5,xlim=c(xm,xM),ylim=c(ymq,yMq))
  
  #   ---- Jason numerator.  
  plot(qQ$batchDate2,qQ$q,pch=19,col="blue",cex=0.5,xlim=c(xm,xM),ylim=c(ymq,yMq))
  
  #   ---- Numerator compare.
  plot(both$batchDate2,both$qDiff,pch=19,col="red",cex=0.5,xlim=c(xm,xM),ylim=c(-500,500))
  
  #   ---- Felipe denominator.  
  plot(felipe$batchDate,felipe$Qf,pch=19,cex=0.5,xlim=c(xm,xM),ylim=c(ymQ,yMQ))
  
  #   ---- Jason denominator
  plot(qQ$batchDate2,qQ$TotalQ,pch=19,col="blue",cex=0.5,xlim=c(xm,xM),ylim=c(ymQ,yMQ))
  
  #   ---- Denominator compare.
  plot(both$batchDate2,both$QDiff,pch=19,col="red",cex=0.5,xlim=c(xm,xM),ylim=c(-50000,50000))
  
  #   ---- Felipe percQ.
  plot(felipe$batchDate,felipe$percQf,pch=19,cex=0.5,xlim=c(xm,xM),ylim=c(ympQ,yMpQ))
  
  #   ---- Jason percQ
  plot(qQ$batchDate2,qQ$percQ,pch=19,col="blue",cex=0.5,xlim=c(xm,xM),ylim=c(ympQ,yMpQ))
  
  #   ---- Numerator compare.
  plot(both$batchDate2,both$percQDiff,pch=19,col="red",cex=0.5,xlim=c(xm,xM),ylim=c(-0.02,0.02))
  
  par(mfrow=c(1,1))
  
  
  plot(felipe$batchDate,felipe$percQf,xlim=c(xm,xM),ylim=c(ym,yM))
  par(new=TRUE)
  plot(felipe$batchDate,yhat,type="l",col="red",xlim=c(xm,xM),ylim=c(ym,yM))
  
  par(new=TRUE)
  plot(qQ$batchDate,qQ$percQ,pch=19,col="blue",cex=0.5,xlim=c(xm,xM),ylim=c(ym,yM))
  
  
  
  
  
  
  
  
  # write.csv(qQ,"C:/Users/jmitchell/Desktop/qQ.csv",row.names=FALSE)
  
  qQ[as.POSIXlt(qQ$batchDate)$year == 112 & as.POSIXlt(qQ$batchDate)$mon == 8 & !is.na(qQ$batchDate),]
  
  
  save.image("L:/PSMFC_CampRST/Workspaces/PercQ.Rdata")
  
  
}