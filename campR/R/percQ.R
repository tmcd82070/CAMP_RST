
#' @param hourlyQ A dataframe housing hourly flow data resulting from a query
#'   against the Environmental Covariate Database.
#'   
#' @param obs.eff.df A dataframe housing water velocity data obtained on days on
#'   which biologists visited a particular trap.
#' 
#' 
percQ(hourlyQ,){

  
  
  load("L:/PSMFC_CampRST/Workspaces/PercQ2.Rdata")

  # when you load, be sure to go back and re-read in db.file from Big_Looper2.0.
  
  
  
  
  
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
  
  
  #   ---- Open ODBC channel.
  db <- get( "db.file", envir=.GlobalEnv )
  ch <- odbcConnectAccess(db)
 
  #   ---- Get environmental data for water velocity and CAMP-recorded dates and times.  
  #   ---- We use the times to join.  
  # env <- sqlQuery(ch,paste0("SELECT subSiteID,
  #                                   measureTime,
  #                                   waterVel
  #                            FROM EnvDataRaw
  #                            WHERE waterVel IS NOT NULL
  #                              AND waterVelUnitID = 8
  #                            ORDER BY subSiteID,measureTime;"))

  vis <- sqlQuery(ch,paste0("SELECT trapVisitID,
                                    trapPositionID,
                                    visitTime,
                                    visitTime2,
                                    coneDepthAtStart,
                                    halfConeID,
                                    visitTypeID
                             FROM trapVisit
                             WHERE ( (visitTypeID < 5 AND fishProcessedID <> 2)
                                OR   (visitTypeID = 1 AND fishProcessedID  = 2) )
                             ORDER BY trapPositionID,visitTime"))
  close(ch)
  
  #   ---- Remove trap instances of "continue."
  vis <- vis[vis$visitTypeID != 2,]
  tmp <- vis
  
  #   ---- Merge together by day.  Stop-gap until I have trapVisitID data.  
  env$measureTimeDate <- as.Date(env$measureTime)
  vis$visitTimeDate <- as.Date(vis$visitTime)
  
  
  numTest <- merge(env,vis,by.x=c("measureTimeDate","subSiteID"),by.y=c("visitTimeDate","trapPositionID"),all.y=TRUE)
  
  # env[as.Date(env$measureTime) == "1997-09-30",]   # one row.
  # vis[as.Date(vis$visitTime) == "1997-09-30",]     # four rows. 
  # numTest[as.Date(numTest$measureTimeDate) == "1997-09-30",]
  
  

  
  
  

  
  # #   ---- Construct start and end times.  I also construct SampleMinutes...sometimes these differ from Connie's, by 60 
  # #   ---- minutes.  This has to do with daylight savings.  I need POSIX to 'be dumb' with respect to daylight savings, 
  # #   ---- to match Connie.  I wonder if Connie's SampleMinutes are off by 60 minutes?  I suspect the times recorded in 
  # #   ---- the CAMP are 'raw' times, and so when one "springs forward" +60 minutes go along for the ride, and when one 
  # #   ---- "falls back," CAMP loses 60 minutes.  This explains why spring-time SampleMinutes that I calculate are short 
  # #   ---- by 60 minutes.  I don't think this is easily fixed.   
  # tmp <- tmp[order(tmp$trapPositionID,tmp$visitTime),]
  # 
  # #   ---- Apply the lag throughout.
  # tmp$StartTime <- as.POSIXlt(strftime(c(as.POSIXlt(NA,tz="America/Los_Angeles"),strftime(tmp$visitTime[1:(nrow(tmp) - 1)],tz="America/Los_Angeles"))),tz="America/Los_Angeles")
  # 
  # #   ---- Identify where the lag logic is wrong.  These occur when the lag visitTime != lag visitTime2.  Adjust the StartTime to be correct. 
  # tmp$StartTime <- ifelse(as.POSIXlt(strftime(c(as.POSIXlt(NA,tz="America/Los_Angeles"),strftime(tmp$visitTime[1:(nrow(tmp) - 1)],tz="America/Los_Angeles"))),tz="America/Los_Angeles") !=
  #                           as.POSIXlt(strftime(c(as.POSIXlt(NA,tz="America/Los_Angeles"),strftime(tmp$visitTime2[1:(nrow(tmp) - 1)],tz="America/Los_Angeles"))),tz="America/Los_Angeles"),
  #                         as.POSIXct(strftime(c(as.POSIXlt(NA,tz="America/Los_Angeles"),strftime(tmp$visitTime2[1:(nrow(tmp) - 1)],tz="America/Los_Angeles"))),tz="America/Los_Angeles"),
  #                         as.POSIXct(tmp$StartTime))
  # 
  # #   ---- So POSIX is awful with ifelse.  Or I'm not doing it right.  Regardless, put it to how we want it.  
  # tmp$StartTime2 <- as.POSIXlt(tmp$StartTime,format="%Y-%m-%d %H:%M:%S",tz="America/Los_Angeles",origin="1970-01-01 00:00:00 UTC")
  # tmp$StartTime <- NULL
  # names(tmp)[names(tmp) == "StartTime2"] <- "StartTime"
  # 
  # 
  # 
  # 
  # 
  # 
  # 
  # 
  # #   ---- Apply the EndTime logic.  This is much easier.  
  # tmp$EndTime <- tmp$visitTime2 
  # tmp[tmp$visitTime != tmp$visitTime2,]$EndTime <- tmp[tmp$visitTime != tmp$visitTime2,]$visitTime
  # 
  # tmp$SampleMinutes <- difftime(tmp$EndTime,tmp$StartTime,units="mins")
  # 
  # #   ---- We need to be smart here.
  # #   ---- Put the SampleMinutes for the first record for each trapPositionID to -99.
  # tmp[tmp$trapPositionID != c(99,tmp$trapPositionID[1:(nrow(tmp) - 1)]),]$SampleMinutes <- -99
  # 
  # #   ---- Put the SampleMinutes for a time frame greater than the gap in fishing length to -88.
  # tmp[tmp$SampleMinutes > fishingGapMinutes,]$SampleMinutes <- -88
  # 
  # tmp$uniqueDate <- NA  
  # 
  # 
  # 
  # eg <- tmpDates
  # 
  # 
  # #   ---- Make a 'tblDates', like that used for the moon and sun proportion calculations.  But now, use start and ending of 
  # #   ---- trapping, based on 8 am start and end times.  
  # N <- length(seq.POSIXt(ISOdate(1990,1,1,hour=0,tz="GMT"),ISOdate(2100,12,31,hour=0,tz="GMT"),by="day"))
  # tblDates <- data.frame(uniqueDate=seq.POSIXt(ISOdate(1990,1,1,hour=0,tz="GMT"),ISOdate(2100,12,31,hour=0,tz="GMT"),by="day"),
  #                        nightLength=seq(1,N,1),
  #                        startTrapFor.q=seq(1,N,1),
  #                        endTrapFor.q=seq(1,N,1))
  # tblDates$uniqueDate <- as.POSIXlt(tblDate$uniqueDate)
  # tblDates$startTrapFor.q <- ISOdate(tblDates$uniqueDate$year + 1900,tblDates$uniqueDate$mon + 1,tblDates$uniqueDate$mday,hour=8,tz="GMT")
  # tblDates$endTrapFor.q <- tblDates$startTrapFor.q + 24*60*60
  # 
  # 
  # #   ---- Calculate the proportion of each trapVisitID experience sun or moon, depending.  
  # traps <- unique(tmp$trapPositionID)
  # 
  # qFishing2 <- rbind(data.frame(time=tblDates$startTrapFor.q,Event="startTrapFor.q",S=seq(1,N),stringsAsFactors=FALSE),
  #                    data.frame(time=tblDates$endTrapFor.q,Event="endTrapFor.q",S=seq(1,N),stringsAsFactors=FALSE))
  # qFishing2 <- qFishing2[order(qFishing2$S,qFishing2$time,qFishing2$Event),]
  # qFishing2$S <- NULL
  # qFishing2$trapVisitID <- NA
  # qFishing2$trapVisitID <- as.integer(qFishing$trapVisitID)
  # # qFishing2$charDate <- as.Date(qFishing2$time)
  # # qFishing2$time2 <- as.POSIXlt(strptime(qFishing2$charDate,format="%Y-%m-%d",tz="UTC"),tz="UTC")  # this is being super annoying.
  # qFishing2$time <- as.POSIXlt(qFishing2$time)
  # qFishing2$S <- seq(1,2*N)
  # # qFishing2$charDate <- NULL
  # 
  # 
  # #qFishing <- makeSkinnyTimes("startTrapFor.q","endTrapFor.q",tblDates)
  # tmp2 <- getTimeProp(qFishing,"startTrapFor.q","endTrapFor.q",traps,tmp,"fish.q")
  # 
  # 
  # 
  # tmp$dDate <- as.Date(tmp$visitTime,tz="America/Los_Angeles")
  # tmp[tmp$dDate == "2000-02-18" & tmp$trapPositionID == "42020",]
  # 
  # tmp2$dDate <- as.Date(tmp2$visitTime,tz="America/Los_Angeles")
  # tmp2[tmp2$dDate == "2000-02-18" & tmp2$trapPositionID == "42020",]
  
  
  
  
  
  
  
  
  #   ---- In numTest, subSiteID comes from Env table, and trapPositionID comes from visit table.
  
  #   ---- Restrict to biologist-obtained data.  
  num <- numTest[,c("measureTimeDate","visitTime","trapVisitID","subSiteID","measureTime","waterVel","coneDepthAtStart","halfConeID")]
  
  #   ---- Get rid of measureTime NAs.  These weren't found.  
  num <- num[!is.na(num$measureTime),]
  
  
  #numTest[as.Date(numTest$measureTimeDate) == "1997-09-30",]
  
  num[is.na(num$coneDepthAtStart) | !(num$coneDepthAtStart %in% seq(45,52,1)),]$coneDepthAtStart <- 48
  
  #   ---- Average waterVel over days.
  num$batchDate <- as.POSIXct(strptime(num$measureTime,format="%Y-%m-%d"),format="%Y-%m-%d",tz="UTC") 
  #names(num)[names(num) == "measureTimeDate"] <- "batchDate"
  

  num[as.Date(num$batchDate) == "1997-09-30",]
  
  
  #num <- num[num$subSiteID == num$trapPositionID,]
  
  
  #   ---- Adjust for varying cross-sectional area.  
  num$crossArea <- 24.6
  num[num$coneDepthAtStart == 45,]$crossArea <- 22.7
  num[num$coneDepthAtStart == 46,]$crossArea <- 23.3
  num[num$coneDepthAtStart == 47,]$crossArea <- 24.0
  num[num$coneDepthAtStart == 48,]$crossArea <- 24.6
  num[num$coneDepthAtStart == 49,]$crossArea <- 25.3
  num[num$coneDepthAtStart == 50,]$crossArea <- 26.0
  num[num$coneDepthAtStart == 51,]$crossArea <- 26.6
  num[num$coneDepthAtStart == 52,]$crossArea <- 24.6
  
  #   ---- Adjust for halfCone. 
  num$halfConeMultiplier <- 1
  num[num$halfConeID == 1 & !is.na(num$halfConeID),]$halfConeMultiplier <- 0.5

  #   ---- First, average over trap and day.
  waterVel <- aggregate(num$waterVel,list(subSiteID=num$subSiteID,batchDate=num$batchDate),function(x) mean(x))
  names(waterVel)[names(waterVel) == "x"] <- "waterVel"
  
  crossArea <- aggregate(num$crossArea,list(subSiteID=num$subSiteID,batchDate=num$batchDate),function(x) mean(x))
  names(crossArea)[names(crossArea) == "x"] <- "crossArea"
  
  halfConeMultiplier <- aggregate(num$halfConeMultiplier,list(subSiteID=num$subSiteID,batchDate=num$batchDate),function(x) mean(x))
  names(halfConeMultiplier)[names(halfConeMultiplier) == "x"] <- "halfConeMultiplier"

  reduced1 <- merge(waterVel,crossArea,by=c("subSiteID","batchDate"))
  reduced2 <- merge(reduced1,halfConeMultiplier,by=c("subSiteID","batchDate"))
  num2 <- num[,c("batchDate","subSiteID")]
  
  num3 <- merge(reduced2,num2,by=c("batchDate","subSiteID"),all.x=TRUE)
  num4 <- num3[!duplicated(num3),]
  
  #   ---- Water velocity (ft/s) * 24 (ft^2) * 1 (assumed full-cone) * 43,560 (1 acre / ft^2) * 86,400 (s)
  #   ---- Sum over all the traps working that day.  
  num4$qid <- num4$waterVel*num4$crossArea*num4$halfConeMultiplier*1/43560*86400
  
  
  #num4[as.Date(num4$batchDate) == "1998-10-16",]
  
  
  
  #   ---- Sum over traps. 
  q <- aggregate(num4$qid,list(batchDate=num4$batchDate),function(x) sum(x))
  names(q)[names(q) == "x"] <- "q"
  
  # q[as.Date(q$batchDate) == "1997-04-06",]
  # q[as.Date(q$batchDate) == "1998-10-16",]
  # q[as.Date(q$batchDate) == "1997-09-30",]
  
  q[as.Date(q$batchDate) %in% c("1997-09-29","1997-09-30","1997-09-30"),]
  num[as.Date(num$batchDate) %in% c("1997-09-29","1997-09-30","1997-09-30"),]
  
  
  
  
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
  
  #   ---- Account for the two little diversions.  Read in daily 11-year-average canal diversion data.  
  data(canal)

  qQ$monthDay <- paste0(qQ$batchDate$mon + 1,"-",qQ$batchDate$mday)
  qQ <- merge(qQ,canal,by=c("monthDay"),all.x=TRUE)
  qQ <- qQ[order(qQ$batchDate),]
  
  #   ---- Account for the flow from the diversions.  (This is minus flow.)
  qQ$TotalFlow <- qQ$flow_cfs - qQ$average
  qQ$monthDay <- qQ$monDay <- NULL
  
  #   ---- Multiply by (86,400 s) and multiply by (1 acre / 43,560 ft^2).
  qQ$TotalQ <- qQ$TotalFlow*86400/43560
  
  #   ---- Calculate percent Q.  
  qQ$percQ <- qQ$q / qQ$TotalQ
  
  #   ---- Non-list POSIX date.
  qQ$batchDate2 <- as.POSIXct(qQ$batchDate)
  
  #   ---- Clean up.  
  qQ <- qQ[,c("batchDate","batchDate2","q","flow_cfs","average_cfs","TotalFlow","TotalQ","percQ")]
  
  
  
  
  
  
  
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
  
  xm <- min(felipe$batchDate)
  xM <- max(felipe$batchDate)
  
  ymq <- min(felipe$qf)
  yMq <- max(felipe$qf)
  
  ymQ <- min(felipe$Qf)
  yMQ <- max(felipe$Qf)
  
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
  
  
  # plot(felipe$batchDate,felipe$percQf,xlim=c(xm,xM),ylim=c(ym,yM))
  # par(new=TRUE)
  # plot(felipe$batchDate,yhat,type="l",col="red",xlim=c(xm,xM),ylim=c(ym,yM))
  # 
  # par(new=TRUE)
  # plot(qQ$batchDate,qQ$percQ,pch=19,col="blue",cex=0.5,xlim=c(xm,xM),ylim=c(ym,yM))
  
  
  
  both[both$percQDiff < -0.02,]
  
  both[both$qDiff < -800,]
  
  both[as.Date(both$batchDate2) == "1997-09-30",]
  
  
  # write.csv(qQ,"C:/Users/jmitchell/Desktop/qQ.csv",row.names=FALSE)
  
  qQ[as.POSIXlt(qQ$batchDate)$year == 112 & as.POSIXlt(qQ$batchDate)$mon == 8 & !is.na(qQ$batchDate),]
  
  
  #save.image("L:/PSMFC_CampRST/Workspaces/PercQ2.Rdata")
  
  
}