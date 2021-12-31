#' @export
#' 
#' @title percQ
#'   
#' @description Estimate per-trap percent-Q, given hourly flow data and recorded
#'   water velocity from a CAMP Access database.
#'   
#' @param hrflow A dataframe housing hourly flow data resulting from a query 
#'   against the Environmental Covariate Database.
#'   
#' @return A data frame that mimics the structure of the other covariates. 
#'   Columns include \code{subSiteID}, \code{measureDate}, \code{percQ}, and
#'   \code{percQUnitID}, which is set to \code{99}.
#'   
#' @details Calculations for \code{percQ} utilize denominators obtained via the 
#'   Big Bend gauge station housed within the Environmental Covariate Database. 
#'   These numbers include an adjustment for the two canals that often divert 
#'   flow away from the Sacramento each summer.  In order to obtain an estimate 
#'   for these diverting flows, the daily means, as measured annually over 11
#'   years from 2002 to 2013, was subtracted from the values stored in the
#'   Environmental Covariate Database.  Package \code{campR} data frame
#'   \code{canal} houses these averages;  submit \code{data(canal)} to see them
#'   after loading the \code{campR} package.
#'   
#'   Numerators derive from velocities stored within the RBDD CAMP Access
#'   database.
#'   
#'   See the \code{"campR"} \code{"percQ"} vignette for more information on the
#'   derivation of percent-Q.
#'   
#' @seealso \code{data(canal)}, vignette
#'   
#' @examples  
#' \dontrun{
#' ans <- percQ(hrflow))
#' }
percQ <- function(hrflow){

  # hrflow <- df[[2]]

  #   ---- DENOMINATOR.  
  
  #   ---- Obtain daily averaged flow, 8 AM - 7 PM of the previous day.  df[[2]] always has this for the 
  #   ---- Sacramento by design (via the xwalk dataframe).  
  hourlyQ <- hrflow[,c("date","flow_cfs")]
  
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
  env <- sqlQuery(ch,paste0("SELECT subSiteID AS trapPositionID,
                                    trapVisitID,
                                    measureDate,
                                    waterVel
                             FROM EnvDataRaw_Standardized
                             WHERE waterVel IS NOT NULL
                             ORDER BY subSiteID,measureDate;"))

  #   ---- Get trap information.  Need this for cone depths.  
  vis <- sqlQuery(ch,paste0("SELECT trapVisitID,
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

  #   ---- Bring together the data.  
  numTest <- merge(env,vis,by=c("trapVisitID"),all.x=TRUE)
  
  #   ---- Restrict to biologist-obtained data.  
  num <- numTest[,c("measureDate","visitTime","trapVisitID","trapPositionID","waterVel","coneDepthAtStart","halfConeID")]

  #   ---- Take care of NAs and other weird values.  
  if( any(is.na(num$coneDepthAtStart) | !(num$coneDepthAtStart %in% seq(45,52,1))) ){
    num[is.na(num$coneDepthAtStart) | !(num$coneDepthAtStart %in% seq(45,52,1)),]$coneDepthAtStart <- 48
  }
  
  #   ---- Average waterVel over days.
  num$batchDate <- as.POSIXct(strptime(num$measureDate,format="%Y-%m-%d"),format="%Y-%m-%d",tz="UTC") 
  
  #   ---- Adjust for varying cross-sectional area.  
  num$crossArea <- 24.6
  if(sum(num$coneDepthAtStart == 45) > 0) num[num$coneDepthAtStart == 45,]$crossArea <- 22.7
  if(sum(num$coneDepthAtStart == 46) > 0) num[num$coneDepthAtStart == 46,]$crossArea <- 23.3
  if(sum(num$coneDepthAtStart == 47) > 0) num[num$coneDepthAtStart == 47,]$crossArea <- 24.0
  if(sum(num$coneDepthAtStart == 48) > 0) num[num$coneDepthAtStart == 48,]$crossArea <- 24.6
  if(sum(num$coneDepthAtStart == 49) > 0) num[num$coneDepthAtStart == 49,]$crossArea <- 25.3
  if(sum(num$coneDepthAtStart == 50) > 0) num[num$coneDepthAtStart == 50,]$crossArea <- 26.0
  if(sum(num$coneDepthAtStart == 51) > 0) num[num$coneDepthAtStart == 51,]$crossArea <- 26.6
  if(sum(num$coneDepthAtStart == 52) > 0) num[num$coneDepthAtStart == 52,]$crossArea <- 24.6
  
  #   ---- Adjust for halfCone. 
  num$halfConeMultiplier <- 1
  if(sum(num$halfConeID == 1 & !is.na(num$halfConeID)) > 0) num[num$halfConeID == 1 & !is.na(num$halfConeID),]$halfConeMultiplier <- 0.5

  #   ---- First, average over trap and day.
  waterVel <- aggregate(num$waterVel,list(trapPositionID=num$trapPositionID,batchDate=num$batchDate),function(x) mean(x))
  names(waterVel)[names(waterVel) == "x"] <- "waterVel"
  
  crossArea <- aggregate(num$crossArea,list(trapPositionID=num$trapPositionID,batchDate=num$batchDate),function(x) mean(x))
  names(crossArea)[names(crossArea) == "x"] <- "crossArea"
  
  halfConeMultiplier <- aggregate(num$halfConeMultiplier,list(trapPositionID=num$trapPositionID,batchDate=num$batchDate),function(x) mean(x))
  names(halfConeMultiplier)[names(halfConeMultiplier) == "x"] <- "halfConeMultiplier"

  reduced1 <- merge(waterVel,crossArea,by=c("trapPositionID","batchDate"))
  reduced2 <- merge(reduced1,halfConeMultiplier,by=c("trapPositionID","batchDate"))
  num2 <- num[,c("batchDate","trapPositionID")]
  
  num3 <- merge(reduced2,num2,by=c("batchDate","trapPositionID"),all.x=TRUE)
  num4 <- num3[!duplicated(num3),]
  
  #   ---- Water velocity (ft/s) * 24 (ft^2) * 1 (assumed full-cone) * 43,560 (1 acre / ft^2) * 86,400 (s)
  #   ---- Sum over all the traps working that day.  
  num4$qid <- num4$waterVel*num4$crossArea*num4$halfConeMultiplier*1/43560*86400
   
  #   ---- Sum over traps. 
  q <- aggregate(num4$qid,list(batchDate=num4$batchDate),function(x) sum(x))
  names(q)[names(q) == "x"] <- "q"

  
  #   ---- BRING IT TOGETHER.
   
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
  data(canal,envir=environment())
  canal <- canal[is.numeric(canal$average_cfs),]
  
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
  
  # #   ---- Now, fit a smoothing spline to these data, and then compare THOSE to Felipe -- maybe we 
  # #   ---- are close enough already.  
  # qQ <- qQ[!is.na(qQ$batchDate),]
  # percQsm <- smooth.spline(qQ$batchDate,qQ$percQ,lambda=0.0000000000000000001)   # <--- It seems I can only do so much here.
  # percQsm <- predict(percQsm)
  # percQsm <- data.frame(batchDate=percQsm$x,percQsm=percQsm$y)
  # percQsm$batchDate <- as.POSIXct(percQsm$batchDate,format="%Y-%m-%d",tz="UTC",origin="1970-01-01 UTC")
  # 
  # qQ <- merge(qQ,percQsm,by.x=c("batchDate2"),by.y=c("batchDate"),all.x=TRUE)
  
  #   ---- While we use qQ for checking, so as to try and match Felipe, we decided to use the per-trap flows.
  #   ---- Use num4 for the numerator qid (q flow on the ith d day).  Use data frame qQ to get acre-feet 
  #   ---- measurements for total flow, which also incorporates the diversions.  
  qQ2 <- qQ
  qQ2$batchDate <- NULL
  names(qQ2)[names(qQ2) == "batchDate2"] <- "batchDate"
  westPercQ <- merge(num4,qQ2,by=c("batchDate"),all.x=TRUE)
  westPercQ <- westPercQ[order(westPercQ$trapPositionID,westPercQ$batchDate),]
  westPercQ$percQWest <- westPercQ$qid / westPercQ$TotalQ
  names(westPercQ)[names(westPercQ) == "trapPositionID"] <- "TrapPositionID"

  dbperQ <- data.frame(subSiteID=westPercQ$TrapPositionID,measureDate=westPercQ$batchDate,percQ=westPercQ$percQWest,percQUnitID=99)
  attr(dbperQ,"cov") <- "percQ"
  attr(dbperQ,"uniqueUnitID") <- 99
  


  
  # #   ---- CHECKING OTHE ORIGINAL.
  # 
  # #   ---- Read in in Felipe's gold standard.
  # felipe <- read.csv("L:/PSMFC_CampRST/Workspaces/Felipe.csv",stringsAsFactors=FALSE)
  # names(felipe)[names(felipe) == "IDDate"] <- "batchDate"
  # names(felipe)[names(felipe) == "SumOfAcre_feet"] <- "qf"
  # names(felipe)[names(felipe) == "AvgOfQ_SID"] <- "Qf"
  # names(felipe)[names(felipe) == "Perc_Q"] <- "percQf"
  # felipe <- felipe[,names(felipe)[!(grepl("X",names(felipe)))]]
  # felipe$batchDate <- as.POSIXct(strptime(felipe$batchDate,format="%m/%d/%Y"),format="%Y-%m-%d",tz="UTC")
  # felipe <- felipe[order(felipe$batchDate),]
  # felipe <- felipe[!is.na(felipe$percQf),]
  # 
  # #   ---- Bring both together so we can see how they compare.
  # both <- merge(qQ,felipe,by.x=c("batchDate2"),by.y=c("batchDate"))
  # 
  # both$qDiff <- both$qf - both$q
  # both$QDiff <- both$Qf - both$TotalQ
  # both$percQDiff <- both$percQf - both$percQ
  # 
  # #   ---- Set graphical parameters.
  # xm <- min(felipe$batchDate)
  # xM <- max(felipe$batchDate)
  # 
  # ymq <- min(felipe$qf)
  # yMq <- max(felipe$qf)
  # 
  # ymQ <- min(felipe$Qf)
  # yMQ <- max(felipe$Qf)
  # 
  # ympQ <- min(felipe$percQf)
  # yMpQ <- max(felipe$percQf)
  # 
  # 
  # 
  # #   ---- Output.
  # png("L:/PSMFC_CampRST/felipe products/percQ/smooth.png",res=400,units="in",width=36,height=3)
  # 
  #   #par(mfrow=c(3,3))
  #   
  #   #   ---- Felipe percQ.
  #   plot(felipe$batchDate,felipe$percQf,pch=19,col="red",cex=0.5,xlim=c(xm,xM),ylim=c(ympQ,yMpQ),type="l")
  #   
  #   #   ---- Jason percQ.
  #   lines(qQ$batchDate2,qQ$percQ,pch=19,col="blue",cex=0.5,xlim=c(xm,xM),ylim=c(ympQ,yMpQ),type="l")
  #   
  #   #   ---- Smoothing percQ. 
  #   lines(qQ$batchDate2,qQ$percQsm,pch=19,col="green",cex=0.5,xlim=c(xm,xM),ylim=c(ympQ,yMpQ),type="l")
  #   
  #   #par(mfrow=c(1,1))
  #   
  # dev.off()
  # 
  # 
  # #   ---- Output.
  # png("L:/PSMFC_CampRST/felipe products/percQ/compare.png",res=400,units="in",width=60,height=6)
  # 
  #   par(mfrow=c(3,3))
  # 
  #   #   ---- Felipe numerator.
  #   plot(felipe$batchDate,felipe$qf,pch=19,cex=0.5,xlim=c(xm,xM),ylim=c(ymq,yMq))
  # 
  #   #   ---- Jason numerator.
  #   plot(qQ$batchDate2,qQ$q,pch=19,col="blue",cex=0.5,xlim=c(xm,xM),ylim=c(ymq,yMq))
  # 
  #   #   ---- Numerator compare.
  #   plot(both$batchDate2,both$qDiff,pch=19,col="red",cex=0.5,xlim=c(xm,xM),ylim=c(-500,500),type="l")
  # 
  #   #   ---- Felipe denominator.
  #   plot(felipe$batchDate,felipe$Qf,pch=19,cex=0.5,xlim=c(xm,xM),ylim=c(ymQ,yMQ))
  # 
  #   #   ---- Jason denominator
  #   plot(qQ$batchDate2,qQ$TotalQ,pch=19,col="blue",cex=0.5,xlim=c(xm,xM),ylim=c(ymQ,yMQ))
  # 
  #   #   ---- Denominator compare.
  #   plot(both$batchDate2,both$QDiff,pch=19,col="red",cex=0.5,xlim=c(xm,xM),ylim=c(-50000,50000),type="l")
  # 
  #   #   ---- Felipe percQ.
  #   plot(felipe$batchDate,felipe$percQf,pch=19,cex=0.5,xlim=c(xm,xM),ylim=c(ympQ,yMpQ))
  # 
  #   #   ---- Jason percQ
  #   plot(qQ$batchDate2,qQ$percQ,pch=19,col="blue",cex=0.5,xlim=c(xm,xM),ylim=c(ympQ,yMpQ))
  # 
  #   #   ---- Numerator compare.
  #   plot(both$batchDate2,both$percQDiff,pch=19,col="red",cex=0.5,xlim=c(xm,xM),ylim=c(-0.02,0.02),type="l")
  # 
  #   par(mfrow=c(1,1))
  # 
  # dev.off()
 
  return(dbperQ)
  
}