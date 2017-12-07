#' @export
#' 
#' @title buildAstroStats
#'   
#' @description Given a particular trap sequence, calculate the proportion of
#'   time spent fishing during nighttime and while the moon was up.
#' 
#' @param release.visit A data frame containing release data obtained from the
#'   release-data SQL sequence.
#'   
#' @param visit.df A data frame containing visits obtained from the catch-data
#'   SQL sequence.
#'   
#' @return A dataframe with one row for all unique \code{batchDates} included 
#'   within \code{min.date} and \code{max.date}, inclusive.  All variables, as 
#'   provided via \code{varVec}, have a non-\code{NA} value.
#'   
#' @details The \code{buildAstronStats} function first queries the CAMP database
#'   for all relevant visits via the \code{Visits} table, as well as the 
#'   astronomical sun and moon rise and set times, contained within table 
#'   \code{Dates}.  The "Not Fishing" data are also obtained, via a query to 
#'   table \code{TempSumUnmarkedByTrap_Run_Final} in the underlying CAMP 
#'   \code{mdb}.  Because of this, the catch SQL sequence must be run prior to 
#'   the same of the efficiency.
#'   
#'   The function emulates the calculation of \code{SampleMinutes} performed by 
#'   the catch SQL sequence.  This is done in order to calculate denominators 
#'   for calculation of the proportion of time spent fishing at night and while 
#'   the moon is up.  The \code{SampleMinutes} field from 
#'   \code{TempSumUnmarkedByTrap_Run_Final} was not utilized because the 
#'   astronomical metrics need to calculate fishing with respect to the included
#'   fishing instances in the calculation of efficiency.  These include fishing 
#'   instances regardless of the value of the \code{includeCatchID} field.  The 
#'   value of \code{SampleMinutes} in the \code{TempSumUnmarkedByTrap_Run_Final}
#'   calculated for catch estimation excludes bad fishing, as identified via the
#'   \code{includecatchID} field.  Therefore, re-estimation of 
#'   \code{SampleMinutes}, based on efficiency concerns, must be performed. 
#'   These \code{SampleMinutes} are called \code{JasonSampleMinutes}, so as to 
#'   differentiate. Values of this variable equals \code{-99} for the first 
#'   visit of a trapping sequence, and \code{-88} in the case of a time frame 
#'   greater than the gap in fishing length. Currently, the fishing-gap length 
#'   is set via global variable \code{fishingGapMinutes}, and equals 
#'   \code{10080} minutes.
#'   
#'   Calculation of astronomical proportions utilized \code{campR} functions 
#'   \code{makeSkinnyTimes} and \code{getTimeProp}.
#'   
#' @seealso  \code{makeSkinnyTimes}, \code{getTimeProp}
#'   
#' @author WEST Inc.
#'   
#' @examples
#' \dontrun{
#' df <- buildAstroStats(release.visit=release.visit,
#'                       visit.df=visit.df)
#' }
buildAstroStats <- function(release.visit,visit.df){
  
  # release.visit <- release.visit
  # visit.df <- visit.df
  
  
  #   ----- Define some convenient dates.  
  min.date2 <- "1990-01-01"
  max.date2 <- Sys.Date()
  
  #   ---- Grab the time zone. 
  time.zone <- get( "time.zone", envir=.GlobalEnv )
  
  #   ---- Open ODBC channel.
  db <- get( "db.file", envir=.GlobalEnv )
  ch <- odbcConnectAccess(db)
  
  # max.date.eff <- max(release.visit$ReleaseDate) + 30*24*60*60  # 30-day buffer after
  # min.date.eff <- min(release.visit$ReleaseDate) - 30*24*60*60  # 30-day buffer before
  
  max.date.eff <- as.POSIXct(max.date,format="%Y-%m-%d",tz="UTC") + 30*24*60*60
  min.date.eff <- as.POSIXct(min.date,format="%Y-%m-%d",tz="UTC") - 30*24*60*60
  
  #   ---- Get dates information for moon and sun info. 
  tblDates <- sqlQuery(ch,paste0("SELECT uniqueDate,
                                 nightLength,
                                 moonRise,
                                 moonSet,
                                 sunRise,
                                 sunSet
                                 FROM Dates 
                                 WHERE uniqueDate <= #",as.Date(max.date.eff),"# 
                                 AND uniqueDate >= #",as.Date(min.date.eff),"# 
                                 ORDER BY uniqueDate"))
  
  trapVisits <- sqlQuery(ch,paste0("SELECT trapVisitID,
                                   trapPositionID,
                                   visitTime,
                                   visitTime2,
                                   visitTypeID
                                   FROM trapVisit
                                   WHERE visitTime <= #",as.Date(max.date2),"# 
                                   AND visitTime >= #",as.Date(min.date2),"# 
                                   AND ( (visitTypeID < 5 AND fishProcessedID <> 2)
                                   OR   (visitTypeID = 1 AND fishProcessedID = 2) )
                                   ORDER BY trapPositionID,visitTime"))
  
  #   ---- Need to make sure this table exists first, when this code is finalized.  
  notFishing <- sqlQuery(ch,paste0("SELECT SampleDate,
                                   StartTime,
                                   EndTime,
                                   oldtrapPositionID AS TrapPositionID,
                                   SampleMinutes
                                   FROM TempSumUnmarkedByTrap_Run_Final 
                                   WHERE TrapStatus = 'Not Fishing'
                                   ORDER BY oldTrapPositionID, EndTime"))
  
  close(ch)
  
  
  #   ---- We compile metrics that we need to compile over trapping.  
  
  #   ---- Construct a trapVisit data frame of all visits.  
  tmp <- trapVisits
  #tmp$fishProcessedID <- NULL
  tmp$visitTime <- as.POSIXlt(strftime(trapVisits$visitTime),tz=time.zone)
  tmp$visitTime2 <- as.POSIXlt(strftime(trapVisits$visitTime2),tz=time.zone)
  
  #   ---- Bring in the mean forkLengths. 
  fl <- attr(visit.df,"fl")

  
  #   ---- Some trapVisitIDs are not in the final catch table, for whatever reason.  Find these 
  #   ---- missing trap instances so they can be zeroed out.  These trap visits often are found 
  #   ---- in dfs when calculating efficiency.  We will put their fish at n of fish at 0.
  maxTrapVisitID <- max(trapVisits$trapVisitID)
  trapVisitIDSpine <- data.frame(trapVisitID=seq(1,maxTrapVisitID,1))
  fl0 <- merge(trapVisitIDSpine,fl,by=c("trapVisitID"),all.x=TRUE)
  fl0[is.na(fl0)] <- 0
  fl0 <- fl0[fl0$nForkLength == 0,]
  
  tmp <- merge(tmp,fl,by=c("trapVisitID"),all.x=TRUE)
  
  #   ---- Construct start and end times.  I also construct SampleMinutes...sometimes these differ from Connie's, by 60 
  #   ---- minutes.  This has to do with daylight savings.  I need POSIX to 'be dumb' with respect to daylight savings, 
  #   ---- to match Connie.  I wonder if Connie's SampleMinutes are off by 60 minutes?  I suspect the times recorded in 
  #   ---- the CAMP are 'raw' times, and so when one "springs forward" +60 minutes go along for the ride, and when one 
  #   ---- "falls back," CAMP loses 60 minutes.  This explains why spring-time SampleMinutes that I calculate are short 
  #   ---- by 60 minutes.  I don't think this is easily fixed.   
  tmp <- tmp[order(tmp$trapPositionID,tmp$visitTime),]
  
  #   ---- Apply the lag throughout.
  tmp$StartTime <- as.POSIXlt(strftime(c(as.POSIXlt(NA,tz=time.zone),strftime(tmp$visitTime[1:(nrow(tmp) - 1)],tz=time.zone))),tz=time.zone)
  
  #   ---- Identify where the lag logic is wrong.  These occur when the lag visitTime != lag visitTime2.  Adjust the StartTime to be correct. 
  tmp$StartTime <- ifelse(as.POSIXlt(strftime(c(as.POSIXlt(NA,tz=time.zone),strftime(tmp$visitTime[1:(nrow(tmp) - 1)],tz=time.zone))),tz=time.zone) !=
                            as.POSIXlt(strftime(c(as.POSIXlt(NA,tz=time.zone),strftime(tmp$visitTime2[1:(nrow(tmp) - 1)],tz=time.zone))),tz=time.zone),
                          as.POSIXct(strftime(c(as.POSIXlt(NA,tz=time.zone),strftime(tmp$visitTime2[1:(nrow(tmp) - 1)],tz=time.zone))),tz=time.zone),
                          as.POSIXct(tmp$StartTime))
  
  #   ---- So POSIX is awful with ifelse.  Or I'm not doing it right.  Regardless, put it to how we want it.  
  tmp$StartTime2 <- as.POSIXlt(tmp$StartTime,format="%Y-%m-%d %H:%M:%S",tz=time.zone,origin="1970-01-01 00:00:00 UTC")
  tmp$StartTime <- NULL
  names(tmp)[names(tmp) == "StartTime2"] <- "StartTime"
  
  #   ---- Apply the EndTime logic.  This is much easier.  
  tmp$EndTime <- tmp$visitTime2 
  tmp[tmp$visitTime != tmp$visitTime2,]$EndTime <- tmp[tmp$visitTime != tmp$visitTime2,]$visitTime
  
  tmp$SampleMinutes <- difftime(tmp$EndTime,tmp$StartTime,units="mins")
  
  #   ---- We need to be smart here.
  #   ---- Put the SampleMinutes for the first record for each trapPositionID to -99.
  tmp[tmp$trapPositionID != c(99,tmp$trapPositionID[1:(nrow(tmp) - 1)]),]$SampleMinutes <- -99
  
  #   ---- Put the SampleMinutes for a time frame greater than the gap in fishing length to -88.
  tmp[tmp$SampleMinutes > fishingGapMinutes,]$SampleMinutes <- -88
  
  tmp$uniqueDate <- NA  
  
  #   ---- Check where we can.  Note that catch.df isn't read in by the function. 
  # connieSM <- unique(catch.df[,c("oldtrapPositionID","trapVisitID","SampleMinutes")])
  # names(connieSM)[names(connieSM) == "oldtrapPositionID"] <- "trapPositionID"
  # names(connieSM)[names(connieSM) == "SampleMinutes"] <- "SampleMinutesC"
  # 
  # tmp2 <- merge(tmp,connieSM,by=c("trapPositionID","trapVisitID"),all.x=TRUE)
  # tmp2$Diff <- as.numeric(tmp2$SampleMinutes) - tmp2$SampleMinutesC
  # 
  # tmp2[tmp2$Diff != 0 & !is.na(tmp2$Diff),]
  # tmp2[tmp2$Diff != -60 & tmp2$Diff != 0 & !is.na(tmp2$Diff),]
  
  #   ---- Calculate the proportion of each trapVisitID experience sun or moon, depending.  
  traps <- unique(tmp$trapPositionID)
  
  sun <- makeSkinnyTimes("sunRise","sunSet",tblDates)
  tmp <- getTimeProp(sun,"sunRise","sunSet",traps,tmp,"sun")
  
  moon <- makeSkinnyTimes("moonRise","moonSet",tblDates)
  tmp <- getTimeProp(moon,"moonRise","moonSet",traps,tmp,"moon")
  
  #   ---- But, we really want proportion of night, and not day.
  tmp$nightMinutes <- NA
  tmp[!is.na(tmp$sunProp),]$nightMinutes <- as.numeric(tmp[!is.na(tmp$sunProp),]$SampleMinutes) - tmp[!is.na(tmp$sunProp),]$sunMinutes
  tmp$nightProp <- 1 - tmp$sunProp
  
  
  
  
  # tmp2 <- F.assign.batch.date(tmp)
  # 
  # tmp.sun <- aggregate(tmp2$sunMinutes,list(trapPositionID=tmp2$trapPositionID,batchDate=tmp2$batchDate), function(x) sum(x))
  # names(tmp.sun)[names(tmp.sun) == "x"] <- "sunMinutes"
  # 
  # tmp.moon <- aggregate(tmp2$moonMinutes,list(trapPositionID=tmp2$trapPositionID,batchDate=tmp2$batchDate), function(x) sum(x))
  # names(tmp.moon)[names(tmp.moon) == "x"] <- "moonMinutes"
  # 
  # tmp.night <- aggregate(tmp2$nightMinutes,list(trapPositionID=tmp2$trapPositionID,batchDate=tmp2$batchDate), function(x) sum(x))
  # names(tmp.night)[names(tmp.night) == "x"] <- "nightMinutes"
  # 
  # tmp.SampleMinutes <- aggregate(tmp2$SampleMinutes,list(trapPositionID=tmp2$trapPositionID,batchDate=tmp2$batchDate), function(x) sum(x))
  # names(tmp.SampleMinutes)[names(tmp.SampleMinutes) == "x"] <- "SampleMinutes"
  # 
  # 
  # one <- merge(tmp.sun,tmp.moon,by=c("batchDate","trapPositionID"))
  # two <- merge(one,tmp.night,by=c("batchDate","trapPositionID"))
  # thr <- merge(two,tmp.SampleMinutes,by=c("batchDate","trapPositionID"))
  # 
  # thr$nightProp <- thr$nightMinutes / thr$SampleMinutes
  # thr$moonProp <- thr$moonMinutes / thre$SampleMinutes
  # 
  # test <- thr[thr$trapPositionID == "57001",]
  
  forEffPlots <- tmp[,c("trapVisitID","trapPositionID","StartTime","EndTime","wmForkLength","nForkLength","nightProp","moonProp")]
  #forEffPlots <- forEffPlots[order(forEffPlots$trapPositionID,forEffPlots$EndTime),]
  
  #plot(forEffPlots$EndTime,forEffPlots$nightProp,col=c("red","orange","green","blue","black")[as.factor(forEffPlots$trapPositionID)],pch=19)
  #plot(forEffPlots$EndTime,forEffPlots$moonProp,col=c("red","orange","green","blue","black")[as.factor(forEffPlots$trapPositionID)],pch=19)
  
  #   ---- Rename to preserve, since 'tmp' is used below, and bring in the goodies.  Clean up tmp a bit so it merges in nicely, 
  #   ---- and doesn't reproduce data already present in release.visit.  
  tmpAstro <- tmp
  names(tmpAstro)[names(tmpAstro) == "SampleMinutes"] <- "JasonSampleMinutes"
  tmpAstro$trapPositionID <- NULL
  release.visit <- merge(release.visit,tmpAstro,by=c("trapVisitID"),all.x=TRUE)

  return(list(release.visit=release.visit,forEffPlots=forEffPlots,fl0=fl0))
  
}