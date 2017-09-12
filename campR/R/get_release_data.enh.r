#' @export
#' 
#' @title F.get.release.data 
#' 
#' @description Fetch data on efficiency trials from an Access database. 
#' 
#' @param site The identification number of the site for which estimates are
#'   required.
#' @param taxon The species identifier indicating the type of fish of interest. 
#'   This is always \code{161980}; i.e., Chinook Salmon.
#' @param min.date The start date for data to include. This is a text string in
#'   the format \code{\%Y-\%m-\%d}, or \code{YYYY-MM-DD}.
#' @param max.date The end date for data to include.  Same format as
#'   \code{min.date}.
#'   
#' @return A data frame summarizing efficiency trials for the site of interest
#'   for all traps between the dates indicated.  Data include \code{Recaps}
#'   numerators and \code{nReleased} denominators.
#'   
#' @details Function \code{F.get.release.data} utilizes query sequences Build 
#'   Report Criteria and Build Report Criteria Release to obtain all results 
#'   within the specified \code{min.date} and \code{max.date}.  See section
#'   Structured Query Language (SQL) Queries in function 
#'   \code{F.run.sqlFile} for more details.
#'   
#'   Query results include one record for every trap visit within a 
#'   \code{releaseTime}, say 7 days, even if the trap visit did not catch any 
#'   marked fish.  In this case, zeros are recorded for all combinations of 
#'   \code{releaseID}, \code{trapPositionID}, and \code{trapVisitID}.
#'   
#'   Given a specific release, the resulting data frame tells how many fish from
#'   the release were captured on subsequent trap visits, for each trap.  These 
#'   result from collapsing all trap visits and computing the total number of 
#'   each release's captured fish.  Note that generally, at any one time, more
#'   than one release can "go," and so a single trap visit may catch fish from
#'   multiple releases.  Total recaptures are summarized over unique
#'   combinations of trap visits and positions, via variables \code{releaseID}
#'   and \code{trapPositionID}.
#'   
#'   Release records need to have both variables \code{IncludeTest} and 
#'   \code{IncludeCatch} flagged as \code{"Yes"} for inclusion in efficiency 
#'   estimation. Recaptures that took place during half-cone operations are 
#'   multiplied by the value of the \code{halfConeMulti} global variable, which is set 
#'   at 2. Half cone operations are identified by variable \code{HalfCone} 
#'   having a value of \code{"Yes"}.
#'   
#'   Variables \code{HrsToFirstVisitAfter} and \code{HrsToLastVisitAfter} are 
#'   used in function \code{F.est.efficiency} as helper variables to derive a 
#'   batch date when the \code{meanEndTime} variable is \code{NA}.
#'   
#' @section Mean Recapture Time: The mean recapture time is estimated for each 
#'   unique grouping of \code{releaseID} and \code{trapPositionID}.  In the case
#'   of no recaptures, the mean recapture time is recorded as \code{NA}.  In all
#'   other cases, the mean recapture time is calculated as the weighted mean of 
#'   recapture times, weighting on the number of caught fish.  For example, 
#'   suppose fishing takes place at a particular trap over 7 consecutive days. 
#'   If the bulk of fish were caught on day 7, then the mean recapture time 
#'   would be near that 7th day of fishing.  This is in contrast to a "straight"
#'   mean recapture time, which would estimate a mean recapture time 3.5 days
#'   after the start of fishing, regardless of the temporal distribution of 
#'   captured fish over the entire 7-day period.
#'   
#' @seealso \code{F.run.sqlFile}, \code{F.summarize.releases}, \code{F.release.summary}
#' 
#' @author WEST Inc. 
#' 
#' @examples
#' \dontrun{
#' #   ---- Fetch all Chinook salmon efficiency data on the American River 
#' #   ---- between Jan. 16, 2013 and June 8th, 2013.  
#' site <- 57000
#' taxon <- 161980
#' min.date <- "2013-01-01"
#' max.date <- "2013-06-01"
#' df <- F.get.release.data(site,taxon,min.date,max.date)
#' }
F.get.release.data.enh <- function( site, taxon, min.date, max.date, visit.df ){

  # site <- 34000
  # taxon <- 161980
  # min.date <- min.date2#"2006-01-01"
  # max.date <- max.date2#"2006-12-01"
  # visit.df <- visit.df
  
  #   ---- Get global environment data. 
  halfConeMulti <- get("halfConeMulti",envir=.GlobalEnv)

  #   ---- Run report criteria for trap visits. Build TempReportCriteria_Trapvisit.
  nvisits <- F.buildReportCriteria( site, min.date, max.date )

  if( nvisits == 0 ){
    warning("Your criteria returned no trap visits.")
    return()
  }

  #   ---- Run report criteria for efficiency releases. Build TempReportCriteria_Release.
  nreleases <- F.buildReportCriteriaRelease( site, min.date, max.date )

  if( nreleases == 0 ){
    warning("Your criteria returned no releases.")
    return()
  }

  #   ---- Open ODBC channel.
  db <- get( "db.file", envir=.GlobalEnv )
  ch <- odbcConnectAccess(db)

  #   ---- Develop the TempSamplingSummary table.
  F.run.sqlFile( ch, "QrySamplePeriod.sql", R.TAXON=taxon )

  #   ---- Develop the hours fished and TempSamplingSummary table.  
  F.run.sqlFile( ch, "QryEfficiencyTests.sql", R.TAXON=taxon )

  #   ---- Now, fetch the result
  release.visit <- sqlFetch( ch, "TempRelRecap_final" )
  F.sql.error.check(release.visit)

  max.date.eff <- max(release.visit$ReleaseDate) + 30*24*60*60  # 30-day buffer after
  min.date.eff <- min(release.visit$ReleaseDate) - 30*24*60*60  # 30-day buffer before
  
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
                                           visitTime2
                                    FROM trapVisit
                                    WHERE visitTime <= #",as.Date(max.date2),"# 
                                      AND visitTime >= #",as.Date(min.date2),"# 
                                      AND ( (visitTypeID < 5 AND fishProcessedID <> 2)
                                       OR   (visitTypeID = 1 AND fishProcessedID = 2) )
                                    ORDER BY trapPositionID,visitTime"))
  
  close(ch)

  
  
  #   ---- We compile metrics that we need to compile over trapping.  
  
  #   ---- Construct a trapVisit data frame of all visits.  
  tmp <- trapVisits
  #tmp$fishProcessedID <- NULL
  tmp$visitTime <- as.POSIXlt(strftime(trapVisits$visitTime),tz="America/Los_Angeles")
  tmp$visitTime2 <- as.POSIXlt(strftime(trapVisits$visitTime2),tz="America/Los_Angeles")
  
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
  tmp$StartTime <- as.POSIXlt(strftime(c(as.POSIXlt(NA,tz="America/Los_Angeles"),strftime(tmp$visitTime[1:(nrow(tmp) - 1)],tz="America/Los_Angeles"))),tz="America/Los_Angeles")
  
  #   ---- Identify where the lag logic is wrong.  These occur when the lag visitTime != lag visitTime2.  Adjust the StartTime to be correct. 
  tmp$StartTime <- ifelse(as.POSIXlt(strftime(c(as.POSIXlt(NA,tz="America/Los_Angeles"),strftime(tmp$visitTime[1:(nrow(tmp) - 1)],tz="America/Los_Angeles"))),tz="America/Los_Angeles") !=
         as.POSIXlt(strftime(c(as.POSIXlt(NA,tz="America/Los_Angeles"),strftime(tmp$visitTime2[1:(nrow(tmp) - 1)],tz="America/Los_Angeles"))),tz="America/Los_Angeles"),
         as.POSIXct(strftime(c(as.POSIXlt(NA,tz="America/Los_Angeles"),strftime(tmp$visitTime2[1:(nrow(tmp) - 1)],tz="America/Los_Angeles"))),tz="America/Los_Angeles"),
         as.POSIXct(tmp$StartTime))
  
  #   ---- So POSIX is awful with ifelse.  Or I'm not doing it right.  Regardless, put it to how we want it.  
  tmp$StartTime2 <- as.POSIXlt(tmp$StartTime,format="%Y-%m-%d %H:%M:%S",tz="America/Los_Angeles",origin="1970-01-01 00:00:00 UTC")
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
  
  
  forEffPlots <<- tmp[,c("trapVisitID","trapPositionID","StartTime","EndTime","wmForkLength","nForkLength","nightProp","moonProp")]
  
  #plot(forEffPlots$EndTime,forEffPlots$nightProp,col=c("red","orange","green","blue","black")[as.factor(forEffPlots$trapPositionID)],pch=19)
  #plot(forEffPlots$EndTime,forEffPlots$moonProp,col=c("red","orange","green","blue","black")[as.factor(forEffPlots$trapPositionID)],pch=19)
  
  #   ---- Rename to preserve, since 'tmp' is used below, and bring in the goodies.  Clean up tmp a bit so it merges in nicely, 
  #   ---- and doesn't reproduce data already present in release.visit.  
  tmpAstro <- tmp
  names(tmpAstro)[names(tmpAstro) == "SampleMinutes"] <- "JasonSampleMinutes"
  tmpAstro$trapPositionID <- NULL
  release.visit <- merge(release.visit,tmpAstro,by=c("trapVisitID"),all.x=TRUE)

  #   ---- Assign time zones to date-time columns
  time.zone <- get( "time.zone", envir=.GlobalEnv )
  attr(release.visit$ReleaseDate, "tzone") <- time.zone
  attr(release.visit$VisitTime, "tzone") <- time.zone

  #   ---- Drop any rows that are flagged as "Do not include."
  release.visit <- release.visit[ (release.visit$IncludeTest == "Yes") & (release.visit$IncludeCatch == "Yes"), ]

  #   ---- Adjust for halfCone adjustment via the halfConeMulti global variable.
  release.visit$halfConeAdj <- ifelse(release.visit$HalfCone == 'Yes',(halfConeMulti - 1)*release.visit$Recaps,0)
  release.visit$oldRecaps <- release.visit$Recaps
  release.visit$Recaps <- release.visit$oldRecaps + release.visit$halfConeAdj

  
  release.visit[is.na(release.visit$nForkLength),]$nForkLength <- fl0[match(release.visit[is.na(release.visit$nForkLength),]$trapVisitID,fl0$trapVisitID),]$nForkLength
  
  
  
  
  #   ---- Sum over trapVisits at a trapPosition.  
  by.list <- list(trapPositionID = release.visit$trapPositionID,
                  releaseID = release.visit$releaseID )

  #   ---- Create indicator for unique groups below.
  ind  <- tapply( release.visit$Recaps, by.list, FUN=NULL)

  #   ---- Calculate the mean recapture time.
  u.groups <- sort(unique(ind))
  ans <- NULL
  for( g in u.groups ){
    tmp <- release.visit[ ind == g, ]
    one.row <- tmp[1,]

    #   ---- Number caught.  And number of night and moon minutes.  And total (Jason) fishing minutes.  
    one.row$Recaps <- sum(tmp$Recaps, na.rm=T)
    one.row$allNightMins <- sum(tmp$nightMinutes)        
    one.row$allMoonMins <- sum(as.numeric(tmp$moonMinutes))
    one.row$allSampleMins <- sum(as.numeric(tmp$JasonSampleMinutes))
    one.row$allfl <- sum(tmp[tmp$nForkLength > 0,]$wmForkLength*tmp[tmp$nForkLength > 0,]$nForkLength)
    one.row$allNfl <- sum(tmp$nForkLength) 

    #   ---- Compute time to first and last visit after release, even if they did not catch any marked fish. 
    tmp.hdiff <- as.numeric( difftime(tmp$VisitTime, tmp$ReleaseDate, units="hours") )
    one.row$HrsToFirstVisitAfter <-  min( tmp.hdiff )
    one.row$HrsToLastVisitAfter <-  max( tmp.hdiff )

    #   ---- Mean time frame of released fish that were captured.
    if( one.row$Recaps == 0 ){
      one.row$meanRecapTime <- NA
      one.row$meanTimeAtLargeHrs <- NA   
      
      one.row$meanNightProp <- one.row$allNightMins / one.row$allSampleMins
      one.row$meanMoonProp <- one.row$allMoonMins / one.row$allSampleMins
      one.row$meanForkLength <- one.row$allfl / one.row$allNfl
    } else {
      tmp.v <- as.numeric(tmp$VisitTime)
      one.row$meanRecapTime <- sum(tmp.v * tmp$Recaps, na.rm=T) / one.row$Recaps
      one.row$meanTimeAtLargeHrs <- sum(tmp.hdiff * tmp$Recaps, na.rm=T) / one.row$Recaps 
      
      one.row$meanNightProp <- one.row$allNightMins / one.row$allSampleMins
      one.row$meanMoonProp <- one.row$allMoonMins / one.row$allSampleMins
      one.row$meanForkLength <- one.row$allfl / one.row$allNfl
    }
  
    #   ---- Drop the columns over which we are summing.
    one.row <- one.row[,-which( names(one.row) %in% c("VisitTime", "trapVisitID", "SampleMinutes")) ]   
    ans <- rbind(ans, data.frame(one.row))
  }

  class(ans$meanRecapTime) <- class(release.visit$VisitTime)
  attr(ans$meanRecapTime, "tzone") <- attr(release.visit$VisitTime, "tzone")

  cat("First 20 records of RELEASE table:\n")
  print( ans[1:min(nrow(ans),20),] )

  #   ---- Store values of some header info as attribute.
  attr(ans, "taxonID" ) <- taxon
  attr(ans, "siteID" ) <- site

  ans
  #plot(tmp$sunProp[!is.na(tmp$sunProp)],tmp$moonProp[!is.na(tmp$moonProp)])
  #plot(ans$meanNightProp[!is.na(ans$meanNightProp)],ans$meanMoonProp[!is.na(ans$meanMoonProp)])
}