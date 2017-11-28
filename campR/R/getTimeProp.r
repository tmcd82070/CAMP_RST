#' @export
#' 
#' @title getTimeProp
#'   
#' @description Shuffle fishing \code{StartTime}s and \code{EndTime}s with rise 
#'   and set times associated with one of either sun or moon astrological data.
#'   
#' @param df A data frame of skinny astrological data describing one of either 
#'   sun or moon rise or set datetimes.
#' @param rise A text string describing the rise time from data frame 
#'   \code{dates}.  Either sun or moon rise times.
#' @param set A text string describing the set time from data frame 
#'   \code{dates}.  Either sun or moon set times.
#' @param traps A vector of \code{trapPositionID}s over which rise and set data 
#'   are required.
#' @param tmp A data frame consisting of \code{StartTime}s and \code{EndTime}s
#'   for fishing for the current data run.
#' @param metric A text vector of size one.  Typically intended to be one of 
#'   either \code{"sun"} or \code{"moon"}.
#'   
#' @return A data frame with the proportion of time spent fishing during the day
#'   (when the sun was up), or the same when the moon was up.
#'   
#' @details Denominators are defined for each unique trapping instance via
#'   variable \code{trapVisitID}, and are defined as the length of time in
#'   fishing, in minutes.  Numerators are the length of time the sun was up, in
#'   minutes, given the fishing interval, or the corresponding metric for moon
#'   times.
#'   
#'   Note that this variable, usually labeled as \code{"SampleMinutes"} in
#'   results deriving from Connie's SQL queries, varies slightly from Connie's
#'   calculated value.  Generally, however, the number of deviations is small,
#'   given a large sample of unique \code{trapVisitIDs}; i.e., fishing
#'   instances.  In the overwheling majority of cases, the results between 
#'   Connie's value and here exactly agree.  
#'   
#'   The temporal \code{dates} table is defined via the earliest and 
#'   latest efficiency trial date for the run in question.  To this, an 
#'   additional month is appended at each end.
#'   
#' @seealso \code{makeSkinnyTimes}
#'   
#' @author WEST Inc.
#'   
#' @examples
#' \dontrun{
#' dfNew <- getTimeProp(df,rise,set,traps,tmp,metric)
#' }
getTimeProp <- function(df,rise,set,traps,tmp,metric){

  # df <- moon
  # rise <- "moonRise"
  # set <- "moonSet"
  # traps <- traps
  # tmp <- tmp
  # metric <- "moon"
  
  # df <- sun
  # rise <- "sunRise"
  # set <- "sunSet"
  # traps <- traps
  # tmp <- tmp
  # metric <- "sun"

  #   ---- Create skinny of fishing times that is in temporal order by trapPositionID.  
  #   ---- Similar to the above, but with StartTime, EndTime, and by trapPositionID.
  allTmp <- NULL
  for(trap in traps){
  
    #   ---- Get StartTime by itself.  
    tmpRise <- tmp[tmp$trapPositionID == trap,c("trapVisitID","StartTime")]
    names(tmpRise)[names(tmpRise) == "StartTime"] <- "time"
    tmpRise$Event <- rep("StartTime",nrow(tmpRise))
    
    #   ---- Get EndTime by itself.  
    tmpSet <- tmp[tmp$trapPositionID == trap,c("trapVisitID","EndTime")]
    names(tmpSet)[names(tmpSet) == "EndTime"] <- "time"
    tmpSet$Event <- rep("EndTime",nrow(tmpSet))
    
    #   ---- Stack temporal time together.  
    tmp2 <- rbind(tmpRise,tmpSet)
    tmp2 <- tmp2[order(tmp2$time,tmp2$Event),]
    tmp2 <- tmp2[,c("time","Event","trapVisitID")]
  
    #   ---- Shuffle in the fishing data with the astrological data.  
    tmp3 <- rbind(tmp2,df)
    tmp3 <- tmp3[order(tmp3$time),]
    rownames(tmp3) <- NULL
  
    n <- nrow(tmp3)
    
    #   ---- Convert all NA in trapVisitID (due to sunSet and sunRise) to appropriate non-NA.
    #   ---- We need info by trap, so clean up this column.  
    currentTrap <- NA
    for(i in 1:n){
    
      #   ---- Find when we have the start of a trapping instance, and store the trapVisitID.  
      if(!is.na(tmp3$trapVisitID[i]) & tmp3$Event[i] == "StartTime"){
        currentTrap <- tmp3$trapVisitID[i]
      
        #   ---- Put in the value we have for the trapVisitID if it's missing
      } else if(is.na(tmp3$trapVisitID[i]) & !is.na(currentTrap)){
        tmp3$trapVisitID[i] <- currentTrap
        
        #   ---- Reset the currentTrap to NA when we close out that trapVisitID.
      } else if(tmp3$Event[i] == "EndTime"){
        currentTrap <- NA
      }
    }
  
    #   ---- I now want to calculate the number of minutes of sun within a fishing instance.
    #   ---- I choose to do this with respect to rise and set.  This is because I purposefully 
    #   ---- brought in more days from dates before the start and end of fishing.  So, df tmp3
    #   ---- has rows of pure rise and set data in the first and last rows.  So, these 'set up'
    #   ---- the sequence.  
    
    #   ---- When a trap starts fishing, the sun is up, or it is not.  
    tmp3$elapsed <- difftime(c(tmp3$time[2:n],NA),tmp3$time,units="mins")
    tmp3$up <- ifelse(tmp3$Event == rise,1,0)
    
    for(i in 2:n){
      
      #   ---- Examine the NEXT row.  We want to push the '1' of 'up' to the next, if the sun
      #   ---- is up.  It's still up if variable Event says it's not set.    
      if(tmp3$up[i - 1] == 1 & tmp3$Event[i] != set){
        repeat{
        
          #   ---- The sun is now up.  Push the '1' through until the sun sets.  
          tmp3$up[i] <- 1
          i <- i + 1
          if(tmp3$Event[i] == set){
            break
          }
        }
      }
    }
    allTmp <- rbind(allTmp,tmp3)
    
    #   ---- When Event == "EndTime" the trapping is done.  Any non-zero value in elapsed 
    #   ---- captures sun-up time after the trap is done fishing.  Force these to zero. 
    allTmp[allTmp$Event == "EndTime",]$elapsed <- 0
  }  
    
  #   ---- Now, tally up the total minutes for each trapping instance.  
  allTmpUp <- allTmp[allTmp$up == 1,]
  allSumm <- aggregate(allTmpUp$elapsed,list(trapVisitID=allTmpUp$trapVisitID),sum) 
  names(allSumm)[names(allSumm) == "x"] <- paste0(metric,"Minutes")
  
  #   ---- If there was no moon up during an entire fishing instance, then we just missed it, 
  #   ---- because up is uniformly 0 in this case.  
  #   ---- Find these, and make sure they get 0 Minutes in allSumm.  
  the0 <- unique(tmp$trapVisitID)[!(unique(tmp$trapVisitID) %in% unique(allSumm$trapVisitID))]
  the0 <- data.frame(trapVisitID=the0,Minutes=rep(0,length(the0)))
  names(the0)[names(the0) == "Minutes"] <- paste0(metric,"Minutes")
  
  #   ---- Put the two together and sort for prettiness.  
  allSumm <- rbind(allSumm,the0)
  allSumm[,paste0(metric,"Minutes")] <- as.numeric(allSumm[,paste0(metric,"Minutes")])
  allSumm <- allSumm[order(allSumm$trapVisitID),]
    
  #   ---- Merge in the results to the master dataframe tmp, and clean up for exit.  
  done <- merge(tmp,allSumm,by=c("trapVisitID"),all.x=TRUE)
  propMet <- paste0(metric,"Prop")
  done[,propMet] <- done[,paste0(metric,"Minutes")] / as.numeric(done[,"SampleMinutes"])
  
  #   ---- Make sure that proportion measures that should have no value actually have none.
  done[,propMet] <- ifelse(done$SampleMinutes %in% c(-88,-99),NA,done[,propMet])

  return(done)
}
