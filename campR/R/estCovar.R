#'@export
#'
#'@title estCovar
#'  
#'@description Query individual covariates and their recorded units from an 
#'  Access CAMP database.
#'  
#' @param dbCov An object originating from function \code{getCAMPEnvCov}, i.e., 
#'   preprocessed environmental data from a CAMP database.
#' 
#' @param covName  The text string name of an environmental covariate, with its 
#'   units appended via an underscore, e.g., \code{temperature_C}.
#'   
#' @param estType A single number taking on values \code{1} or \code{2}.  Number
#'   \code{1} is used for quantitative variables, while number \code{2} is used
#'   for quantitative.
#'  
#' @param traps A vector of \code{trapPositionID}s encompassing all the unique 
#'  traps in data frame \code{obs.eff.df}.
#'  
#' @param obs.eff.df  A data frame containing observed efficiency trials.  Note
#'  that this data frame contains one day for all in-between days, given
#'  efficiency trials.
#'  
#' @return Data frame \code{obs.eff.df} with the requested covariate's data
#'  values appended on days for which data were available.
#'  
#' @details Quantitative variables are fed into a smoothing spline (via function
#'  \code{smooth.spline}) so as to allow for exact interpolation / prediction on
#'  efficiency time values.
#'  
#'  Qualitative variables, which currently only includes \code{"weather"},
#'  simply reports values.  No smoothing is performed.
#'  
#' @seealso \code{getCAMPEnvCov}
#'  
#' @author WEST Inc.
#'  
#' @examples
#' \dontrun{
#' estCovar(dbCov,covName,estType,traps,obs.eff.df)
#'}
estCovar <- function(dbCov,covName,estType,traps,obs.eff.df){
  
  # dbCov <- dbDpcm
  # covName <- "waterDepth_cm"
  # estType <- 1
  # traps <- traps
  # obs.eff.df <- obs.eff.df

  # dbCov <- dbWeat
  # covName <- "precipLevel_qual"
  # estType <- 2
  # traps <- traps
  # obs.eff.df <- obs.eff.df
  
  CAMPCovName <- strsplit(covName,"_",fixed=TRUE)[[1]][1]
  
  if(nrow(dbCov) == 0 | sum(!is.na(dbCov[,CAMPCovName])) == 0){
    #obs.eff.df[,covName] <- NA
  } else {
    
    allCovar <- NULL
    dbCov <- dbCov[dbCov$subSiteID %in% traps,]
    theJJ <- unique(dbCov$subSiteID)
    obs.eff.df[,covName] <- NA                                 #     $turbidity_ntu
    if(sum(!is.na(dbCov[,CAMPCovName])) > 0){
      
      if(estType == 1){
        
        for(jj in 1:length(theJJ)){
          
          jdbCov <- dbCov[dbCov$subSiteID == theJJ[jj],]
          
          #   ---- Compile the good dates for each subSiteID.   
          min.date.cov <- suppressWarnings(min(jdbCov[!is.na(jdbCov[,CAMPCovName]),]$measureTime))
          max.date.cov <- suppressWarnings(max(jdbCov[!is.na(jdbCov[,CAMPCovName]),]$measureTime))
          
          #   ---- I only keep the current.  So, after running, only the last jj is here.  
          m3[[ii]] <- smooth.spline(as.numeric(jdbCov[!is.na(jdbCov[,CAMPCovName]),]$measureTime),jdbCov[!is.na(jdbCov[,CAMPCovName]),CAMPCovName],cv=TRUE)
          
          #   ---- Build up the formula string in data frame obs.eff.df.
          if("covar" %in% names(obs.eff.df)){  # always true?
            if(is.na(obs.eff.df$covar[1])){
              obs.eff.df[obs.eff.df$TrapPositionID == theJJ[jj],]$covar <- covName
            } else {
              obs.eff.df[obs.eff.df$TrapPositionID == theJJ[jj],]$covar <- paste0(obs.eff.df[obs.eff.df$TrapPositionID == theJJ[jj],]$covar," + ",covName)
            }
          } else {
            obs.eff.df[obs.eff.df$TrapPositionID == theJJ[jj],]$covar <- covName
          }
          
          #   ---- Helpful in checking.  Eventually delete.  
          #table(obs.eff.df$TrapPositionID,obs.eff.df$covar,exclude=NULL)
          
          obs.eff.df[obs.eff.df$TrapPositionID == theJJ[jj] & obs.eff.df$TrapPositionID %in% xwalk[xwalk$ourSiteIDChoice1 == oursitevar,]$subSiteID,covName] <- predict(m3[[ii]],as.numeric(obs.eff.df[obs.eff.df$TrapPositionID == theJJ[jj],]$batchDate))$y
          
          #jdbCov$pred_turbidity_ntu <- predict(m3[[ii]])$y
          jdbCov[paste0("pred_",covName)] <- predict(m3[[ii]],x=as.numeric(jdbCov$measureTime))$y
          
          allCovar <- rbind(allCovar,jdbCov)
          
          #    ---- See if we have any predicted values outside the range for which we have data.
          if(sum(jdbCov$measureTime < min.date.cov | jdbCov$measureTime > max.date.cov) > 0){
            jdbTurb[jdbCov$measureTime < min.date.cov | jdbCov$measureTime > max.date.cov,paste0("pred_",covName)] <- NA
          }
          
          #    ---- See if we have any predicted values outside the range for which we have data.  Off by a day..?  Daylight savings?  So buffer.
          if(sum(obs.eff.df[obs.eff.df$TrapPositionID == theJJ[jj],]$batchDate + 60*60 < min.date.cov | obs.eff.df[obs.eff.df$TrapPositionID == theJJ[jj],]$batchDate - 60*60 > max.date.cov) > 0){
            obs.eff.df[obs.eff.df$TrapPositionID == theJJ[jj] & (obs.eff.df[obs.eff.df$TrapPositionID == theJJ[jj],]$batchDate + 60*60 < min.date.cov | obs.eff.df[obs.eff.df$TrapPositionID == theJJ[jj],]$batchDate - 60*60 > max.date.cov),covName] <- NA
          }
          
          #   ---- Helpful in checking.  Eventually delete.  
          #obs.eff.df[obs.eff.df$TrapPositionID == "1001" & !is.na(obs.eff.df$turbidity_ntu),]$turbidity_ntu
          
        }
      } else if(estType == 2){
        
        #   ---- Get rid of setting up the covariate that we did above.  We are going to merge in, and not match. 
        obs.eff.df[,covName] <- NULL
        
        #   ---- Use this if the covariate is qualitative -- doesn't make sense to spline it out, e.g., weather.
        #dbCov$batchDate <- as.POSIXct(strptime(dbCov$measureTime,format="%Y-%m-%d",tz=time.zone),format="%Y-%m-%d",tz=time.zone)
        names(dbCov)[names(dbCov) == "measureTime"] <- "EndTime"
        names(dbCov)[names(dbCov) == "subSiteID"] <- "TrapPositionID"
        dbCov <- F.assign.batch.date(dbCov)
        
        #   ---- In the qualitative case, could have non-unique TrapPositionID + batchDate values, due to readings taken twice (or more)
        #   ---- in the same day.  Since we don't use a smoothing spline with prediction here, this is a problem.  So...just take
        #   ---- the first.  
        dbCov <- dbCov[!duplicated(paste0(dbCov$TrapPositionID,dbCov$batchDate)),]
    
        #   ---- Bring new data in.  
        obs.eff.df <- merge(obs.eff.df,dbCov[,c("TrapPositionID","batchDate",CAMPCovName)],by=c("TrapPositionID","batchDate"),all.x=TRUE)
        names(obs.eff.df)[names(obs.eff.df) == CAMPCovName] <- covName
      }
    }  
  }  
  return(obs.eff.df)
}
