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
#' @param xwalk A data frame containing lookup information tying
#'   \code{subSiteID} to the unique identifying \code{ourSiteIDChoice1} (and
#'   \code{ourSiteIDChoice2}) in the online postgreSQL covariate database.
#'   
#' @param oursitevar The integer of length one identifying the
#'   \code{ourSiteIDChoice1} (or \code{ourSiteIDChoice2}) of the
#'   \code{subSiteID}'s identifying station number in the postgreSQL covariate
#'   database.
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
estCovar <- function(dbCov,covName,estType,traps,obs.eff.df,xwalk,oursitevar){
  
  # dbCov <- dbTurb #dbWVel
  # covName <- "turbidity_ntu"   #discharge_cfs" #"waterVel_fts"
  # estType <- 1
  # traps <- traps
  # obs.eff.df <- obs.eff.df
  # xwalk <- xwalk
  # oursitevar <- oursitevar
  
  #   ---- Obtain necessary variables from the global environment.  
  time.zone <- get("time.zone",envir=.GlobalEnv)
  
  CAMPCovName <- strsplit(covName,"_",fixed=TRUE)[[1]][1]
  
  if(nrow(dbCov) == 0 | sum(!is.na(dbCov[,CAMPCovName])) == 0){
    #obs.eff.df[,covName] <- NA
    
    
  
  } else {
    
    allCovar <- NULL
    dbCov <- dbCov[dbCov$subSiteID %in% traps,]
    theJJ <- unique(dbCov$subSiteID)
    obs.eff.df[,covName] <- NA
    
    if(sum(!is.na(dbCov[,CAMPCovName])) > 0){
      
      if(estType == 1){
        
        for(jj in 1:length(theJJ)){
          
          jdbCov <- dbCov[dbCov$subSiteID == theJJ[jj],]
          
          #   ---- Compile the good dates for each subSiteID.   Jason changes old "measureTime" to "measureDate" on 11/20/2017 due to updated db.
          min.date.cov <- as.POSIXct(format(min(jdbCov[!is.na(jdbCov[,CAMPCovName]),]$measureDate),format="%Y-%m-%d",tz=time.zone),format="%Y-%m-%d",tz=time.zone)
          max.date.cov <- as.POSIXct(format(max(jdbCov[!is.na(jdbCov[,CAMPCovName]),]$measureDate),format="%Y-%m-%d",tz=time.zone),format="%Y-%m-%d",tz=time.zone)
          
          #   ---- If there is only one observation, or < 4 unique values, the smooth.spline doesn't appear to work.  Force it.
          if( (sum(!is.na(jdbCov[,CAMPCovName])) == 1) | (length(unique(jdbCov[,CAMPCovName][!is.na(jdbCov[,CAMPCovName])])) < 4) ){
            m3 <- jdbCov[,CAMPCovName]
          } else {
             
            #   ---- I only keep the current.  So, after running, only the last jj is here.  Jason uses cv=FALSE due to (now duplicated) dates in measureDate (11/20/2017).
            m3 <- smooth.spline(as.numeric(jdbCov[!is.na(jdbCov[,CAMPCovName]),]$measureDate),jdbCov[!is.na(jdbCov[,CAMPCovName]),CAMPCovName],cv=FALSE)
          }
          
          
          
          # plot(dbTurb$measureDate,dbTurb$turbidity,pch=19,cex=0.5,xlim=c(min(dbTurb$measureDate),max(dbTurb$measureDate)),ylim=c(0,30))
          # par(new=TRUE)
          # plot(m3$x,m3$y,xlim=c(min(dbTurb$measureDate),max(dbTurb$measureDate)),ylim=c(0,30),type="l",col="red")
          
          #   ---- Sometimes, there is an environmental observation on a day that doesn't correspond to an efficiency trial
          #   ---- batchDate.  This could be because we take the average of catch days as the defined efficiency-trial date.  
          #   ---- For now, throw out instances where these dates don't match.  Generally, this occurs because the 
          #   ---- environmental measurement wasn't made consistently on all days during a season.  Could be enhanced so 
          #   ---- that single-day measurements of environmental covariates cover more days.  We deal with this right here
          #   ---- before the covariate is added to the building covar string.  
          batchDateForChecking <- as.POSIXct(format(jdbCov[!is.na(jdbCov[,CAMPCovName]),]$measureDate,format="%Y-%m-%d",tz=time.zone),format="%Y-%m-%d",tz=time.zone)
          if(sum(batchDateForChecking %in% obs.eff.df[obs.eff.df$TrapPositionID == theJJ[jj] & !is.na(obs.eff.df$efficiency),]$batchDate) > 0){
           
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
            
            #   ---- If there is only one observation, the smooth.spline doesn't appear to work.  Force it.
            if( (sum(!is.na(jdbCov[,CAMPCovName])) == 1) | (length(unique(jdbCov[,CAMPCovName][!is.na(jdbCov[,CAMPCovName])])) < 4) ){
              obs.eff.df[obs.eff.df$TrapPositionID == theJJ[jj] & obs.eff.df$TrapPositionID %in% xwalk[xwalk$ourSiteIDChoice1 == oursitevar,]$subSiteID & obs.eff.df$batchDate %in% batchDateForChecking,covName] <- m3
              jdbCov[paste0("pred_",covName)] <- m3
            } else {
              obs.eff.df[obs.eff.df$TrapPositionID == theJJ[jj] & obs.eff.df$TrapPositionID %in% xwalk[xwalk$ourSiteIDChoice1 == oursitevar,]$subSiteID,covName] <- predict(m3,as.numeric(obs.eff.df[obs.eff.df$TrapPositionID == theJJ[jj],]$batchDate))$y
              #jdbCov$pred_turbidity_ntu <- predict(m3)$y
              jdbCov[paste0("pred_",covName)] <- predict(m3,x=as.numeric(jdbCov$measureDate))$y
            }
            
            allCovar <- rbind(allCovar,jdbCov)
            
            #    ---- See if we have any predicted values outside the range for which we have data.
            if(sum(allCovar$measureDate < min.date.cov | allCovar$measureDate > max.date.cov) > 0){
              allCovar[allCovar$measureDate < min.date.cov | allCovar$measureDate > max.date.cov,paste0("pred_",covName)] <- NA
            }
            
            #    ---- See if we have any predicted values outside the range for which we have data.  
            if(sum(obs.eff.df[obs.eff.df$TrapPositionID == theJJ[jj],]$batchDate < min.date.cov | obs.eff.df[obs.eff.df$TrapPositionID == theJJ[jj],]$batchDate > max.date.cov) > 0){
              obs.eff.df[obs.eff.df$TrapPositionID == theJJ[jj] & (obs.eff.df[obs.eff.df$TrapPositionID == theJJ[jj],]$batchDate < min.date.cov | obs.eff.df[obs.eff.df$TrapPositionID == theJJ[jj],]$batchDate > max.date.cov),covName] <- NA
            }
            
            #   ---- Helpful in checking.  Eventually delete.  
            #obs.eff.df[obs.eff.df$TrapPositionID == "1001" & !is.na(obs.eff.df$turbidity_ntu),]$turbidity_ntu
          } else {
            
            #   ---- If we're here, we don't have an environmental observation on a date of an efficiency trial.  So, if 
            #   ---- we have a column of all NA in obs.eff.df, get rid of it.  I think it can only be NA, or possibly
            #   ---- non-NA on non-batchDates, which we don't want.
            if(covName %in% names(obs.eff.df)){
              obs.eff.df[,covName] <- NULL
            }
          }
        }
      } else if(estType == 2){
        
        #   ---- Get rid of setting up the covariate that we did above.  We are going to merge in, and not match. 
        obs.eff.df[,covName] <- NULL
        
        #   ---- Use this if the covariate is qualitative -- doesn't make sense to spline it out, e.g., weather.
        #dbCov$batchDate <- as.POSIXct(strptime(dbCov$measureTime,format="%Y-%m-%d",tz=time.zone),format="%Y-%m-%d",tz=time.zone)
        names(dbCov)[names(dbCov) == "measureDate"] <- "EndTime"
        names(dbCov)[names(dbCov) == "subSiteID"] <- "TrapPositionID"
        dbCov <- F.assign.batch.date(dbCov)
        
        #   ---- In the qualitative case, could have non-unique TrapPositionID + batchDate values, due to readings taken twice (or more)
        #   ---- in the same day.  Since we don't use a smoothing spline with prediction here, this is a problem.  So...just take
        #   ---- the first.  
        dbCov <- dbCov[!duplicated(paste0(dbCov$TrapPositionID,dbCov$batchDate)),]
    
        
        #   ---- Build up the formula string in data frame obs.eff.df.
        for(jj in 1:length(theJJ)){
          if("covar" %in% names(obs.eff.df)){  # always true?
            if(is.na(obs.eff.df$covar[1])){
              obs.eff.df[obs.eff.df$TrapPositionID == theJJ[jj],]$covar <- covName
            } else {
              obs.eff.df[obs.eff.df$TrapPositionID == theJJ[jj],]$covar <- paste0(obs.eff.df[obs.eff.df$TrapPositionID == theJJ[jj],]$covar," + ",covName)
            }
          } else {
            obs.eff.df[obs.eff.df$TrapPositionID == theJJ[jj],]$covar <- covName
          }
        }
        
        #   ---- Bring new data in.  
        obs.eff.df <- merge(obs.eff.df,dbCov[,c("TrapPositionID","batchDate",CAMPCovName)],by=c("TrapPositionID","batchDate"),all.x=TRUE)
        names(obs.eff.df)[names(obs.eff.df) == CAMPCovName] <- covName
        
      }
    }  
  }  
  return(obs.eff.df)
}
