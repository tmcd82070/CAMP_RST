#' @export
#' 
#' @title checkValidCovars
#'   
#' @description Check a data frame of efficiency trials and covariates for a 
#'   trap location for complete data.
#'   
#' @param df The data frame for a specific \code{TrapPositionID} containing 
#'   efficiency-trial information and covariates, if available, at the time of 
#'   fitting enhanced efficiency trials in \code{eff_model.r} (or
#'   \code{F.efficiency.model }).
#'   
#' @param tmp.df The reduced data frame originating from \code{df} containing 
#'   only efficiency-trial dates; i.e., those with non-\code{NA}
#'   \code{nReleased} values.
#'   
#' @param min.date The start date for data to include. This is a text string in 
#'   the format \code{\%Y-\%m-\%d}, or \code{YYYY-MM-DD}.
#'   
#' @param max.date The end date for data to include.  Same format as 
#'   \code{min.date}.
#'   
#' @param covarB A character vector containing the covariates deemed important 
#'   in the enhanced efficiency model tied to the \code{TrapPositionID} returned
#'   from \code{df}.
#' 
#' @details The values of \code{min.date} and \code{max.date} are the same as 
#'   those provided by the user in the initial passage estimation run.
#'   
#'   Function \code{checkValidCovars} first assesses the degree of presence of 
#'   each covariate (other than temporal spline basis vectors) required to run 
#'   that particular \code{TrapPositionID}'s enhanced efficiency model via 
#'   \code{df}.
#'   
#'   It performs two checks.  The first assesses for presence, for each 
#'   \code{batchDate}, with respect to the user-provided \code{min.date} and 
#'   \code{max.date}, which could be arbitrary, while the second assesses for 
#'   covariate presence within the \code{batchDate} date range over which 
#'   efficiency trials took place within the provided \code{min.date} and 
#'   \code{max.date}.
#'   
#'   All covariates tied to an enhanced-efficiency fit must at least be present 
#'   for all inclusive efficiency-trial \code{batchDates} to fit the
#'   \code{TrapPositionID}'s enhanced efficiency model.
#'   
#' @return A modified dataframe input of \code{df}, containing possibly 
#'   filled-in values for \code{NA} observed outside the temporal range of 
#'   efficiency trials returned from \code{min.date} and \code{max.date}.
#'   
#' @examples 
#' \dontrun{
#' checkValidCovars(df,tmp.df,min.date,max.date,covarB)
#' }
checkValidCovars <- function(df,tmp.df,min.date,max.date,covarB){
  
  # df <- df
  # tmp.df <- tmp.df
  # min.date <- min.date
  # max.date <- max.date
  # covarB <- covarB
  
  
  #   ---- PART 1:  Evaluate how much of each covariate we have.  
  
  min.date.p <- as.POSIXct(min.date,format="%Y-%m-%d",tz=time.zone)
  max.date.p <- as.POSIXct(max.date,format="%Y-%m-%d",tz=time.zone)
  red.usr.df <- df[min.date.p <= df$batchDate & df$batchDate <= max.date.p,]
  red.eff.df <- df[tmp.df$batchDate[1] <= df$batchDate & df$batchDate <= tmp.df$batchDate[nrow(tmp.df)],]
  N.red.usr <- nrow(red.usr.df)
  N.red.eff <- nrow(red.eff.df)
  check1 <- check2 <- good1 <- good2 <- rep(0,length(covarB))
  names(check1) <- names(check2) <- names(good1) <- names(good2) <- names(covarB)
  C <- length(covarB)
  cat("Checking presence of daily enhanced efficiency covariates against your requested time frame.\n")
  cat("If that doesn't work, I'll at least try to find daily covariate data in your returned set of efficiency trials, in your requested time frame.\n\n")
  for(c in 1:C){
    
    #   ---- Explicitly identify the covar on this loop.  
    covar <- names(covarB)[c]
    
    #   ---- Case 1:  We have all data for this covar, in between provided min.date and max.date.  Keep in mind 
    #   ---- these are provided by the user.
    check1[c] <- sum( seq(min.date.p,max.date.p,by="DSTday") %in% red.usr.df[!is.na(red.usr.df[,covar]),"batchDate"] )
    
    #   ---- Case 2:  We have a covar lacking all data for provided min.date and max.date, but with all data 
    #   ---- inside at least the efficiency-trial dates within min.date and max.date.
    check2[c] <- sum( seq(tmp.df$batchDate[1],tmp.df$batchDate[nrow(tmp.df)],by="DSTday") %in% red.eff.df[!is.na(covar),"batchDate"] )
    
    #   ---- Each evaluated covar gets a message, indicating the strength of the data variable, in terms of presence.  
    if(check1[c] == N.red.usr){
      good1[c] <- 1
      if(good1[c] == 1){
        cat(paste0("With min.date=",min.date," and max.date=",max.date,", covar ",covar," needs ",N.red.usr," batchDate values, and I see all ",check1[c]," of them.\n" ))
      } else {
        cat(paste0("With min.date=",min.date," and max.date=",max.date,", covar ",covar," needs ",N.red.usr," batchDate values, and I see only ",check1[c]," of them.\n" ))
      }
    }
    
    #   ---- Only print this message from the weaker check if we didn't already report status from the first check.  
    if(check2[c] == N.red.eff){
      good2[c] <- 1
      if(good1[c] != 1){
        if(good2[c] == 1){
          cat(paste0("With earliest eff.date=",tmp.df$batchDate[1]," and latest eff.date=",tmp.df$batchDate[nrow(tmp.df)],", covar ",covar," needs ",N.red.eff," batchDate values, and I see all ",check2[c]," of them.\n" ))
        } else {
          cat(paste0("With earliest eff.date=",tmp.df$batchDate[1]," and latest eff.date=",tmp.df$batchDate[nrow(tmp.df)],", covar ",covar," needs ",N.red.eff," batchDate values, and I see only ",check2[c]," of them.\n\n" ))
        }
      }
    } 
  }
  
  
  #   ---- PART 2:  Evaluate what we have, and what we don't.  
  
  #   ---- Check if all covariates have data on all dates, given by min.date and max.date from user.  
  if(sum(good1) == C){
    
    cat(paste0("\n\n"))
    
    #   ---- No problem!
    cat(paste0("All batchDates contain data for all necessary covariates, given the requested time frame.  Use of enhanced efficiency models will proceed.\n"))
    doEnhEff <- TRUE
    
    #   ---- Check if all covariates have data on at least all inclusive eff dates. 
  } else if(sum(good2) == C){
    
    #   ---- Minor problem.  Fill in missing dates with the mean of the covariate.  Tell user I'm doing this.  
    cat(paste0("All batchDates contain data for all necessary covariates, given min.date and max.date.  Use of enhanced efficiency models will proceed.\n"))
    
    for(c in 1:length(good1[good1 == 0])){
      covar <- names(good1[good1 == 0])[c]
      covarMean <- mean(df[!is.na(df[,covar]),covar])
      cat(paste0("First, however, I'm filling in some missing ",covar," data with a mean of ",covarMean," outside the range of eff trials, but within the range of the provided timeframe.\n"))
      
      #   ---- Actually fill in with the mean for NA.  These NAs probably extend before min.date and after 
      #   ---- max.date, but I don't care, because I made df bigger than it needs to be, to fit the covariate
      #   ---- splines well on the endpoints.  And if I'm here, I know the NA are outside the date-range 
      #   ---- specified by the start and end of eff trials, given the provided min.date and max.date.  
      df[is.na(df[,covar]),covar] <- covarMean
      doEnhEff <- TRUE
      
    }
  } else if(sum(good2) < C){
    
    #   ---- We have a problem, and cannot fit enhanced efficiency trials. 
    doEnhEff <- FALSE
  }
  ans <- list(df=df,doEnhEff=doEnhEff)
}