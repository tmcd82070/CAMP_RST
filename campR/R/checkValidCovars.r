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
#'   It performs three checks.  
#'   
#'   The first ensures that all covariates listed in \code{covarB} are actually 
#'   present in data frame \code{df}.  A covariate could have been used for 
#'   enhanced efficiency fitting, but yet be missing in a particular year
#'   requested for passage, if over ALL years on which the enhanced efficiency
#'   estimation was fit, more than 90% of the e-trials contained data on the
#'   covariate in question.  This implies that, inevitably, one year could "take
#'   the blame," and thus, prevent enhanced-efficiency estimation.  This
#'   behavior occurs on the Stanislaw in 1998 on \code{subsiteID=1004}, where
#'   covariate waterDepth_cm is missing for the majority of the e-trial year. 
#'   Thus, passage involving this year and trap utilized original spline-style
#'   efficiency modeling.
#'   
#'   The second assesses for presence, for each 
#'   \code{batchDate}, with respect to the user-provided \code{min.date} and 
#'   \code{max.date}, which could be arbitrary, while the third assesses for 
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
  
  #   ---- Obtain necessary variables from the global environment.  
  time.zone <- get("time.zone",envir=.GlobalEnv)
  
  #   ---- Part 0:  Make sure all required covariates are actually in df.  It could be they're not in there 
  #   ---- because more than 90% of the covariate, over all years, was present, but for THIS YEAR, it's
  #   ---- missing value(s).  This happens on the Stanlislaw, year 1998, when not much covariate data on
  #   ---- waterDepth_cm was collected, but yet, this covariate is in the final model.  This prevents
  #   ---- the use of Enh Eff for this year on this river.  
  
  #   ---- We set the flag to TRUE, hoping for the best.  
  doEnhEff <- TRUE
  if( sum(names(covarB) %in% names(df)) != length(covarB) ){
    missingVars <- names(covarB)[!(names(covarB) %in% names(df))]
    cat(paste0("\nPROBELM:  Missing variable(s) ",paste0(missingVars,collapse=", ")," in queried set of covariate information, when comparing to required data.  Cannot use enhanced efficiency.\n"))
    doEnhEff <- FALSE
  }

  
  if(doEnhEff == TRUE){
    
    #   ---- PART 1:  Evaluate how much of each covariate we have.  
    min.date.p <- as.POSIXct(min.date,format="%Y-%m-%d",tz=time.zone)
    max.date.p <- as.POSIXct(max.date,format="%Y-%m-%d",tz=time.zone)
    red.usr.df <- df[min.date.p <= df$batchDate & df$batchDate <= max.date.p,]
    red.eff.df <- df[tmp.df$batchDate[1] <= df$batchDate & df$batchDate <= tmp.df$batchDate[nrow(tmp.df)],]
    N.red.usr <- nrow(red.usr.df)
    N.red.eff <- nrow(red.eff.df)
    check0 <- check1 <- check2 <- good1 <- good2 <- rep(0,length(covarB))
    names(check0) <- names(check1) <- names(check2) <- names(good1) <- names(good2) <- names(covarB)
    C <- length(covarB)
    cat(paste0("\nChecking presence of daily enhanced efficiency covariates against your requested time frame for trap ",unique(df$TrapPositionID)[1],".\n"))
    cat("If that doesn't work, I'll at least try to find daily covariate data in your returned set of efficiency trials, in your requested time frame.\n\n")
    for(c in 1:C){
      
      #   ---- Explicitly identify the covar on this loop.  
      covar <- names(covarB)[c]
      
      #   ---- Case 0:  We have no efficiency trials for the given min.date and max.date.  This excludes most attempts 
      #   ---- doing an enhanced efficiency model because they probably also then didn't collect CAMP covars.  We would like 
      #   ---- to still use enh eff in this case, but if the enh eff model has covariates, this is no go.  It is not 
      #   ---- appropriate to simply take the temporal spline alone from the enh eff fit, because that was fit with covariates 
      #   ---- as well.  The exception would be if the enh eff model was a temporal spline alone with no covariates.  
      check0[c] <-  as.logical( (covar %in% names(tmp.df) & nrow(tmp.df) == 0) & (covar %in% names(df) & sum(df[!is.na(df[,covar]),covar]) == 0) )
        
      #   ---- If check0[c] is a zero, then we have no covariate data, due to no efficiency trials.  
      if(check0[c] == 1){
        
        #   ---- Cut to the chase:  we have a problem, and cannot fit enhanced efficiency trials.  
        cat(paste0("PROBELM:  I have no data on variable ",covar," inside the efficiency-trial data range.  Cannot use enhanced efficiency.\n"))
        doEnhEff <- FALSE
      }
    }
    if(all(check0 == 0)){
      for(c in 1:C){
        
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
    }
    
    
    #   ---- PART 2:  Evaluate what we have, and what we don't.  
    cat(paste0("\n"))
    
    #   ---- Check if all covariates have data on all dates, given by min.date and max.date from user.  
    if(sum(good1) == C & all(check0 == 0)){
      
      #   ---- No problem!
      cat(paste0("All batchDates contain data for all necessary covariates, given the requested time frame.  Use of enhanced efficiency models will proceed.\n"))
      doEnhEff <- TRUE
      
      #   ---- Check if all covariates have data on at least all inclusive eff dates. 
    } else if(sum(good2) == C & all(check0 == 0)){
      
      #   ---- Minor problem.  Fill in missing dates with the mean of the covariate.  Tell user I'm doing this.  
      cat(paste0("All batchDates contain data for all necessary covariates, given the first and last efficiency trials in the requested time frame.  Use of enhanced efficiency models will proceed.\n"))
      
      for(c in 1:length(good1[good1 == 0])){
        covar <- names(good1[good1 == 0])[c]
        covarMean <- mean(df[!is.na(df[,covar]),covar])
        cat(paste0("First, however, I'm filling in some missing ",covar," data outside the range of eff trials, but within the range of the provided timeframe, with a mean of ",covarMean,".\n"))
        
        #   ---- Actually fill in with the mean for NA.  These NAs probably extend before min.date and after 
        #   ---- max.date, but I don't care, because I made df bigger than it needs to be, to fit the covariate
        #   ---- splines well on the endpoints.  And if I'm here, I know the NA are outside the date-range 
        #   ---- specified by the start and end of eff trials, given the provided min.date and max.date.  
        df[is.na(df[,covar]),covar] <- covarMean
        doEnhEff <- TRUE
      }
    } else if(sum(good2) < C | any(check0 == 1)){
      
      #   ---- We have a problem, and cannot fit enhanced efficiency trials. 
      cat(paste0("PROBELM:  I'm missing data on at least one variable inside the efficiency-trial data range.  Cannot use enhanced efficiency.\n"))
      doEnhEff <- FALSE
    }
  } 
  ans <- list(df=df,doEnhEff=doEnhEff)
  return(ans)
}