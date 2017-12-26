#' @export
#' 
#' @title stepper
#'   
#' @description For a set of covariates for which estimates have been obtained
#'   on non-continguous dates, expand recorded data values to fill in all dates,
#'   so as to create a step-like function.
#'   
#' @param df A data frame containing variables recorded via non-continguous
#'   \code{batchDate}s.  Argument \code{df} should include \code{"batchDate"}
#'   and \code{"TrapPositionID"} in addition to the variables for which
#'   step-expansion is required.
#'   
#' @param varVec A string vector containing the names of variables for which
#'   step-expansion is required.  Typically set to
#'   \code{c("bdMeanNightProp","bdMeanMoonProp","bdMeanForkLength")} for most
#'   CAMP applications.
#'   
#' @param min.date The start date for data to include. This is a text string in 
#'   the format \code{\%Y-\%m-\%d}, or \code{YYYY-MM-DD}.
#'   
#' @param max.date The end date for data to include.  Same format as 
#'   \code{min.date}.
#'   
#' @return A dataframe with one row for all unique \code{batchDates} included 
#'   within \code{min.date} and \code{max.date}, inclusive.  All variables, as 
#'   provided via \code{varVec}, have a non-\code{NA} value.
#'   
#' @details The \code{stepper} function is usually only called within
#'   \code{eff_model} when applying previously obtained enhanced efficiency
#'   model results.
#'   
#' @seealso \code{eff_model}
#'   
#' @author WEST Inc.
#'   
#' @examples
#' \dontrun{
#' df <- stepper(df=df,
#'               varVec=c("bdMeanNightProp","bdMeanMoonProp","bdMeanForkLength"),
#'               min.date=min.date,
#'               max.date=max.date)
#' }
stepper <- function(df,varVec,min.date,max.date){
  
  # df <- tmp.df[,c("batchDate","TrapPositionID","bdMeanNightProp","bdMeanMoonProp","bdMeanForkLength")]
  # varVec <-   c("bdMeanNightProp","bdMeanMoonProp","bdMeanForkLength")
  # min.date <- min.date
  # max.date <- max.date

  #traps <- unique(df$TrapPositionID)
  
  #   ---- Get quantities from the global environment.  
  time.zone <- get( "time.zone", envir=.GlobalEnv )
  
  df.new <- NULL
  trap <- df$TrapPositionID[1]
 
  #   ---- Start the sequence of days.  
  earliestBatchDate <- as.POSIXct(min.date,format="%Y-%m-%d",tz=time.zone)
  fakeFirstRow <- data.frame(batchDate=earliestBatchDate,
                             TrapPositionID = trap,
                             bdMeanNightProp = df[1,]$bdMeanNightProp,
                             bdMeanMoonProp = df[1,]$bdMeanMoonProp,
                             bdMeanForkLength = df[1,]$bdMeanForkLength)
    
  df <- rbind(fakeFirstRow,df)
    
  df$batchDate  <- as.POSIXct(strptime(df$batchDate,format="%Y-%m-%d"),format="%Y-%m-%d",tz=time.zone)
  df$batchDateNext <- as.POSIXct(strptime(df$batchDate[c(2:nrow(df),NA)],format="%Y-%m-%d"),format="%Y-%m-%d",tz=time.zone)
    
  #   ---- Put in a value that is one month after the last batchDate for batchDateNext.
  df[nrow(df),]$batchDateNext <- as.POSIXct(max.date,format="%Y-%m-%d",tz=time.zone) 
    
  #   ---- Get rid of rows where batchDate = batchDateNext
  df <- df[df$batchDate != df$batchDateNext,]
    
  #   ---- Slide batchDateNext back by one day, so as to prevent duplication of batchDate in expansion. Daylight savings?
  #df$batchDateNext <- df$batchDateNext - 24*60*60
    
  #   ---- Actually assign values for each "step."  
  df.new <- NULL
  for(i in 1:nrow(df)){
    if(i < nrow(df)){
      df.i <- data.frame(TrapPositionID=trap,batchDate=seq(df[i,"batchDate"],df[i,"batchDateNext"] - 24*60*60,by="1 DSTday"))
    } else {   # Last of them all, so don't slide back a day.  Otherwise, we lose the last max.date. 
      df.i <- data.frame(TrapPositionID=trap,batchDate=seq(df[i,"batchDate"],df[i,"batchDateNext"],by="1 DSTday"))
    }
    for(j in 1:length(varVec)){
      df.i[,paste0(varVec[j],"Step")] <- ifelse(is.na(df[i,varVec[j]]) | is.nan(df[i,varVec[j]]),-99,df[i,varVec[j]])
    }
      
    df.new <- rbind(df.new,df.i)
  }
    
  #   ---- Grab a mean value to use in case any one of the metrics is -99.  Do this after the expansion 
  #   ---- so that we weight on the number of days.  Do this for each trap. 
  means <- rep(0,length(varVec))
  for(i in 1:length(varVec)){
    nums <- df.new[,paste0(varVec[i],"Step")]
    means[i] <- mean(nums[nums != -99])
    df.new[df.new[,paste0(varVec[i],"Step")] == -99,paste0(varVec[i],"Step")] <- means[i]
  }

  return(df.new)
}
