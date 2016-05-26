#' @export
#' 
#' @title F.assign.batch.date - Assign batch dates
#' 
#' @description Assigns batch dates to all trap visis in a data frame based on
#' \code{samplePeriodCutTime}. 
#'   
#' @param df The data frame containing trap visit records for which a day must be 
#'   assigned.
#'   
#' @return Function \code{F.assign.batch.date} maps trapping records containing 
#'   trapping-instance variable \code{EndTime}s to particular days, taking into
#'   consideration that trapping instances often take place over midnight, i.e.,
#'   two separate calendar days.  This function also considers multi-day
#'   trapping instances.
#'   
#'   The function works by utilizing a standardized time, via which trapping 
#'   periods are partitioned into particular days.  The cut time utilized to 
#'   assign minutes to a particular day is 4:00 AM Pacific Time (regardless of 
#'   Daylight Savings).  As of release 4.5, this value is set via global 
#'   variable \code{samplePeriodCutTime}.  The time of 4:00 AM was chosen, 
#'   rather than midnight, in order to not prematurely cut-off trapping 
#'   instances that often continue past midnight.
#'   
#'   Generally, the variable created by function \code{F.assign.batch.date}, 
#'   \code{batchDate}, is the same as variable \code{SampleDate} found in 
#'   query-derived table \code{TempSumUnmarkedByTrap_Run_Final} in any CAMP 
#'   Database.  Variable \code{batchDate} replaces variable \code{SampleDate} in
#'   all dataframes for which function \code{F.assign.batch.date} is called.  To
#'   avoid confusion, new variable \code{batchDate} replaces old variable 
#'   \code{SampleDate} after the running of this function.
#'   
#' @details
#' In order to process the sequence of dates correctly, 
#' function \code{F.assign.batch.date} first identifies the earliest and latest 
#' dates in the provided data frame's trapping \code{EndTime} variable.  Given 
#' \code{EndTime} dates and times, it then moves back one day in time, and two 
#' forward; this encapsulates the earliest and latest \code{EndTime} dates. 
#' Given the unique dates within this resulting range, each of which start at 
#' 12:00 midnight, it then adds the value of global variable 
#' \code{samplePeriodCutTime};  this effectively moves the traditional 
#' day-separation time of midnight to the value of \code{samplePeriodCutTime}, 
#' or 4:00 AM as of release 4.5, for the purposes of assigning fish-counts to
#' particular days.  See Examples.  
#' 
#' All calculations consider daylight savings time and assume that measurements 
#' take place within the Pacific Time Zone.
#' 
#' @seealso
#' 
#' @aliases
#' 
#' @examples
#' # Set the time zone to Pacific.
#' time.zone <- "America/Los_Angeles"
#' 
#' # Create a data set of three trapping instances.  
#' origDate <- data.frame(trapVisitID=c(1234,1235),
#' SampleDate=strptime(c("2014-01-24","2014-01-24"),"%F"),
#' StartTime=strptime(c("2014-01-23 14:28:00","2014-01-24 03:59:00"),"%Y-%m-%d %H:%M:%S", tz=time.zone),
#' EndTime=strptime(c("2014-01-24 03:59:00","2014-01-24 04:00:00"),"%Y-%m-%d %H:%M:%S", tz=time.zone))
#'
#' # BatchDate for \code{trapVisitID=1234} is now 1/23, due to that trap stopping before 4:00 AM.  
#' newDate <- F.assign.batch.date(origDate)

F.assign.batch.date <- function( df ){

  
  
#   startTime <- strptime("2014-01-24 03:55:00","%Y-%m-%d %H:%M:%S",tz=time.zone)
#   times <- data.frame(EndTime=seq(from=startTime,by=60,length.out=180))
  
  
  
  # df <- times     df <- origDate[,c('EndTime')]              # df <- nvCatch2

  cuttime <- get( "samplePeriodCutTime", env=.GlobalEnv )
  midtime <- "00:00:00"   # this is the time of day assigned to batchDates.  Could be half way between cut times or (cuttime - 12*60*60).
  time.zone <- get( "time.zone", env=.GlobalEnv )

  #   A sequence of dates at cuttime every day
  #min.day <- as.POSIXlt( min(df$EndTime) - 24*60*60, format="%Y-%m-%d %H:%M:%S", tz=time.zone)
  #max.day <- as.POSIXlt( max(df$EndTime) + 2*24*60*60, format="%Y-%m-%d %H:%M:%S", tz=time.zone)
  min.day <- min(df$EndTime) - 24*60*60
  max.day <- max(df$EndTime) + 2*24*60*60
  cut.seq <- seq( min.day, max.day, by=24*60*60 )
  cut.day <- format( cut.seq, "%Y-%m-%d" )
  cut.seq <- unique(as.POSIXct( paste( cut.day, cuttime ), format="%Y-%m-%d %H:%M:%S", tz=time.zone))

  #   Bin the sampleEnd's to cut.seq
  ind <- cut( df$EndTime, cut.seq, labels=FALSE )

  #   Establish when the cut time "wraps" to the next day.  I.e., at some point, as cut time increases from 0, you
  #   stop calling the sample from the night before, and attribute it to the current night.  This time is set in 
  #   wrap.POSIX.  jason 4/7/2016 -- note that cut.POSIX < wrap.POSIX always (because we set cuttime to be 4 AM), 
  #   so not sure why this is here.  maybe an artifact from a much older version?  
  cut.POSIX <- as.POSIXct( paste("1970-1-1", cuttime, format="%Y-%m-%d %H:%M:%S" ))
  wrap.POSIX <- as.POSIXct("1970-1-1 06:00:00", format="%Y-%m-%d %H:%M:%S" )

  if( cut.POSIX < wrap.POSIX ){
      add.day <- 0
  } else {
      add.day <- 1
  }
    
  #   Compute batchDate    
  bDate <- as.POSIXct( paste( format(cut.seq[ind] + add.day*24*60*60, "%Y-%m-%d"), midtime), format="%Y-%m-%d %H:%M:%S", tz=time.zone)

  #   If we allow biologists to override batchDate, uncomment the following code.
  #   Figure out which batchDates are missing and replace just those.
  #miss <- is.na(df$batchDate)
  #df$batchDate[miss] <- bDate[miss]
  
  #   Add batch date to data frame
  if( "SampleDate" %in% names(df) ){
    #   overwrite with batchDate
    df$SampleDate <- bDate
    names(df)[names(df) == "SampleDate" ] <- "batchDate"
  } else {
    df$batchDate <- bDate
  }

  df
}
