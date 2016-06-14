#' @export
#' 
#' @title F.assign.batch.date
#' 
#' @description Assigns batch dates to all trap visits in a data frame via
#'   variable \code{samplePeriodCutTime}.
#'   
#' @param df The data frame containing trap visit records for which a day must be 
#'   assigned.
#'   
#' @return The same data frame \code{df} submitted to the function, with new
#' variable \code{batchDate} replacing old variable \code{SampleDate}.
#'   
#' @details In order to process the sequence of dates correctly, function 
#'   \code{F.assign.batch.date} first identifies the earliest and latest dates 
#'   in the provided data frame's trapping \code{EndTime} variable.  Given 
#'   \code{EndTime} dates and times, it then moves back one day in time, and two
#'   forward; this encapsulates the earliest and latest \code{EndTime} dates. 
#'   Given the unique dates within this resulting range, each of which start at 
#'   12:00 midnight, it then adds the value of global variable 
#'   \code{samplePeriodCutTime}, or 4:00 AM.  The time of 4:00 AM was chosen 
#'    to not prematurely cut-off trapping 
#'   instances that often continue past midnight. This effectively moves the
#'   traditional day-separation time of midnight to 4:00 AM.
#'   
#'   Function \code{F.assign.batch.date} maps trapping records containing 
#'   trapping-instance variable \code{EndTime}s to particular days, taking into 
#'   consideration that trapping instances often take place over midnight, i.e.,
#'   two separate calendar days.  This function also considers multi-day
#'   trapping instances.  All calculations consider daylight savings time and
#'   assume that measurements take place within the Pacific Time Zone.  
#'   
#'   Generally, the variable created by function \code{F.assign.batch.date}, 
#'   \code{batchDate}, is the same as variable \code{SampleDate} found in 
#'   query-derived table \code{TempSumUnmarkedByTrap_Run_Final} in any CAMP 
#'   Database.  Variable \code{batchDate} replaces variable \code{SampleDate} in
#'   all dataframes for which function \code{F.assign.batch.date} is called.  To
#'   avoid confusion, new variable \code{batchDate} replaces old variable 
#'   \code{SampleDate} after the running of this function.
#'
#' @author Trent McDonald (tmcdonald@west-inc.com)
#' 
#' @examples
#' #   ---- Set the time zone to Pacific.
#' time.zone <- "America/Los_Angeles"
#' 
#' #   ---- Create a data set of three trapping instances.  
#' origDate <- data.frame(trapVisitID=c(1234,1235),
#' SampleDate <- strptime(c("2014-01-24","2014-01-24"),"%F"),
#' StartTime <- strptime(c("2014-01-23 14:28:00","2014-01-24 03:59:00"),"%Y-%m-%d %H:%M:%S", tz=time.zone),
#' EndTime <- strptime(c("2014-01-24 03:59:00","2014-01-24 04:00:00"),"%Y-%m-%d %H:%M:%S", tz=time.zone))
#'
#' #   ---- BatchDate for trapVisitID=1234 is now 1/23, 
#' #   ---- since that trap stopped before 4 AM.  
#' newDate <- F.assign.batch.date(origDate)

F.assign.batch.date <- function( df ){
  
  # df <- times 
  
  cuttime <- get( "samplePeriodCutTime", envir=.GlobalEnv )
  
  #   ---- This is the time of day assigned to batchDates.  Could be halfway between cut times or (cuttime - 12*60*60).
  midtime <- "00:00:00"   
  time.zone <- get( "time.zone", envir=.GlobalEnv )
  
  #   ---- A sequence of dates at cuttime every day.
  min.day <- min(df$EndTime) - 24*60*60
  max.day <- max(df$EndTime) + 2*24*60*60
  cut.seq <- seq( min.day, max.day, by=24*60*60 )
  cut.day <- format( cut.seq, "%Y-%m-%d" )
  cut.seq <- unique(as.POSIXct( paste( cut.day, cuttime ), format="%Y-%m-%d %H:%M:%S", tz=time.zone))
  
  #   ---- Bin the sampleEnds to cut.seq.
  ind <- cut( df$EndTime, cut.seq, labels=FALSE )
  
  #   ---- Establish when the cut time "wraps" to the next day.  At some point, as cut time increases from 0, you
  #   ---- stop calling the sample from the night before, and attribute it to the current night.  This time is set in 
  #   ---- wrap.POSIX.    
  cut.POSIX <- as.POSIXct( paste("1970-1-1", cuttime, format="%Y-%m-%d %H:%M:%S" ))
  wrap.POSIX <- as.POSIXct("1970-1-1 06:00:00", format="%Y-%m-%d %H:%M:%S" )
  
  if( cut.POSIX < wrap.POSIX ){
    add.day <- 0
  } else {
    add.day <- 1
  }
  
  #   ---- Compute batchDate.    
  bDate <- as.POSIXct( paste( format(cut.seq[ind] + add.day*24*60*60, "%Y-%m-%d"), midtime), format="%Y-%m-%d %H:%M:%S", tz=time.zone)
  
  #   ---- Add batch date to data frame.
  if( "SampleDate" %in% names(df) ){
    
    #   ---- Overwrite with batchDate.
    df$SampleDate <- bDate
    names(df)[names(df) == "SampleDate" ] <- "batchDate"
  } else {
    df$batchDate <- bDate
  }
  df
}
