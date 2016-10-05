#' @export
#' 
#' @title F.assign.batch.date
#' 
#' @description Assigns batch dates to all trap visits in a data frame via global
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
#'   \code{samplePeriodCutTime}, or 4:00 AM.  The time of 4:00 AM was chosen so 
#'   as to not prematurely cut-off trapping instances that often continue past 
#'   midnight. This effectively moves the traditional day-separation time of 
#'   midnight to 4:00 AM.
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
#'   Database.  See section Structured Query Language (SQL) Queries in
#'   \code{F.run.sqlFile}. Variable \code{batchDate} replaces variable
#'   \code{SampleDate} in all dataframes for which function
#'   \code{F.assign.batch.date} is called.
#'
#' @author WEST Inc.
#' 
#' @examples
#' \dontrun{
#' #   ---- Set the time zone to Pacific.
#' time.zone <- "America/Los_Angeles"
#' 
#' #   ---- Create a data set of three trapping instances.  
#' origDate <- data.frame(trapVisitID=c(1234,1235),
#' SampleDate <- strptime(c("2014-01-24","2014-01-24"),"%F"),
#' StartTime <- strptime(c("2014-01-23 14:28:00","2014-01-24 03:59:00"),
#'   "%Y-%m-%d %H:%M:%S", tz=time.zone),
#' EndTime <- strptime(c("2014-01-24 03:59:00","2014-01-24 04:00:00"),
#'   "%Y-%m-%d %H:%M:%S", tz=time.zone))
#'
#' #   ---- BatchDate for trapVisitID=1234 is now 1/23, 
#' #   ---- since that trap stopped before 4 AM.  
#' newDate <- F.assign.batch.date(origDate)
#' }

F.assign.batch.date <- function( df ){
  
  # df <- df.test
  
  #   ---- Get quantities from the global environment.  
  cuttime <- get( "samplePeriodCutTime", envir=.GlobalEnv )
  time.zone <- get( "time.zone", envir=.GlobalEnv )
  
  #   ---- if-else loses the POSIX formatting.  Get an if-else function that prevents this annoying behavior.
  #   ---- http://stackoverflow.com/questions/6668963/how-to-prevent-ifelse-from-turning-date-objects-into-numeric-objects
  #   ---- Accessed October 5, 2016. 
  safe.ifelse <- function(cond, yes, no) structure(ifelse(cond, yes, no), class = class(yes)) 

  #   ---- Remap EndTime dates to respect the "redefined midnight," i.e., the cuttime. 
  bDate <- as.POSIXct(safe.ifelse(strftime(df$EndTime, format="%H:%M:%S") <= cuttime,
                      format(df$EndTime - 24*60*60, "%Y-%m-%d"),
                      format(df$EndTime, "%Y-%m-%d")), format="%Y-%m-%d", tz=time.zone)

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