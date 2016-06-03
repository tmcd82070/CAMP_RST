#' @export
#' 
#' @title Summarize a POSIX-formatted vector of dates to one of either day,
#'   week, month, or year.
#'   
#' @param dt A POSIX-formatted date.
#' @param summarize.by A string time unit into which summary occurs, i.e., one 
#'   of "\code{day}," "\code{week}," "\code{month}," "\code{year}."
#'   
#' @return A single-entry list containing a vector of POSIX dates, formatted via
#'   the units specified by \code{summarize.by}.
#'   
#' @details The vector fed to function \code{F.summarize.passage} must have a 
#'   day-fished POSIX column, formatted via the ISO 8601 date format
#'   (\code{\%F}).
#'   
#'   Function \code{F.summarize.index} formats each of day, week, month, and 
#'   year separately.  When \code{summarize.by="day"}, function 
#'   \code{F.summarize.index} simply places the provided POSIX vector \code{dt} 
#'   into a list.  When \code{summarize.by="week"}, table \code{the.dates}, 
#'   containing Julian weeks and stored in the Global environment, is used to 
#'   map provided dates to the specialized Julian Week.  When 
#'   \code{summarize.by="month"}, the provided dates are formatted via 
#'   \code{"\%Y-\%m"}.  Finally, when \code{summarize.by="year"}, provided dates 
#'   are set to the mean year spanning the range of dates provided.  This is 
#'   necessary because dates could span up to 365 days, which normally includes 
#'   two distinct years.???
#'   
#' @examples
#' # Create a list containing a vector of POSIX dates.
#' beg <- strptime("2013-12-24",format="%F",tz="America/Los_Angeles")
#' batchDate <- rep(seq(beg,by=60*60*24,length.out=100),2)
#' 
#' # Summarize indices by different time frames.
#' list.day <- F.summarize.index( batchDate, "day" )
#' 
#' # Dec. 31st becomes the 53rd week, by design.
#' list.week <- F.summarize.index( batchDate, "week" )
#' list.month <- F.summarize.index( batchDate, "month" )
#' 
#' # All indices 2014, even though some dates 2013.
#' list.year <- F.summarize.index( batchDate, "year" )
F.summarize.index <- function( dt, summarize.by ){

  # dt <- catch.df.reduced$batchDate
  # summarize.by <- "week"

  #   ---- A helper function to deal with Julian weeks. 
  f.week <- function( x ){
    jDates <- the.Jdates
    jDates$week <- paste0(jDates$year,'-',formatC(jDates$julianWeek, width=2, flag="0"))
    jDates <- unique(jDates)
    jDates$uniqueDate <- as.Date(jDates$uniqueDate)
  
    dtDF <- data.frame(uniqueDate=as.Date(dt))
    dtDF$R_ID <- seq(1,nrow(dtDF),1)
    test <- merge(dtDF,jDates,by=c('uniqueDate'),all.x=TRUE)
    test <- test[order(test$R_ID),]
    ans <- test$week
    ans
  }

  #   ---- Construct index
  if( summarize.by == "week" ){
    index <- list(s.by=f.week( dt ) )
  } else if( summarize.by == "month" ){
    index <- list(s.by= format( dt, "%Y-%m" ))
  } else if( summarize.by == "year" ){
    #   Because we checked, and min.date and max.date are less than 365 days appart, for annual numbers
    #   we just sum everything.  This is necessary because most winter runs occur in two calender years.
    year.of.mean.date <- mean( dt, na.rm=T )
    tzn <- attr( dt, "tzone" )
    tz.offset <- as.numeric(as.POSIXct(0, origin="1970-01-01", tz=tzn))
    year.of.mean.date <- as.POSIXct( year.of.mean.date-tz.offset, origin="1970-01-01", tz=tzn )  # I think this only works west of GMT (North America).
    year.of.mean.date <- format(year.of.mean.date, "%Y")
    index <- list(s.by= rep( year.of.mean.date, length(dt) ))
  } else { # summarize by day.  
    index <- list(s.by= format( dt, "%Y-%m-%d" ))
  }

  index
}
