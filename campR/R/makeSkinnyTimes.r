#' @export
#' 
#' @title makeSkinnyTimes
#' 
#' @description Convert one of either the sun or moon wide rise and set times 
#'   from table \code{Dates} from an Access CAMP database into a long skinny
#'   with only one time variable.
#' 
#' @param rise A text string describing the rise time from data frame
#'   \code{dates}.  Either sun or moon rise times.
#' @param set A text string describing the set time from data frame
#'   \code{dates}.  Either sun or moon set times.
#' @param dates A data frame copy of the table \code{Dates} from an Access CAMP
#'   database.
#'   
#' @return A skinny data frame with one column housing datetimes, another to
#'   identify the "Event," i.e., a rise or set, and a third with all-\code{NA} 
#'   \code{trapVisitID}s.
#'   
#' @details Function \code{makeSkinnyTimes} simply converts datetime data in a
#'   wide format housed in two variables (a "rise" and "set") into one data
#'   frame with all datetime data in one column. This function prepares data
#'   frames for use with function \code{getTimeProp}, the true workhorse
#'   function.
#'   
#'   Note that the temporal \code{dates} table is defined via the earliest and
#'   latest efficiency trial date for the run in question.  To this, an
#'   additional month us appended at each end.
#'   
#' @seealso \code{getTimeProp}
#' 
#' @author WEST Inc. 
#' 
#' @examples
#' \dontrun{
#' rise <- "sunRise"
#' set <- "sunSet"
#' dates <- tblDates
#' 
#' sun <- makeSkinnyTimes(rise,set,dates)
#' }
makeSkinnyTimes <- function(rise,set,dates){
  
  # rise <- "sunRise"
  # set <- "sunSet"
  # dates <- tblDates
  
  #   ---- Get variables from the global environment.
  time.zone <- get("time.zone",envir=.GlobalEnv)

  #   ---- Pull out the rise info alone.
  riseDF <- dates[,c("uniqueDate",rise)]
  names(riseDF)[names(riseDF) == rise] <- "time"
  riseDF$Event <- rep(rise,nrow(riseDF))
  
  #   ---- Pull out the set info alone.
  setDF <- dates[,c("uniqueDate",set)]
  names(setDF)[names(setDF) == set] <- "time"
  setDF$Event <- rep(set,nrow(setDF))
  
  #   ---- Stack rise and set info and format appropriately. 
  all <- rbind(riseDF,setDF)
  all$time <- as.POSIXlt(paste(all$uniqueDate,format(all$time,"%H:%M:%S")),format="%Y-%m-%d %H:%M:%S",tz=time.zone)
  all$uniqueDate <- NULL
  all$trapVisitID <- NA
  all <- all[order(all$time),]
  return(all)
}  
