#' @export
#' 
#' @title reduceETrials
#'   
#' @description Map the set of efficiency trials to the 1959-1960 enhanced
#'   efficiency spline-fitting paradigm.
#'   
#' @param df The data frame for a specific \code{TrapPositionID} containing 
#'   efficiency-trial information and covariates, if available, at the time of 
#'   fitting enhanced efficiency trials in \code{eff_model.r} (or 
#'   \code{F.efficiency.model }).
#' 
#' @param possibleVars The set of all available variables for possible inclusion
#'   to the fitting of an enhanced efficiency model.  Usually include
#'   efficiency-trial variables, environmental covariate variables, and
#'   CAMP-collected variables.
#' 
#' @param bsplBegDt The first date, via the spline-1959-1960 paradigm, to which
#'   all efficiency years and dates collapse.
#'   
#' @param bsplEndDt The last date, via the spline-1959-1960 paradigm, to which
#'   all efficiency years and dates collapse.
#' 
#' @param trap A trap for which efficiency data are available. 
#' 
#' @param all.ind.inside A logical vector of length equal to the number of rows
#'   of data frame \code{df}, expressing which \code{batchDates} fall within the
#'   temporal data-range for which estimation occurs.
#'   
#' @details Function \code{reduceETrials} does the work of mapping several
#'   years' worth of efficiency trials to one common temporal period.  For
#'   convenience, the common temporal period is 1960.  1960 was select because
#'   it is a leap-year.  It is also near the \code{POSIX} temporal original date
#'   of \code{"1970-01-01"}.  This is helpful in rare cases in which direct
#'   manipulation of \code{POSIX} items is necessary, since raw numerics
#'   associated with \code{POSIX} variables record the number of seconds since 
#'   the origination date.
#'   
#'   Sometimes, the spline year paradigm for a river may stretch back into 1959.
#'   It never stetches into 1961.  This is important for use in function 
#'   \code{eff_model}, where care must be taken to ensure that the resulting 
#'   1960-spline fit housed in previously fit enhanced efficiency data maps 
#'   correctly to provided \code{min.date} and \code{max.date}, when provided
#'   for passage estimation.  Lack of attention here could result in re-maps to
#'   the wrong \code{min.date} and \code{max.date} year.
#'   
#' @return A list containing several items.  
#' 
#' \describe{
#'   \item{df}{The original \code{df} with \code{batchDate2} appended.}
#'   \item{tmp.df}{The set of efficiency trials for the \code{TrapPositionID} of interest.}
#'   \item{m.i}{The number of efficiency trials within the time period of interest.}
#'   \item{initialVars}{The set of possible covariates available prior to model fitting.}
#'   \item{initialVarsNum}{An accounting data frame helpful in creating data frame \code{betas} following model fitting.}
#'   \item{all.ind.inside}{A logical vector indicating which \code{batchDates} fell within the temporal range spanned by valid efficiency trials.}
#' }
#' 
#' @examples 
#' \dontrun{
#' ans <- reduceETrials(df,possibleVars,bsplBegDt,bsplEndDt,trap,all.ind.inside)
#' }
reduceETrials <- function(df,possibleVars,bsplBegDt,bsplEndDt,trap,all.ind.inside){
  
  # df <- obs.eff.df
  # possibleVars <- possibleVars
  # bsplBegDt <- bsplBegDt
  # bsplEndDt <- bsplEndDt
  # trap <- trap
  # all.ind.inside <- all.ind.inside

  #   ---- Obtain necessary variables from the global environment.  
  time.zone <- get("time.zone",envir=.GlobalEnv)
  
  df <- df[ is.na(df$TrapPositionID) | (df$TrapPositionID == trap), ]
  ind <- !is.na(df$efficiency)
  
  initialVars <- c("(Intercept)",names(df)[!(names(df) %in% c("batchDate","nReleased","nCaught","efficiency","covar","batchDate2","fishDay"))])
  initialVarsNum <- c(2,as.integer(possibleVars %in% initialVars))
  
  # #   ---- Define these so we can model on (basically) fishing day of the season.  Do this with respect to 
  # #   ---- the year 1960 paradigm defined above.  We really only care about the number of days since the
  # #   ---- minimum start of fishing, over all years, so the year is immaterial.  Use 1960, or maybe 1959, 
  # #   ---- so we always keep this fact in mind.  Not sure this will always work... Need to check.  
  sign <- as.numeric(strftime(df$batchDate,format="%j")) - as.numeric(strftime(bsplBegDt,format="%j"))
  df$fishDay <- ifelse(sign == 0,0,
                       ifelse(sign  < 0,sign + 365,as.numeric(strftime(df$batchDate,format="%j"))))

  firstYear <- min(bsplBegDt$year + 1900)
  df$batchDate2 <- ifelse(sign >= 0,as.POSIXct(paste0(firstYear,"-",as.POSIXlt(df$batchDate)$mon + 1,"-",as.POSIXlt(df$batchDate)$mday),format="%Y-%m-%d",tz=time.zone),
                          ifelse(sign < 0,as.POSIXct(paste0(firstYear + 1,"-",as.POSIXlt(df$batchDate)$mon + 1,"-",as.POSIXlt(df$batchDate)$mday),format="%Y-%m-%d",tz=time.zone),
                                 NA))
  

  
  # #   ---- Find the difference in years between our efficiency setup in this run, as the mapped back-
  # #   ---- in-time 1959 (or maybe 1958 if the 1959 period starts early in the year, and has additionally
  # #   ---- been buffed to start in 1958.  This should be rare.) 
  # 
  # for(i in 1:99){
  #   
  #   bsplBegDtMMDD <- strftime(bsplBegDt,format="%m-%d")
  #   df
  #   
  #   thisYear <- df[bsplBegDt <= df$batchDate & df$batchDate + years(1) <= ,]
  #   
  #   
  #   theYearNow <- as.POSIXlt(df$batchDate[1])$year 
  #   theYearBeg <- rep(as.POSIXlt(bsplBegDt)$year,length(theYearNow))
  #   theYearDif <- theYearNow - theYearBeg
  #   
  #   #   ---- This 'years' function makes everything so much better!
  #   df$batchDate2 <- df$batchDate - lubridate::years(theYearDif)
  #   
  # }
  # 
  # theYearNow <- as.POSIXlt(df$batchDate[1])$year 
  # theYearBeg <- rep(as.POSIXlt(bsplBegDt)$year,length(theYearNow))
  # theYearDif <- theYearNow - theYearBeg
  # 
  # #   ---- This 'years' function makes everything so much better!
  # df$batchDate2 <- df$batchDate - lubridate::years(theYearDif)
  # 
  # 
  # head(df[df$TrapPositionID == "42010",c("batchDate","batchDate2")],600)
  
  #   ---- If we have a batchDate on 2/29, we have a problem, because 1970 wasn't a leap year.  
  df$batchDate2 <- as.POSIXct(df$batchDate2,format="%Y-%m-%d",tz="America/Los_Angeles",origin="1970-01-01 00:00:00 UTC")
  
  #   ---- Find the "season", which is between first and last trials
  #   ---- We look for 'inside' with respect to our 1959-1960 mapped year of trials.  
  strt.dt <- bsplBegDt #min( df$batchDate[ind], na.rm=T )  # Earliest date with an efficiency trial
  end.dt  <- bsplEndDt #max( df$batchDate[ind], na.rm=T )  # Latest date with efficiency trial
  ind.inside <- (strt.dt <= df$batchDate2) & (df$batchDate2 <= end.dt)
  inside.dates <- c(strt.dt, end.dt)
  all.ind.inside[[trap]] <- inside.dates  # save season dates for bootstrapping
  
  #   ---- The fitting data frame
  tmp.df <- df[ind & ind.inside,]
  m.i <- sum(ind & ind.inside)
  
  return(list(df=df,tmp.df=tmp.df,m.i=m.i,initialVars=initialVars,initialVarsNum=initialVarsNum,all.ind.inside=all.ind.inside))
}