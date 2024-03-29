#' @export
#' 
#' @title reMap2
#'   
#' @description Map the dates of an enhanced efficiency model fit back to the
#'   present, as defined via \code{min.date} and \code{max.date}.
#'   
#' @param min.date The start date for data to include. This is a text string in 
#'   the format \code{\%Y-\%m-\%d}, or \code{YYYY-MM-DD}.
#'   
#' @param max.date The end date for data to include.  Same format as 
#'   \code{min.date}.
#'   
#' @param splineDays The set of spline days in the 1959-1960 paradigm on which 
#'   enhanced efficiency models were fit.
#'   
#' @return A data frame with all observed and imputed \code{efficiency} values, 
#'   where variable \code{gam.estimated} identifies days with imputed values.
#'   
#' @seealso \code{reMap}
#'   
#' @author WEST Inc.
#'   
#' @examples 
#' \dontrun{
#' reMap2( min.date,max.date,splineDays )
#' }
reMap2 <- function(min.date,max.date,splineDays){
  
  # min.date <- min.date
  # max.date <- max.date 
  # splineDays <- splineDays

  time.zone <- get("time.zone", .GlobalEnv )
  
  #   ---- Find the "season", which is between first and last OVERALL efficiency trials.  This is different
  #   ---- than "regular" eff models, where we define the "season" as first and last eff trials, as defined
  #   ---- within the provided min.date and max.date.  
  min.date.p <- as.POSIXlt(strptime(min.date,format="%Y-%m-%d"),format="%Y-%m-%d",tz="UTC")
  max.date.p <- as.POSIXlt(strptime(max.date,format="%Y-%m-%d"),format="%Y-%m-%d",tz="UTC")
  
  yr.m <- as.POSIXlt(strptime(min.date,format="%Y-%m-%d"),format="%Y-%m-%d",tz="UTC")$year
  yr.M <- as.POSIXlt(strptime(max.date,format="%Y-%m-%d"),format="%Y-%m-%d",tz="UTC")$year
 
  strt.dt <- as.POSIXlt(min(splineDays))   # Earliest date with an efficiency trial 1960 paradigm
  end.dt  <- as.POSIXlt(max(splineDays))   # Latest date with efficiency trial 1960 paradigm
  
  #   ---- Re-map 1959-1960 data to the correct year we care about.  
  #   ---- Spline data all in 1960.  
  if(strt.dt$year == 60 & end.dt$year == 60){
    if(yr.m == yr.M){
      strt.dt$year <- yr.m
      end.dt$year <- yr.M
    } else if(yr.m != yr.M){
      strt.dt$year <- yr.m + 1      # We know spline start and end have same year, so min.date must be back one.
      end.dt$year <- yr.M
    }
  }

  #   ---- Spline data start in 1959, but end in 1960.
  if(strt.dt$year == 59 & end.dt$year == 60){
    if(yr.m == yr.M){
      if(min.date.p$mon == strt.dt$mon){
        # do nothing.  added 6/21/2018.  when strt.dt min.dt month and year both equal. 
        
        strt.dt$year <- yr.m
        end.dt$year <- yr.M + 1       # max.date year needs to get upped by 1 to keep tabs with strt.dt end.dt
        
      } else if(min.date.p$mon < strt.dt$mon){
        strt.dt$year <- yr.m - 1
        end.dt$year <- yr.M
      } else {
        strt.dt$year <- yr.m + 1      # Make up the missing 1 year, because starting in 1959.
        end.dt$year <- yr.M
      }
    } else if(yr.m != yr.M){
      strt.dt$year <- yr.m          # Do not make up the missing year, because we don't need to.
      end.dt$year <- yr.M
    }
  }
  
  #   ---- If max(splineDays) is leap day 1960, but we are ultimately going to map back
  #   ---- to a non-leap year, then R will push to March 1st.  This screws things up.  Push back 
  #   ---- end.dt by one day.  I don't know what to do for strt.dt...leave this for now.  
  
  #   ---- We only need to make the correction if it's NOT a leap year.  If min.date -- max.date 
  #   ---- encompasses a leap day, we sure don't want to get rid of it.  
  checkLeapDay <- as.POSIXlt(seq.POSIXt(as.POSIXct(min.date,format="%Y-%m-%d",tz=time.zone),
                                        as.POSIXct(max.date,format="%Y-%m-%d",tz=time.zone),
                                        by="1 DSTday"))
  
  weHaveALeapDay <- 0
  if( sum(checkLeapDay$mon == 1 & checkLeapDay$mday == 29) == 0 ){
    weHaveALeapDay <- 1 
  }
  
  maxSplineDayslt <- as.POSIXlt(max(splineDays),format="%Y-%m-%d")
  if( (maxSplineDayslt$mon == 1 & maxSplineDayslt$mday == 29) & weHaveALeapDay == 1) {
    end.dt <- end.dt - 24*60*60
  }
  
  ans <- list(strt.dt=strt.dt,end.dt=end.dt)
  return(ans)
}