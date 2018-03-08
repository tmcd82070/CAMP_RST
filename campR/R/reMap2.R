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
      if(min.date.p$mon < strt.dt$mon){
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
  
  ans <- list(strt.dt,end.dt)
  return(ans)
}