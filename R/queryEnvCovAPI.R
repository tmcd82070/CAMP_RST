#' @title Query the Environmental Covariate API
#'   
#' @description  Query the Environmental Covariate API for data between two
#'   dates.
#'   
#' @param min.date The start date for data to include. This is a text string in 
#'   the format \code{\%Y-\%m-\%d}, or \code{YYYY-MM-DD}.  Queries on the Unit 
#'   table \code{tblu} assume a start time of midnight on the day specified.
#'   
#' @param max.date The end date for data to include.  Same format as 
#'   \code{min.date}.  Queries on the Unit table \code{tblu} assume an end time 
#'   of midnight on the day specified.
#'   
#' @param oursitevar An integer indicating the site identifier.  These identifiers
#'   must be the custom ones designed specifically for the Environmental 
#'   Covariate Database.
#'   
#' @param type A text string indicating if either Daily \code{"D"} or Unit 
#'   \code{"U"} values should be queried.  At present, only "D" is implemented.
#'   
#' @details    
#'   For a given time interval (e.g., day or hour), a site may have more than one statistic
#'   recorded.  Statistics are mean, median, max, or min over the time point.  
#'   The statistic returned by this routine for time intervals with multiple statistics
#'   is determined by the following rules, applied in order of precedence: 
#'   \enumerate{
#'     \item median is selected before the mean
#'     \item mean is selected before the max or min
#'     \item if max AND min are available (but neither median or mean), 
#'     an average of min and max is returned
#'     \item if only one of max or min is available, it is returned.  
#'   }
#'   
#' @return A data frame of the query results, with a fields for both 
#'   flow and temperature values, what statistics are returned, and date or 
#'   time interval. The returned data frame has the following fields: 
#'   \itemize{
#'     \item \code{date} = date of the measurement in POSIXct format.
#'     \item \code{flow_statistic} = an integer specifying the type of statistic 
#'     returned for flow (1 = maximum; 2 = minimum; 3 = mean; 8 = median).
#'     \item \code{flow_statistic_name} = the name of the flow statistic.  
#'     "Maximum", "Minimum", "Mean", "Median" as specified by \code{flow_statistic}. 
#'     \code{flow_statistic_name} and \code{flow_statistic} are redundant, the former 
#'     is included for convenience. 
#'     \item \code{flow_cfs} = the value of flow statistic in cubic feet per second. 
#'     \item \code{temp_statistic} = an integer specifying the type of statistic 
#'     returned for flow (1 = maximum; 2 = minimum; 3 = mean; 8 = median).
#'     \item \code{temp_statistic_name} = the name of the flow statistic.  
#'     "Maximum", "Minimum", "Mean", "Median" as specified by \code{temp_statistic}. 
#'     \code{temp_statistic_name} and \code{temp_statistic} are redundant, the former 
#'     is included for convenience. 
#'     \item \code{temp_cfs} = the value of flow statistic in cubic feet per second. 
#'   }
#'   
#' @author Trent McDonald and Jason Mitchell 
#'   
#' @seealso \code{plot2Panel}
#'   
#' @examples 
#' \dontrun{
#' #   ---- Query for daily results.  
#' tmp <- queryEnvCovAPI(min.date = "2000-01-01",
#'               max.date = "2010-12-31",
#'               oursitevar = 11,
#'               type = "D")
#' }
#' 
#' @export
#' @importFrom magrittr %>% 
#' 
queryEnvCovAPI <- function(min.date,
                          max.date,
                          oursitevar,
                          type = "D"){

  cat("------ In queryEnvCovAPI -----------------\n")
  cat(paste("min.date =", min.date, "\n"))
  cat(paste("max.date =", max.date, "\n"))
  cat(paste("oursitevar =", oursitevar, "\n"))
  
  # Check for valid date even though the API does this too.
  # This does not check for out of range dates.  E.g., "2001-2-30" is valid.
  # But, the API does check for out or range dates. 
  
  dateRegex <- "^(19|20)[0-9]+-(0[1-9]|1[0-2]|[1-9])-(0[1-9]|[12][0-9]|3[01]|[1-9])$"
  if(!grepl(dateRegex, min.date)){
    stop(paste("Invalid min.date:", min.date, "Format must be YYYY-MM-DD."))
  }
  if(!grepl(dateRegex, max.date)){
    stop(paste("Invalid max.date:", max.date, "Format must be YYYY-MM-DD."))
  }
  
  urlQueryList <-  list(scheme = "http", 
                    hostname="api.streamnet.org", 
                    path="api/v1/rst.json", 
                    query=list(xapikey = "B1AE7557-6F19-4B50-9CA4-D4E766E1A491",
                               startdate = min.date,
                               enddate= max.date,
                               sitecode = oursitevar
                    ))
  class(urlQueryList) <- "url"
  urlQuery <- httr::build_url(urlQueryList)
  
  
  urlResult <- tryCatch({
    httr::GET(urlQuery)
  },
  error=function(cond){
    cat(crayon::red(paste0("Failure: \n", cond, "\n")))
    NULL
  },
  warning=function(cond){
    cat(crayon::red(paste0("WARNING: ", cond, "\n")))
    NULL
  }
  )  
  
  cat(paste0("Returned API status code: ", urlResult$status_code, "\n"))

  if( !is.null(urlResult) ){

    # I could make the 'stop' in these if statements into 'warning' and 
    # still return NULL.  For now, I think it better to stop.

    tbl <-  suppressWarnings( {jsonlite::fromJSON(httr::content(urlResult, 
                                                                as = "text")) })
    
    if( 200 <= urlResult$status_code & urlResult$status_code < 300 ){
      # we are good
      
      # --- debuggin 
      cat("Head of env data frame returned by API:\n")
      print(head(tbl))
      print(str(tbl))
      cat("------ end debug in queryEnvCovAPI -----------------\n")
      # --- debuggin 
      
      
      tbl$date <- as.POSIXct(tbl$date, format = "%Y-%m-%d", tz = "America/Los_Angeles")
      tbl <- tbl %>% dplyr::select(
        date,
        dplyr::starts_with("flow_"), 
        dplyr::starts_with("temp_")
      )
      attr(tbl, "oursitevar") <- oursitevar
    } else if( urlResult$status_code == 401 ){
      stop(paste("One or more invalid dates. Found", min.date, "and", max.date))
      # tbl <- NULL
    } else if( urlResult$status_code == 402 ){
      stop(paste("Cannot find site code ", oursitevar))
      # tbl <- NULL
    } else if( urlResult$status_code == 403 ){
      stop(paste("min.date must come before max.date. min.date =", min.date, "max.date =", max.date))
      # tbl <- NULL
    } else {
      # whoops, Something else is wrong on the API side
      cat(tbl$DETAIL)
      stop(paste0("API ERROR: Usually these errors are temporary and due to server issues.\n", 
                  "Please re-run the same CAMP analysis multiple times after some time has passed.\n",
                  "If this error persists, contact CAMP support."))
    }
  } else {
    stop(paste(urlQueryList$hostname, "is down or unreachable."))
    # tbl <- NULL
  }
    
  return(tbl)
}
