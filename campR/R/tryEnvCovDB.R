#' @export
#' 
#' @title tryEnvCovDB
#'   
#' @description Query the Environmental Covariate Database for other users prior
#'   to creating internal database tables.
#'   
#' @param nTries The number of attempts to query the Environmental Covariate
#'   Database.
#'   
#' @param secSleep The number of seconds to wait between attempts.
#' 
#' @param ch A previously defined connection to the Environmental Covariate
#'   Database utilizing \code{RPostgres::dbConnect}.
#'   
#' @details The function is a simple \code{repeat}-type loop, running at most
#'   \code{nTries} times.
#'   
#' @return The function is nearly silent.  It simply reports messages to the log
#'   file indicating connection attempts.  In the case a connection is
#'   successful, a message of success is recorded.
#'   
#' @author WEST, Inc.
#'   
tryEnvCovDB <- function(nTries,secSleep,ch){

  # nTries <- 24
  # secSleep <- 5

  checkForOthers <- RPostgres::dbSendQuery(ch,"SELECT COUNT(usename) FROM pg_stat_activity WHERE usename = 'envcovread';")
  nOthers <- RPostgres::dbFetch(checkForOthers) - 1
  RPostgres::dbClearResult(checkForOthers)

  if(nOthers == 0){
    cat(paste("You are the only 'envcovread'.  Proceed to query.\n"))
  } else if(nOthers >= 1){
    try <- 0
    repeat{  
      
      #   ---- We try 24 times, or 120 seconds = 2 minutes.  
      try <- try + 1
      if(try <= nTries){
        if(nOthers == 0){
          cat(paste0("You are now the only 'envcovread'.  Proceed to query.\n"))
          break
        } else if(nOthers == 1){
          cat(paste0("Another 'envcovread' is signed in.  This is try number ",try,". I will wait 5 seconds and check again.\n"))
        } else if(nOthers >= 2){
          cat(paste0("At least two other 'envcovread's are signed in.  This is try number ",try,".  I will wait 5 second and try again.\n"))
        }
        
        #   ---- See if we can get in now.
        Sys.sleep(secSleep)
        checkForOthers <- RPostgres::dbSendQuery(ch,"SELECT COUNT(usename) FROM pg_stat_activity WHERE usename = 'envcovread';")
        nOthers <- RPostgres::dbFetch(checkForOthers) - 1
        RPostgres::dbClearResult(checkForOthers)
        
      } else {
        cat(paste0("I cannot get into the Environmental Covariate Database after 24 attempts.  Contact the CAMP Administrator or try again later.\n"))
        stop()
      }
    }
  } 
}