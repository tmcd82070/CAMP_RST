#' @export
#' 
#' @title F.expand.plus.counts
#' 
#' @description Expand a catch data frame to account for plus counts.
#' 
#' @param catch A data frame containing missing data, i.e., values of
#'   \code{"Unassigned"}, for variables \code{FinalRun} and/or \code{lifeStage}.
#'   
#' @return A data frame with fish tallied via variable \code{Unmarked}, whose 
#'   \code{FinalRun} and/or \code{lifeStage} was recorded as 
#'   \code{"Unassigned"}, reassigned to a valid run and/or life stage observed
#'   in the data.
#'   
#' @details Data frame \code{catch} contains records with missing data for one 
#'   or both of variables \code{FinalRun} and \code{lifeStage}.  In these
#'   variables, these records have a value of \code{"Unassigned"}. 
#'   Additionally, variable \code{Unmarked} quantifies the number of fish whose
#'   run and/or life stage is unknown.  Together, these two variables identify
#'   the count and type of fish not measured during sampling.  
#'   
#'   Accurate estimation of passage requires the use of all fish in a catch. 
#'   Sometimes however, catch can number in the thousands.  Practically, these 
#'   large numbers of fish cannot be individually examined to determine life 
#'   stage and run.  To quantify the underlying proportions of life stage and 
#'   run on the catch as a whole, a random selection is drawn.  From this random
#'   selection, the proportions of individual life stages and runs are 
#'   quantified via observation.  Plus counts result from applying the 
#'   proportions of life stage and run obtained via sampling to the unsampled 
#'   fish.
#'   
#'   Sometimes, when examining an individual fish for life stage and run, only 
#'   one attribute of the two is recorded.  Thus, following sampling and 
#'   examination, a caught fish falls into one of four separate scenarios: 
#'   \enumerate{ \item both life stage and run are recorded; \item life stage
#'   only is missing, but run is recorded; \item run only is missing, but life
#'   stage is recorded; \item both life stage and run are not recorded. } The
#'   first three scenarios result from a fish being randomly sampled, while the
#'   fourth results from a fish not being a part of the sample.
#'   
#'   Function \code{F.assign.1dim} assigns missing life stage when run is 
#'   recorded.  The same function does the same when run is missing but life 
#'   stage is recorded. Function \code{F.assign.2dim} assigns both missing life 
#'   stage and run.  Thus, function \code{F.assign.1dim} is the workhorse 
#'   function for cases 2 and and 3, while \code{F.assign.2dim} is the same for 
#'   case 4.  Case 1 requires no plus counting, since life stage and run are 
#'   known.  Function \code{F.expand.plus.counts} calls each of these functions 
#'   in turn, with each of the one-dimensional calls performed first via
#'   \code{F.assign.1dim} followed by one call to \code{F.assign.2dim}.
#'   
#'   The plus-count algorithm assumes that all unsuccessful or non-fishing 
#'   visits have been removed from data frame \code{catch} prior to 
#'   implementation. Variable \code{Unmarked} identifies each of these;  a 
#'   \code{0} indicates an unsuccessful visit, while missing corresponds to 
#'   non-fishing.
#'   
#' @seealso \code{F.assign.1dim}, \code{F.assign.2dim}
#'   
#' @author WEST Inc.
#' 
#' @examples 
#' \dontrun{
#' #   ---- Expand unassigned run and life stage fish via the proportions
#' #   ---- observed in a random selection, per trapping instance.  
#' newCatch <- F.expand.plus.counts(catch)
#' }

F.expand.plus.counts <- function( catch ){
  #
  # catch <- catch
  #

  #   ---- On entry, there cannot be any rows with Unmarked = 0 or missing. 
  #   ---- The Unmarked == 0 correspond to unsuccessful visits.  Missing 
  #   ---- Unmarked correspond to non-fishing.  Eliminate these if they exist. 
  catch <- catch[ (catch$Unmarked > 0) & !is.na(catch$Unmarked) , ]

  cat("----------- Assigning run and lifestage based on Plus Counts----------\n")
  cat("Number of records over season in run X lifestage matrix upon entry:\n")

  print( table(FinalRun=catch$FinalRun, lifeStage=catch$lifeStage, useNA="always") )

  #   ---- Assign plus counts one dimension at a time, then for the doubly missing.

  #   ---- Missing run, but present lifestage.  So, assign a run.
  cat("---- lifeStage present, FinalRun absent\n")
  catch <- F.assign.1dim( catch, present.var="lifeStage", absent.var="FinalRun" )
  cat("\n")
  
  #   ---- Missing lifeStage, but present finalRun.  So, assign a life stage.
  cat("---- FinalRun present, lifeStage absent\n")
  catch <- F.assign.1dim( catch, present.var="FinalRun", absent.var="lifeStage" )
  cat("\n")
  
  #   ---- Missing lifeStageID AND missing finalRunID.  So, assign both life stage and run.
  cat("---- FinalRun absent AND lifeStage absent\n")
  if( any( (catch$FinalRun == "Unassigned") & (catch$lifeStage == "Unassigned") & (!is.na(catch$FinalRun)) & (!is.na(catch$lifeStage)) ) ){
    catch <- F.assign.2dim( catch, var1="FinalRun", var2="lifeStage" )
  } else {
    cat( "No Unassigned-Unassigned combinations of FinalRun and lifeStage\n" )
  }

  cat("Final number of records in run X life stage table:\n")
  print( table(FinalRun=catch$FinalRun, lifeStage=catch$lifeStage, useNA="always") )
  cat("----------- DONE assigning plus Counts----------\n")

  catch
}
