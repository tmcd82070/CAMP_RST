#' @export
#' 
#' @title F.assign.2dim
#' 
#' @description Assign the missing counts of both \code{FinalRun} and \code{lifeStage} when 
#' the number fish in variable \code{Unmarked} is greater than zero.
#' 
#' @param catch A data frame containing records of fish itemized by combinations
#'   of variables \code{FinalRun}, \code{lifeStage}, and \code{forkLength}. 
#'   Variable \code{Unmarked} contains the number of fish represented in each 
#'   record.  Use of this function assumes that at least one record in 
#'   \code{FinalRun} and \code{lifeStage} both equal \code{"Unassigned"}.
#' @param var1 The variable in data frame \code{catch} for which data are 
#'   recorded.  Function \code{F.assign.2dim}, as utilized in the estimation of
#'   passage, assumes \code{var1="FinalRun"}.
#' @param var2 The variable in the data frame \code{catch} for which data are 
#'   not recorded.  Function \code{F.assign.2dim}, as utilized in the estimation
#'   of passage, assumes \code{var2="lifeStage"}.
#'   
#' @return A data frame \code{catch} containing estimates of the number of fish 
#'   with respect to both variables \code{var1} and \code{var2}, based on their 
#'   joint distribution.
#'   
#' @details Function \code{F.assign.2dim} makes a new variable, based on 
#'   \code{FinalRun} and \code{lifeStage}, with joint levels.  It then utilizes 
#'   function \code{F.assign.1dim} on the new joint variable, via its 
#'   \code{present.var} argument.  Levels fed to \code{present.var} take the 
#'   form \code{FinalRun.lifeStage}, e.g., \code{"Fall.Smolt"} for observed 
#'   records and \code{"Unassigned"} for unobserved.  Levels fed to 
#'   \var{absent.var} of \code{F.assign.1dim} are either \code{"Yes"} for 
#'   observed records, or \code{NA} for those for which plus-counting is 
#'   necessary, i.e., where \code{present.var="Unassigned"}.  Function 
#'   \code{F.assign.1dim} then allocates plus-counts based on the distribution 
#'   of observed joint frequencies in variable \code{present.var}.  Following 
#'   allocation to the joint values, function \code{F.assign.2dim} then "cleans 
#'   up" by breaking out the joint distribution into its constituent parts
#'   
#'   Thus, for example, \code{"Fall.Smolt"} becomes \code{"Fall"} and
#'   \code{"Smolt"} within variables \code{FinalRun} and \code{lifeStage},
#'   respectively, with the appropriate count of plus-count fish recorded in
#'   variable \code{Unmarked}.
#'   
#' 
#' @examples 
#' 
F.assign.2dim <- function(catch, var1, var2 ){

  # catch <- catch
  # var1 <- "FinalRun"
  # var2 <- "lifeStage"
  
  jointLevs <- paste(catch[,var1], catch[,var2], sep=".")
  
  #   ---- Will change this later.  This makes sure reassignment immediately 
  #   ---- below via grep does not change these.
  jointLevs[ jointLevs == "Unassigned.Unassigned" ] <- "SAVED" 
  
  #   ---- These are things like "Spring.Unassigned", which could not be 
  #   ---- assigned via function F.assign.1dim.  In other words, they failed
  #   ---- all six strategies.  We want to leave these.  
  jointLevs[ grep("Unassigned", jointLevs) ] <- "NA"  
  
  #   ---- Change NA.NA to just NA.
  jointLevs[ grep("NA", jointLevs) ] <- NA  
  
  #   ---- Change back to lower-case so function F.assign.1dim will recognize.
  jointLevs[ jointLevs == "SAVED" ] <- "Unassigned"  
  
  jointLevs <- factor(jointLevs)
  
  JointDist <- rep(1,nrow(catch))
  JointDist <- factor(JointDist, levels=1, labels=c("Yes"))
  
  #   ---- When joint levels is missing, joint distribution is missing.
  #   ---- These NAs result from Unassigned failing via the six strategies
  #   ---- in function F.assign.1dim already called.  
  JointDist[ is.na(jointLevs) ] <- NA  
  
  #   ---- Variable jointLevs are the concatenated levels, e.g., Fall.Fry.  
  #   ---- Variable jointDist can only be one of Yes or NA.  
  catch$jointLevs <- jointLevs
  catch$JointDist <- JointDist
  
  #   ---- Call the routine F.assign.1dim that assigns plus counts based on a 
  #   ---- 1-dimensional distribution.    
  catch <- F.assign.1dim( catch, present.var="JointDist", absent.var="jointLevs" )
  
  #   ---- Now, put the new assignments back in FinalRun and lifeStage.
  jointLevs <- paste(catch[,var1], catch[,var2], sep=".")
  ind <- jointLevs == "Unassigned.Unassigned" 
  newAssigned <- as.character(catch$jointLevs[ind])
  newAssigned <- strsplit(newAssigned, ".", fixed=T)
  newrun <- unlist(lapply( newAssigned, "[", 1))
  newlife <- unlist(lapply( newAssigned, "[", 2))
  catch$FinalRun[ind] <- newrun
  catch$lifeStage[ind] <- newlife
  
  #   ---- Finally, drop the columns we created.     
  catch <- catch[, !(names(catch) %in% c("jointLevs", "JointDist"))]  
  
  catch
  
}
