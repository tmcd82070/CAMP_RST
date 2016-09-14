#' @export
#'
#' @title F.reclassifyLS
#'  
#' @description Reclassify lifeStage via forklength into groups specified via
#'   dataframe \code{forkLengthCutPoints}, as specified in the Global 
#'   Environment.
#'   
#' @param catch A catch dataframe originating from a catch-query sequence.  At
#'   the least, \code{catch} must contain variables entitled \code{forkLength}
#'   and \code{lifeStage}.  See function \code{F.get.catch.data}.
#' 
#' @return A dataframe with original variable \code{lifeStage} values containing
#'   "\code{Fry}," "\code{Parr}," and "\code{Smolt}" into the label values
#'   specified via the Global environment dataframe \code{forkLengthCutPoints}.
#'   See function \code{GlobalVars}.
#'   
#' @details Generally speaking, the process by which a new \code{lifeStage}
#'   variable is constructed is straight-forward.  Care must be taken, however,
#'   to ensure that records with no recorded \code{forkLength}, i.e., those
#'   containing a value of \code{NA}, are mapped to \code{"Unassigned"} in the
#'   new \code{lifeStage} variable.  This process is programmed in order to
#'   ensure correct processing of plus counts via function \code{F.assign.1dim},
#'   for records containing valid \code{FinalRun}, but not \code{lifeStage}, or
#'   \code{forkLength}, as the case may be.
#'   
#' @seealso \code{GlobalVars}, \code{F.get.catch.data}, \code{F.assign.1dim}
#'   
#' @examples  
#' \dontrun{
#' catchWithNewLifeStageCats <- reclassifyLS(catch)
#' }

F.reclassifyLS <- function(catch){
  
  # catch <- catch
  
  forkLengthCutPoints <- get("forkLengthCutPoints",envir=.GlobalEnv)
  
  #   ---- Dataframe forkLengthCutPoints should be in the Global Environment.  Now we use it to get its cut points.  
  #   ---- We pull out the labels and cutpoints separately.  Note that we add in the "0" to the cut points vector
  #   ---- manually. We also add a "-100" since we are going to map NAs in forkLength to -99.  This construction
  #   ---- ensures that NA forkLength get mapped to their own group -- this becomes "Unassigned".  
  newLSLabels <- c("Unassigned",as.character(droplevels(forkLengthCutPoints$lifeStage)))
  newLScutPoints <- c(-100,0,forkLengthCutPoints$cutPoints)
  
  #   ---- Check to make sure user-specified dataframe forkLengthCutPoints is at least numeric.  
  if( !is.numeric(newLScutPoints) ){
    stop("Dataframe forkLengthCutPoints doesn't contain numeric entries in its second column.  Investigate.")
  }
  
  #   ---- Check to make sure user-specified dataframe forkLengthCutPoints covers the data.
  maxFL <- max(catch[!is.na(catch$forkLength),]$forkLength)
  if( maxFL > max(forkLengthCutPoints$cutPoints) ){
    stop("Maximum specified cut point in dataframe forkLengthCutPoints doesn't cover the maximum forklength of ",maxFL," in the requested data.")
  }
  
  #   ---- Get the number of rows before we do magic.  We check this number with the same after we're done.  
  nCatch <- nrow(catch)
  
  #   ---- Break out the two distinct catch types. 
  catchGood <- catch[catch$TrapStatus == "Fishing",]
  catchBad  <- catch[catch$TrapStatus == "Not fishing",]
  
  #   ---- Remap the forklengths, and sneak it into the lifeStage variable.  Note that we must allow the 
  #   ---- possibility that a forklength is missing, even though the original lifestage was recorded.
  #   ---- This means that we must map these missing NAs (in forklength) to "Unassigned".  
  #catchGood[2,]$forkLength <- NA    # <---- for testing.
  catchGood$forkLength <- ifelse(is.na(catchGood$forkLength),-99,catchGood$forkLength)
  catchGood$lifeStage <- cut(catchGood$forkLength,breaks=newLScutPoints,labels=newLSLabels,right=TRUE)
  
  #   ---- Put it all together again.  I use function factor to update the levels in 
  #   ---- the factor variable lifeStage. 
  catch <- rbind(catchGood,catchBad)
  catch$lifeStage <- factor(catch$lifeStage)
  
  #   ---- Now, put forklength back to NA.  At this point, it still has a -99, which isn't good.
  catch$forkLength <- ifelse(catch$forkLength == -99,NA,catch$forkLength)
  
  #   ---- Check to make sure slicing and dicing didn't lose or gain anything.
  nCatchAgain <- nrow(catch)
  if( nCatch != nCatchAgain ){
    stop("The creation of a new lifeStage variable based on forklength has resulted in error in function F.reclassify, line 75.  Investigate.\n")
  }
  
  #   ---- Summarize to get unique combinations of FinalRun, lifeStage, and forkLength.
  catchNames <- names(catch)
  catchF <- catch[catch$TrapStatus == "Fishing",]
  catchF$forkLength <- ifelse(is.na(catchF$forkLength),-99,catchF$forkLength)
  catch$forkLength <- ifelse(is.na(catch$forkLength),-99,catch$forkLength)
  
  catchFsummary <- aggregate(catchF$Unmarked,by=list(catchF$trapVisitID,catchF$FinalRun,catchF$lifeStage,catchF$forkLength,catchF$RandomSelection),sum)
  names(catchFsummary) <- c('trapVisitID','FinalRun','lifeStage','forkLength','RandomSelection','newUnmarked')
  catchNewUnmarked <- merge(catch,catchFsummary,by=c('trapVisitID','FinalRun','lifeStage','forkLength','RandomSelection'),all.x=TRUE)
  catchNewUnmarked$Unmarked <- NULL
  names(catchNewUnmarked)[names(catchNewUnmarked) == "newUnmarked"] <- "Unmarked"
  catchNewUnmarkedUnique <- unique(catchNewUnmarked)
  catchN <- catchNewUnmarkedUnique
  
  #   ---- Now, put forklength back to NA.  At this point, it still has a -99, which isn't good.
  catchN$forkLength <- ifelse(catchN$forkLength == -99,NA,catchN$forkLength)
  
  #   ---- Re-order the columns to reflect their traditional format.
  catchN <- catchN[,catchNames]
  
  #   ---- And finally, place back to the data frame we need.
  catch <- catchN
  
  #   ---- Take a look at the remapping, so as to ensure correctness.  Eventually delete. 
  # table(catch$forkLength,catch$lifeStage,exclude=NULL)
  # table(catch[catch$TrapStatus == "Fishing" & catch$FinalRun != "Unassigned",]$forkLength,catch[catch$TrapStatus == "Fishing" & catch$FinalRun != "Unassigned",]$lifeStage,exclude=NULL)
  # catch44[catch44$TrapStatus == "Fishing" & catch44$FinalRun != "Unassigned" & is.na(catch44$forkLength),]
  # catch[catch$TrapStatus == "Fishing" & catch$FinalRun != "Unassigned" & is.na(catch$forkLength),]
  
  return(catch)
  
}