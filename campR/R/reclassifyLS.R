#' @export
#'
#' @title F.reclassifyLS
#'  
#' @description Reclassify lifeStage via forklength into groups specified
#' via Global Enviornment dataframe \code{forkLengthCutPoints}.
#' 
#' @param catch A catch dataframe originating from a catch-query sequence.  See
#' function \code{F.get.catch.data}.  At the least, \code{catch} must contain
#' variables entitled \code{forkLength} and \code{lifeStage}.
#' 
#' @return A dataframe with original variable \code{lifeStage} values containing
#'   "\code{Fry}," "\code{Parr}," and "\code{Smolt}" into the label values
#'   specified via the Global environment dataframe \code{forkLengthCutPoints}.
#'   See \code{GlobalVars}.
#'   
#' @details 
#'   
#' @seealso \code{GlobalVars.R}, \code{F.get.catch.data}
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
  #   ---- manually.  
  newLSLabels <- forkLengthCutPoints$lifeStage
  newLScutPoints <- c(0,forkLengthCutPoints$cutPoints)
  
  #   ---- Check to make sure user-specified dataframe forkLengthCutPoints is at least numeric.  
  if( !is.numeric(newLScutPoints) ){
    stop("Dataframe forkLengthCutPoints doesn't contain numeric entries in its second column.  Investigate.")
  }
  
  #   ---- Check to make sure user-specified dataframe forkLengthCutPoints covers the data.
  maxFL <- max(catch[!is.na(catch$forkLength),]$forkLength)
  if( maxFL > max(forkLengthCutPoints$cutPoints) ){
    stop("Maximum specified cut point in dataframe forkLengthCutPoints doesn't cover the maximum forklength of ",maxFL," in the requested data.")
  }
  
  #   ---- Remap the forklengths, and sneak it into the lifeStage variable.  
  catch$lifeStage <- cut(catch$forkLength,breaks=newLScutPoints,labels=newLSLabels,right=TRUE)
  
  #   ---- Take a look at the remapping, so as to ensure correctness.  
  # table(catch$forkLength,catch$lifeStage,exclude=NULL)
  
  return(catch)
  
}