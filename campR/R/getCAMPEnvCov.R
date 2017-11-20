#'@export
#'
#'@title getCAMPEnvCov
#'  
#'@description Query individual covariates and their recorded units from an
#'  already queried Access CAMP database.
#'
#'@param dbCov The data frame resulting from a query against the
#'  \code{EnvDataRaw} table in a CAMP mdb, following unit-code clean-up via the
#'  QryCleanEnvCov SQL sequence.
#'@param cov The text string name of the environmental covariate in the
#'  \code{EnvDataRaw} table of the CAMP Access database.
#'@param covID  The text string name of the environmental covariate units
#'  variable tied to \code{cov} in the \code{EnvDataRaw} table of the CAMP
#'  Access database.
#'@param unitID A single number describing the CAMP Access unit code appropriate
#'  for the provided \code{cov} and \code{unitID}. These derive from lookup
#'  table \code{luUnit} in the CAMP database.
#'  
#'@return A data frame containing all the records from table \code{EnvDataRaw}
#'  in the CAMP database.  Only records with a \code{covID} record matching the
#'  \code{unitID} are returned, although all unique values encountered are saved
#'  as attribute \code{"uniqueUnitID"}.
#'  
#'@details Qualitative variables, which currently only includes
#'  \code{"weather"}, do not subset to values with a valid \code{unitID}, if
#'  only because these don't exist.  Alternatively, a lookup table of all
#'  weather values ever recorded is used to map to one of four ordinal values of
#'  increasing cloudiness.
#'  
#'@seealso \code{estCovar}
#'  
#'@author WEST Inc.
#'  
#' @examples
#' \dontrun{
#' getCAMPEnvCov(cov,covID,unitID)
#'}
getCAMPEnvCov <- function(dbCov,cov,covID,unitID){
  
  # dbCov <- dbCov
  # cov <- "weather"
  # covID <- NA
  # unitID <- NA
  
  #   ---- See if the metric of interest has any records.  If so, report the UnitID, if it exists for cov.
  if(sum(!is.na(dbCov[,cov])) > 0 & !is.na(unitID)){
    cat(paste0("Covariate '",cov,"' has these unique UnitID values: ",paste0(sort(na.omit(unique(dbCov[,covID]))),collapse=", "),".\n"))
  } else if(!is.na(unitID)) {
    cat(paste0("No valid records of covariate '",cov,"' found.\n"))
  }
  
  #   ---- Some covariates, like weather, do not have a covID.  
  if(!is.na(covID)){
    dbCov2 <- dbCov[!is.na(dbCov[,cov]),c("subSiteID","measureDate",cov,covID)]  # 11/20/2017:  Before, this was measureTime.  I feel like that was better.
    dbCov2 <- dbCov2[dbCov2[,covID] == unitID,]
  } else {
    dbCov2 <- dbCov[!is.na(dbCov[,cov]),c("subSiteID","measureDate",cov)]        # 11/20/2017: Before, this was measureTime.  I feel like that was better.
    #dbCov2 <- dbCov2[dbCov2[,covID] == unitID,]
  }
  dbCov2 <- dbCov2[order(dbCov2$subSiteID,dbCov2$measureDate),]                  # 11/20/2017: Before, this was measureTime.  I feel like that was better.
  
  #   ---- Put as an attribute the unique covID values, and cov so these can be summarized. 
  attr(dbCov2,"cov") <- cov
  if(!is.na(covID)){
    attr(dbCov2,"uniqueUnitID") <- sort(na.omit(unique(dbCov[,covID])))
  } else {
    attr(dbCov2,"uniqueUnitID") <- NA
  }
  return(dbCov2)
}

