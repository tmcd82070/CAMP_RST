#' @export F.lifestage.passage.assignLS3groupNoWeight
#' 
#' @title F.lifestage.passage.assignLS3groupNoWeight
#' 
#' @description
#' 
#'    ANNUAL PRODUCTION ESTIMATES BY LIFE STAGE AND RUN ? TABULAR SUMMARY
#'    A table of passage estimates, with lifestages down the rows, and runs across the columns.
#' 
#'    Input:
#'    site = site ID of the place we want, trap locaton
#'    taxon = taxon number (from luTaxon) to retrieve
#' 
#' 
#' 
#'  3 life stage groups are fit and weight will not be used to assign lifestage
#' 
#' @param site <describe argument>
#' @param  taxon <describe argument>
#' @param  min.date <describe argument>
#' @param  max.date <describe argument>
#' @param  output.file <describe argument>
#' @param  ci=TRUE <describe argument>
#' 
#' @details <other comments found in file>
#' NA
#' NA
#' NA
#'  3 life stage groups are fit and weight will not be used to assign lifestage
#' 
#' @return <describe return value>
#' 
#' @author WEST Inc.
#' 
#' @seealso \code{\link{<related routine>}}, \code{\link{<related routine>}}
#' 
#' @examples
#' \dontrun{
#' <insert examples>
#' 
#' }
F.lifestage.passage.assignLS3groupNoWeight <- function(site, taxon, min.date, max.date, output.file, ci=TRUE,autoLS=TRUE){
  ###
  ###   ANNUAL PRODUCTION ESTIMATES BY LIFE STAGE AND RUN TABULAR SUMMARY
  ###   A table of passage estimates, with lifestages down the rows, and runs across the columns.
  ###
  ###   Input:
  ###   site = site ID of the place we want, trap locaton
  ###   taxon = taxon number (from luTaxon) to retrieve
  ###
  
  
  ## 3 life stage groups are fit and weight will not be used to assign lifestage
  passageWithLifeStageAssign(site=site,taxon=taxon,min.date=min.date,max.date=max.date,output.file=output.file,ci=ci,nLS=3,weightUse=FALSE,autoLS=TRUE)
  
}

