#' @export F.lifestage.passage.assignLSNoWeight
#' 
#' @title F.lifestage.passage.assignLSNoWeight
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
#'  allow the program to decide the number of life stage groups but weight will not be used to assign lifestage
#' 
#' 
#' @param site The identification number of the site for which estimates are 
#'   required.
#' @param taxon The species identifier indicating the type of fish of interest. 
#'   This is always \code{161980}; i.e., Chinook Salmon.
#' @param min.date The start date for data to include. This is a text string in 
#'   the format \code{\%Y-\%m-\%d}, or \code{YYYY-MM-DD}.  
#' @param max.date The end date for data to include.  Same format as 
#'   \code{min.date}.
#' @param output.file A text string indicating a prefix to append to all output.
#' @param ci A logical indicating if 95% bootstrapped confidence intervals
#'   should be estimated along with passage estimates.
#' @param autoLS Default is FALSE, no analytical life stage assignment is done. If TRUE, the analytical life stage assignment is done, see details.
#' @param nLS Number of life stage groups to be estimated. Ignored if autoLS=FALSE, see details.
#' @param weightUse Boolean variable indicating if weight should be used for the analytical assignment, default is NULL. Ignored if autoLS=FALSE, see details.
#' @param reclassifyFL A logical indicating if passage should be estimated via forklength-based class groups.  
#' 
#' @details <other comments found in file>
#' NA
#' NA
#' NA
#'  allow the program to decide the number of life stage groups but weight will not be used to assign lifestage
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
F.lifestage.passage.assignLSNoWeight <- function(site, taxon, min.date, max.date, output.file, ci=TRUE,autoLS=TRUE,reclassifyFL=FALSE){
  ###
  ###   ANNUAL PRODUCTION ESTIMATES BY LIFE STAGE AND RUN TABULAR SUMMARY
  ###   A table of passage estimates, with lifestages down the rows, and runs across the columns.
  ###
  ###   Input:
  ###   site = site ID of the place we want, trap locaton
  ###   taxon = taxon number (from luTaxon) to retrieve
  ###
  
  
  ## allow the program to decide the number of life stage groups but weight will not be used to assign lifestage
  passageWithLifeStageAssign(site=site,taxon=taxon,min.date=min.date,max.date=max.date,output.file=output.file,ci=ci,nLS=NULL,weightUse=FALSE,autoLS=TRUE,reclassifyFL=FALSE)
  
  
  
}
