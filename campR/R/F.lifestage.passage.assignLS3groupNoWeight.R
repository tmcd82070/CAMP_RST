#' @export F.lifestage.passage.assignLS3groupNoWeight
#' 
#' @title F.lifestage.passage.assignLS3groupNoWeight
#' 
#' @description 3 life stage groups are fit and weight will not be used to assign lifestage
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
#' @param autoLS Default is FALSE, no analytical life stage assignment is done.
#'   If TRUE, the analytical life stage assignment is done, see details.
#' @param reclassifyFL A logical indicating if passage should be estimated via
#'   forklength-based class groups.
#' 
#' @details 3 life stage groups are fit and weight will not be used to assign lifestage
#' 
#' @return jared
#' 
#' @author WEST Inc.
#' 
#' @seealso \code{F.run.passage}, \code{passageWithLifeStageAssign},
#'   \code{F.lifestage.passage.forkLength}, \code{F.lifestage.passage.assignLS},
#'   \code{F.lifestage.passage.assignLS2group}, 
#'   \code{F.lifestage.passage.assignLS2groupNoWeight},
#'   \code{F.lifestage.passage.assignLS3group},
#'   \code{F.lifestage.passage.assignLSNoWeight}
#' 
#' @examples
#' \dontrun{
#' #   ---- Calculate specified auto lifestage on the American.
#' site <- 57000
#' taxon <- 161980
#' min.date <- "2013-01-01"
#' max.date <- "2013-05-30"
#' output.file <- "Testing"
#' ci <- TRUE
#' autoLS <- TRUE
#' reclassifyFL <- FALSE
#' 
#' F.lifestage.passage.assignLS3groupNoWeight(site,taxon,min.date,max.date,
#'   output.file,ci,autoLS,reclassifyFL)
#' 
#' }
F.lifestage.passage.assignLS3groupNoWeight <- function(site, taxon, min.date, max.date, output.file, ci=TRUE,autoLS=TRUE,reclassifyFL=FALSE){
  
  ## 3 life stage groups are fit and weight will not be used to assign lifestage
  passageWithLifeStageAssign(site=site,taxon=taxon,min.date=min.date,max.date=max.date,output.file=output.file,ci=ci,nLS=3,weightUse=FALSE,autoLS=TRUE,reclassifyFL=FALSE)
  
}

