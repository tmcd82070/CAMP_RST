#' @export F.lifestage.passage.assignLS
#' 
#' @title F.lifestage.passage.assignLS
#' 
#' @description
#' 
#'    ANNUAL PRODUCTION ESTIMATES BY LIFE STAGE AND RUN – TABULAR SUMMARY
#'    A table of passage estimates, with lifestages down the rows, and runs across the columns.
#' 
#'    Input:
#'    site = site ID of the place we want, trap locaton
#'    taxon = taxon number (from luTaxon) to retrieve
#' 
#' 
#' 
#'  allow the program to decide the number of life stage groups and if weight will be used or not
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
#'  allow the program to decide the number of life stage groups and if weight will be used or not
#' 
#' @return <describe return value>
#' 
#' @author WEST Inc.
#' 
#' @seealso \code{\link{<related routine>}}, \code{\link{<related routine>}}
#' 
#' @examples
#' <insert examples>
#' 
F.lifestage.passage.assignLS <- function(site, taxon, min.date, max.date, output.file, ci=TRUE){
###
###   ANNUAL PRODUCTION ESTIMATES BY LIFE STAGE AND RUN – TABULAR SUMMARY
###   A table of passage estimates, with lifestages down the rows, and runs across the columns.
###
###   Input:
###   site = site ID of the place we want, trap locaton
###   taxon = taxon number (from luTaxon) to retrieve
###


    ## allow the program to decide the number of life stage groups and if weight will be used or not
    passageWithLifeStageAssign(site=site,taxon=taxon,min.date=min.date,max.date=max.date,output.file=output.file,ci=ci,nLS=NULL,weightUse=NULL)



}
