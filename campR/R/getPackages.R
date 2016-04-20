#' @export getPackages
#' 
#' @title getPackages
#' 
#' @description
#' 
#'  Jared Studyvin
#'  10 Feb 2016
#'  getPackages
#'  this function loads pckages based on a vector string of package names
#'  if needed the package is installed first
#' 
#' 
#' 
#' 
#' @param needPackage <describe argument>
#' 
#' @details <other comments found in file>
#'    require("mclust")
#'    require("car")
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
#############################################
## Jared Studyvin
## 10 Feb 2016
## getPackages
## this function loads pckages based on a vector string of package names
## if needed the package is installed first
#############################################


getPackages <- function(needPackage){
    ## install packages if not installed, then loads the packages


#   require("plyr")
#   require("mclust")
#   require("car")

    packList <- installed.packages()[,'Package']

    needInstall <- needPackage[!needPackage%in%packList]
    cat('need install:\n')
    cat(needInstall,'\n')
    if(length(needInstall)>0){

        local({r <- getOption("repos");
            r["CRAN"] <- "http://cran.us.r-project.org";
            options(repos=r)})
        lapply(needInstall,install.packages)
    }
    cat('packages to load:', '\n')
    cat(needPackage, '\n')

    for(p in needPackage){
        eval(parse(text=paste0('require(',p,')')))
    }
    return(NULL)
} ## end getPackages
