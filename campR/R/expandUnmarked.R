#' @export expandUnmarked
#' 
#' @title expandUnmarked
#' 
#' @description
#' 
#'  Jared Studyvin
#'  15 Jan 2016
#'  replicate the rows of a dataframe based on one of the columns
#' 
#' 
#' 
#' 
#' @param dat describe argument
#' @param colKeep describe argument
#' @param colRep describe argument
#' 
#' @details other comments found in file
#'  colRep = single string of the frequency to repeat the data
#' 
#' @return describe return value
#' 
#' @author WEST Inc.
#' 
#' @seealso \code{\link{related routine}}, \code{\link{related routine}}
#' 
#' @examples
#' #insert examples
#' 
###################################################
## Jared Studyvin
## 15 Jan 2016
## replicate the rows of a dataframe based on one of the columns
###################################################


expandUnmarked <- function(dat,colKeep,colRep){
    ## dat = data frame
    ## colKeep = string vector of the columns names of the data to be repeated
    ## colRep = single string of the frequency to repeat the data
    repeatRow <- function(data,col.Keep,col.Rep){
        row <- data[,col.Keep]
        rep <- data[,col.Rep]
        out <- do.call('rbind',replicate(rep,row,simplify=FALSE))
        return(out)
    }

    outData <- adply(dat,1,.fun=repeatRow,col.Keep=colKeep,col.Rep=colRep)
    outData[,colRep] <- NULL
    return(outData)
}
