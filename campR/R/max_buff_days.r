#' @export maxBuffDays
#' 
#' @title max.buff.days
#' 
#' @description
#'  df <- df2
#' 
#' @param df <describe argument>
#' @param trap <describe argument>
#' 
#' @details <other comments found in file>
#'  calculate beginning buffer
#'  calculate ending buffer
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
maxBuffDays <- function(df,trap){
  
  # df <- df2
  
  # calculate beginning buffer
  n.totVec <- df[order(df$batchDate),]$n.tot
  b <- 1
  while( (n.totVec[b] == 0 | is.na(n.totVec[b])) & b <= (length(n.totVec) - 1) ){
    b <- b + 1
  }
  
  # calculate ending buffer
  n.totVec <- df[order(df$batchDate,decreasing=TRUE),]$n.tot
  e <- 1
  while( (n.totVec[e] == 0 | is.na(n.totVec[e])) & e <= (length(n.totVec) - 1) ){
    e <- e + 1
  }
  
  paste0('Trap ',trap,' needs to examine buffering zeros via a (beg.buff x end.buff) buff-zero ',b,' x ',e,' matrix.')
  buffs <- c(b,e,length(n.totVec))
  buffs
  
}  
