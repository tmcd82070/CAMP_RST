#' @export
#'   
#' @title max_buff_days
#'   
#' @description Identify the number of preceding and antecdedent zeros in the 
#'   catch record for an individual trap catch sequence.
#'   
#' @param df A data frame containing catch records for one 
#'   \code{trapPositionID}, with catch counts contained in \code{n.tot}.
#' @param trap A vector of length one containing the \code{trapPositionID} for 
#'   which buffering is necessary.
#'   
#' @details Function \code{max_buff_days} is a helper function utilized in the 
#'   estimation of catch, and assists to identify preceding and antecedent 
#'   zeros.  It works in conjunction with function \code{F.chuck.zeros}.
#'   
#' @return A vector containing three values.  The first contains the row index 
#'   of the first non-zero / non-\code{NA} in the catch record provided via data
#'   frame \code{df}. The second contains the row index of the last non-zero / 
#'   non-\code{NA} in the catch record provided via data frame \code{df}.  The 
#'   third contains the length of the vector containing 
#'   \code{trapVisitID}-specific fish counts (vector \code{n.tot}).
#'   
#' @author WEST Inc.
#'   
#' @seealso \code{F.chuck.zeros}
#'   
#' @examples
#' \dontrun{
#' #   ---- Find the first and last non-zero and non-NA catch record
#' #   ---- for trap "12345".
#' vec <- max.buff.days(df,"12345")
#' }
max_buff_days <- function(df,trap){
  
  # df <- df2
  
  #   ---- Calculate beginning buffer.
  n.totVec <- df[order(df$batchDate),]$n.tot
  b <- 1
  while( (n.totVec[b] == 0 | is.na(n.totVec[b])) & b <= (length(n.totVec) - 1) ){
    b <- b + 1
  }
  
  #   ---- Calculate ending buffer.
  n.totVec <- df[order(df$batchDate,decreasing=TRUE),]$n.tot
  e <- 1
  while( (n.totVec[e] == 0 | is.na(n.totVec[e])) & e <= (length(n.totVec) - 1) ){
    e <- e + 1
  }
  
  paste0('Trap ',trap,' needs to examine buffering zeros via a (beg.buff x end.buff) buff-zero ',b,' x ',e,' matrix.')
  buffs <- c(b,e,length(n.totVec))
  buffs
  
}  
