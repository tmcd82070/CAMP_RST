#' @export chuck.zeros
#' 
#' @title chuck.zeros
#' 
#' @description
#' 
#' @param b describe argument
#' @param e describe argument
#' @param beg.buff describe argument
#' @param end.buff describe argument
#' @param df describe argument
#' 
#' @details other comments found in file
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
chuck.zeros <- function(b,e,beg.buff,end.buff,df){  
  
  # beg.buff <- 0
  # end.buff <- 0
  # df <- df2
  
  dfO <- df[order(df$batchDate),]
  
  if((b - beg.buff) > 0){
    beg.date <- dfO$batchDate[b - beg.buff]                               # b is the index identifying the first day of non-zero data.
  } else {
    beg.date <- dfO$batchDate[1]
  }
  
  if((e - end.buff) > 0){
    end.date <- dfO$batchDate[nrow(dfO) - e + 1 + end.buff]               # e is the first zero or NA after non-zero data
  } else {
    end.date <- dfO$batchDate[nrow(dfO)]
  }
  
  df.small <- df[df$batchDate >= beg.date & df$batchDate <= end.date,]    # restrict on the original unordered df
  
  df.small
}  
