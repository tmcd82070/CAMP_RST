#' @export chuck.zeros
#'   
#' @title chuck.zeros
#'   
#' @description Removes zeros before the first non-zero catch, and the same for
#'   zeros following the last non-zero catch.
#'   
#' @param b An integer representing the number of zeros and \code{NA} trapping
#'   instances to keep before the first non-zero and non-\code{NA} catch record.
#'   Always set to zero.
#' @param e An integer representing the number of zeros and \code{NA} trapping
#'   instances to keep after the last non-zero and non-\code{NA} catch record.
#'   Always set to zero.
#' @param beg.buff An integer.  The actual number of zeros and \code{NA}
#'   trapping instances prior to the first non-zero and non-\code{NA} catch
#' record in the original data frame \code{df}.
#' @param end.buff An integer.  The actual number of zeros and \code{NA} 
#'   trapping instances after the last non-zero and non-\code{NA} catch record 
#'   in the original data frame \code{df}.
#' @param df A data frame containing catch records for one particular trap's 
#'   temporal sequence.
#'   
#' @details Manipulation of the \code{b} and \code{e} variables specifies the 
#'   number of trapping days with preceding and antecedent zeros to retain. 
#'   Thus, the number of zero catch records both before the first and after the 
#'   last caught fish is independently adjustable.
#'   
#'   Note that variable \code{n.tot} in data frame \code{df} identifies zero and
#'   \code{NA} catch records.
#'   
#' @return A data frame, necessarily a subset of the data frame \code{df} 
#'   provided to the function, with all zero and \code{NA} catch records removed
#'   before the first, and after the last, valid catch record.
#'   
#' @author WEST Inc.
#' 
#' @seealso \code{max.buff.days}
#' 
#' @examples
#' \dontrun{
#' #   ---- Remove all zero and NA records before the first non-zero and non-NA
#' #   ---- catch record in record 4 in data frame catch.df.  Do the same for
#' #   ---- the last, which is in record 12.  
#' df <- chuck.zeros(0,0,4,12,catch.df)
#' }
chuck.zeros <- function(b,e,beg.buff,end.buff,df){  
  
  # beg.buff <- 0
  # end.buff <- 0
  # df <- df2
  
  dfO <- df[order(df$batchDate),]
  
  #   ---- Identify the beginning date of catch, i.e., the first with non-zero catch. 
  #   ---- Variable b is the index identifying the first day of non-zero data.
  if((b - beg.buff) > 0){
    beg.date <- dfO$batchDate[b - beg.buff]                               
  } else {
    beg.date <- dfO$batchDate[1]
  }
  
  #   ---- Identify the ending date of catch, i.e., the last with non-zero catch.
  #   ---- Variable e is the first zero or NA after non-zero data
  if((e - end.buff) > 0){
    end.date <- dfO$batchDate[nrow(dfO) - e + 1 + end.buff]              
  } else {
    end.date <- dfO$batchDate[nrow(dfO)]
  }
  
  #    ---- Restrict on the original unordered data frame based on the beg.date 
  #    ---- end.date identified via the provided size of the requested buffer.
  df.small <- df[df$batchDate >= beg.date & df$batchDate <= end.date,]    
  
  df.small
}  
