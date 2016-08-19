#' @export capwords
#'   
#' @title capwords
#'   
#' @description Capitalize each word in a character vector of length one.
#'   
#' @param s A character vector of length one.
#' @param strict = FALSE If \code{TRUE}, force all letters of a word to 
#'   lowercase after the first letter.  Default is \code{FALSE}.
#' 
#' @return The character vector originally submitted via argument \code{s} with
#'   the first letter of each word capitalized.
#'   
#' @author WEST Inc.
#'   
#' @examples
#' #   ---- Capitalize each of 'hello' and 'there'.  
#' capwords("hello there")
#' 
#' #   ---- Use the strict argument.  Returns 'Camp'.
#' capwords("CAMP",strict=TRUE)
capwords <- function(s, strict = FALSE) {
  cap <- function(s) paste(toupper(substring(s,1,1)),
                {s <- substring(s,2); if(strict) tolower(s) else s},
                            sep = "", collapse = " " )
  sapply(strsplit(s, split = " "), cap, USE.NAMES = !is.null(names(s)))
}