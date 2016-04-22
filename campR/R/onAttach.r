#' @import  quantreg splines MASS mvtnorm RODBC Rcpp plyr mclust car tools
#' 
#' 
.onAttach <- function(libname, pkgname){
	
	v <- utils::packageVersion(pkgname) 
	
	packageStartupMessage( paste(pkgname, " (vers ", v ,")", sep=""))  
	packageStartupMessage( cat(paste("Memory limit:", memory.limit(), "Mb")) )


}