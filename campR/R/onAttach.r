#' @import  quantreg splines MASS mvtnorm RODBC Rcpp plyr mclust car tools utils grDevices graphics stats
#' 
#' 
.onAttach <- function(libname, pkgname){
	
	v <- utils::packageVersion(pkgname) 
	
	packageStartupMessage( paste(pkgname, " (vers ", v ,")", sep=""))  
	packageStartupMessage( paste("Memory limit:", utils::memory.limit(), "Mb") )
	
	GlobalVars()

}
