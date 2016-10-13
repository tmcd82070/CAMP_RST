#' @import  quantreg splines MASS mvtnorm RODBC Rcpp plyr mclust tools grDevices graphics stats DBI R6 assertthat magrittr
#' @importFrom utils packageVersion memory.limit combn getWinProgressBar head setWinProgressBar tail winProgressBar write.csv write.table
#' @importFrom ellipse ellipse
#' 
.onAttach <- function(libname, pkgname){
	
	v <- utils::packageVersion(pkgname) 
	
	packageStartupMessage( paste(pkgname, " (vers ", v ,")", sep=""))  
	packageStartupMessage( paste("Memory limit:", utils::memory.limit(), "Mb") )
	
	GlobalVars()

}