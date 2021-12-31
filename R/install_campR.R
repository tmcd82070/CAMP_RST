#' @title install_campR
#' 
#' @description A utility function to document, build, 
#' install, and attach the CAMP_R package.  This is for 
#' development work in R. Normal users will never call this.
#' 
#' @details All paths are hard coded. This version 
#' installs to the "win-library" library.
#' 
#' @return The result of \code{devtools::install}.
#' 
#' @examples 
#' install_campR()
#' 
#' @export
#' 

install_campR <- function(lib="../Rlibrary", pkg = "campR"){
  
  # Home <- path.expand(file.path("~","Projects","115-GreenDiamond","Spotted Owls", "NSO_DataPrepPackage"))
  # Lib <- .libPaths()[grep("win-library",.libPaths())]
  # Pkg <- file.path(Home, "gdNSO")
  
  
    
    Pkg <- rprojroot::find_package_root_file(path=pkg)
    print(Pkg)
    
    if(is.null(lib)){
      Lib <- NULL
    } else {
      if( length(lib) > 1 ){
        stop("lib must have length 1")
      }  
      if( is.numeric(lib) ){
        Lib <- .libPaths()[lib]
      } else if( is.character(lib) ){
        Lib <- lib
      } else {
        stop("lib type not recognized")
      }
    } 
    
    # this method is fastest among several options (no build)
    devtools::document(Pkg)
    withr::with_libpaths(c(Lib,.libPaths()), 
                         out<- devtools::install(Pkg, 
                                                 dependencies = FALSE, 
                                                 quick=TRUE,
                                                 upgrade = "never"))
    
    if(out){
      # get package name so can return it. But, cannot rely on last 
      # directory in Pkg being package name
      # must get the name from the description file
      descFName <- file.path(Pkg, "DESCRIPTION")
      if( file.exists(descFName) ){
        # should always be here because proj_get finds the description file
        descFile <- readLines(descFName)
        packName <- sub("Package: ","", descFile[grep("Package:", descFile)])
      } else {
        # this should never happen
        packName <- NA
      }
      
      out <- data.frame(package = packName, library = Lib) 
    }
    
    invisible(out)
}
