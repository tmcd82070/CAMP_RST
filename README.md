# campR 

This repository contains R code associated with the CAMP Rotary Screw Trap platform.  
The RST platform is a data base application that collects and manages rotary screw 
trap data, and was originally built for the network of rotary screw traps in rivers 
of the central valley of California.  These R code files are called from the application
to do analyses.  

# Programming Notes

## Installing `campR` into the platform

```
# Set directory to the R/library folder in the The Platform application
campR.Rdir <- "C:/Users/trent/Documents/Projects/1200-RSTPlatform/ThePlatform/CAMP_RST20220103-campR2.0.14/R/library"

# Install a new version of campR into the application
withr::with_libpaths(campR.Rdir, devtools::install(upgrade ="never"))
```


## Upgrading R in the platform

Steps to update the version of R used in The Platform:

1. Download new R
2. Install new R into a clean directory INSIDE the RST Platform directory.
   That is, install into '/R' folder inside the platform's directory structure.  
   Don't accept the default
   directory name that has version number in it, eg., 'R-4.1.2'. Just "R".
3. Delete the following directories from the new R installation:
   'doc', 'tcl', 'tests'.  Keep same directories as you did during previous
   builds of the platform.
4. Update the packages of the new R installation.  Check `DESCRIPTION` for 
   the list of Imports and Dependencies.  Do this with statements like: 
```
# Set lib directory
newLib <- "C:/Users/trent/Documents/Projects/1200-RSTPlatform/ThePlatform/CAMP_RST20220103-campR2.0.14/R/library"

# Install packages
campRImports <- c("RODBC", "DBI", "splines", "dplyr", "RPostgres", "httr", "crayon", "magrittr", "knitr", "ellipse", "jsonlite", 
     "mvtnorm", "quantreg", "SparseM", "MatrixModels")
install.packages(campRImports, lib = newLib)
```
