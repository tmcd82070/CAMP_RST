F.sql.error.check <- function( obj ){
#
#   Check if a SQL statement executed correctly.
#

if( is.vector(obj) & length(obj) >= 2 ){
    if( length(grep("ERROR", obj[2])) > 0 ){
        #   An error has occurred.
        stop(paste(obj,"\n"))
    }
}

1
}
