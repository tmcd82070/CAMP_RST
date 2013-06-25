F.vec2sqlstr <- function( tbl, vec, sep.str=", " ){
#
#   Convert a vector of strings to one giant string, suitable 
#   for inclusion in an SQL statement.  i.e., de-vectorize and paste together.
#

ans <- paste(tbl,vec[1], sep=".")
for( i in 2:length(vec)){
    ans <- paste( ans, paste(tbl,vec[i],sep="."), sep=sep.str )
}

ans
}
