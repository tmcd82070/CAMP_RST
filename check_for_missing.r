F.check.for.missing <- function( df, var ){
#
#   Check for presence of any missing values in "var" of the data frame df.
#

if( any( is.na(df[,var]) ) ){
    stop( paste( "Missing values found.  Missing values are not allowed in variable", var ))
}

1
}
