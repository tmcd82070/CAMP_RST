F.latex.passage <- function( pass.df ){
#
#   Write a nice Latex table of passage estimates for inclusion in a report.
#
#   input:
#   pass.df = passage estimate data frame from F.est.passage
#

dt <- format( pass.df$datetime, "%d %b %Y" )
pass <- format( round(pass.df$passage), big.mark="," )
ans <- paste( dt, pass, sep="&" )
ans <- paste( ans, "\\\\ \n" )


cat( "                        &\\multicolumn{1}{c}{Passage}\\\\ \n")
cat( "\\multicolumn{1}{c}{Date}&\\multicolumn{1}{c}{Estimate}\\\\ \n")
cat( "\\endfirsthead \n" )
cat( "\\caption[]{\\em (continued)} \\\\ \n" )
cat( "\\hline\n" )
cat( "                        &\\multicolumn{1}{c}{Passage}\\\\ \n")
cat( "\\multicolumn{1}{c}{Date}&\\multicolumn{1}{c}{Estimate}\\\\ \n")
cat( "\\endhead\\hline\\endfoot \n" )
cat( ans )
cat( "\\hline\n" )
cat( paste("Total:&",format( round(sum(pass.df$passage)), big.mark="," ), "\n"))


#   These are commands that will produce a regular table, not a long table.
#cat( "\\begin{center}\n" )
#cat( "\\begin{tabular}{rc} \\hline \n")
#cat( "&Passage\n" )
#cat( "Date&Estimate\n" )
#cat( "\\hline\n" )
#cat( ans )
#cat( "\\hline\n" )
#cat( "\\end{tabular}\n" )
#cat( "\\end{center}\n" )
#cat( "\\end{table}\n" )


}
