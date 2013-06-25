F.latex.biweekly.table <- function( df = grand.df, ctch = catch.df ){
#
#   Write Latex command to produce a table in the biweekly report
#

#   --- compute values and other things that go in the table

#   Collapse some values in grand data frame over subsites
pass <- tapply( df$passage, df$batchDate, sum, na.rm=T )
p.imp <- tapply( df$imputed.catch, df$batchDate, mean, na.rm=T )
pass <- data.frame( batchDate=names(pass), passage=pass, p.imp, stringsAsFactors=F )

min.fl <- tapply( ctch$mean.fl, ctch$batchDate, function(x){if(all(is.na(x))) NA else min(x, na.rm=T)} )
max.fl <- tapply( ctch$mean.fl, ctch$batchDate, function(x){if(all(is.na(x))) NA else max(x, na.rm=T)} )
fl.df <- data.frame( batchDate=names(max.fl), min.fl, max.fl, stringsAsFactors=F )
df <- merge( pass, fl.df, all.x=T )

#   Data for individual columns
dt <- as.POSIXct(df$batchDate, format="%Y-%m-%d")
dt <- format(dt, "%d-%b-%y")

#   Fall passage estimates
fallpass <- format( round( df$passage ), big.mark="," )
pimp <- format( round( df$p.imp * 100 ) )
pimp <- paste( pimp, "\\%", sep="" )

minfl <- as.character(round( df$min.fl ))
maxfl <- as.character(round( df$max.fl ))

minfl[ is.na(minfl) ] <- "--"
maxfl[ is.na(maxfl) ] <- "--"

minfl <- format(minfl)
maxfl <- format(maxfl)


table.cells <- paste( dt, fallpass, pimp, minfl, maxfl,  sep="&")
table.cells <- paste( table.cells, "\\\\ \n" )

#   Totals
tot.pass <- format(round(sum(df$passage, na.rm=T)), big.mark=",")
tot.pimp <- format(round(mean(df$p.imp, na.rm=T)*100) )
tot.pimp <- paste(tot.pimp, "\\%", sep="")
tot.minfl <- paste( round( min(df$min.fl, na.rm=T) ), round( max(df$min.fl, na.rm=T)), sep="--")
tot.maxfl <- paste( round( min(df$max.fl, na.rm=T) ), round( max(df$max.fl, na.rm=T)), sep="--")



#   --- Now write out the LaTex code for the table

cat( "\\begin{longtable}{rrrrr} \n")
cat( "\\caption{\\label{tab:pass}Preliminary estimates of daily passage for unmarked juvenile " )
cat( SPECIES ); cat( "\n")
cat( " captured by rotary-screw traps at " )
cat( SITE.NAME ); cat( "\n")
cat( " on the " )
cat( STREAM ); cat( ".\n")
cat( "\\textit{Percent Imputed} is the percentage of that day's passage that has been imputed by predictive models.} \n" )
 

#   Header on first page of table
cat( "\\\\ \n \\hline \n" )
cat( "&Estimated&Percent&\\multicolumn{2}{c}{Fork Lengths (mm)} \\\\ \n" )
cat( "\\cline{4-5} \n" )
cat( "\\multicolumn{1}{c}{Date}&\\multicolumn{1}{r}{Passage}&\\multicolumn{1}{r}{Imputed}&\\multicolumn{1}{r}{Min}&\\multicolumn{1}{r}{Max} \\\\ \n")
cat( "\\hline \n")
cat( "\\endfirsthead \n")

#   Header on subsequent pages of table
cat( "\\multicolumn{5}{c}{\\textit{(\\textbf{Table \\thetable} continued.)}} \\\\ \\hline \n")
cat( "&Estimated&Percent&\\multicolumn{2}{c}{Fork Lengths (mm)} \\\\ \n" )
cat( "\\cline{4-5} \n" )
cat( "\\multicolumn{1}{c}{Date}&\\multicolumn{1}{r}{Passage}&\\multicolumn{1}{r}{Imputed}&\\multicolumn{1}{r}{Min}&\\multicolumn{1}{r}{Max} \\\\ \n")
cat( "\\hline \n")
cat( "\\endhead \n")

#   Footer on all pages but last
cat( "\\hline \n" )
cat( "\\multicolumn{5}{c}{\\emph{(Continued next page)}} \\\\ \n") 
cat( "\\endfoot \n")

#   Footer on last page
cat( "\\hline \n" ) 
cat( paste("\\textbf{Total and Ranges:} &", tot.pass, "&", tot.pimp, "&", tot.minfl, "&", tot.maxfl, "\\\\ \n"))
cat( "\\endlastfoot \n")



cat( table.cells )

cat( "\\end{longtable} \n")

}

