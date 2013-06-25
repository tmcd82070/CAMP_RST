F.latex.recapSummary <- function( recapSumm.df ){
#
#   Write a nice Latex table of release and recapture information for inclusion in a report.
#
#   input:
#   recapSumm.df = passage estimate data frame from F.summarize.releases
#

#   ---- Computations for table

dt <- format( recapSumm.df$releaseDate, "%d %b %Y" )
per <- format( round(as.numeric(recapSumm.df$recapPeriod),2), big.mark="," )
rel <- format( round(recapSumm.df$nReleased), big.mark="," )
recap <- format( round(recapSumm.df$nRecaped), big.mark="," )
te <- (recapSumm.df$nRecaped + 1) / (recapSumm.df$nReleased + 1)    # Baileys efficiency (r + 1)/(m + 1) to avoid 0
te <- format( round(te,4) )

ans <- paste( dt, per, rel, recap, te, sep="&" )
ans <- paste( ans, "\\\\ \n" )

tot.rel <- sum(recapSumm.df$nReleased)
tot.recap <- sum(recapSumm.df$nRecaped)
rom.te <- (tot.recap+1) / (tot.rel+1)

tot.rel.str <- format( tot.rel, big.mark="," )
tot.recap.str <- format( tot.recap, big.mark="," )
rom.te <- paste(format( round(rom.te,4), big.mark="," ), "$^{\\dagger}$", sep="")


#   ---- Now write the table to screen
cat( "  &
        \\multicolumn{1}{c}{Recap. Period} &
        \\multicolumn{1}{c}{Number} &
        \\multicolumn{1}{c}{Number}\\\\ \n")

cat( "\\multicolumn{1}{c}{Release Date} &
        \\multicolumn{1}{c}{Length (days)} &
        \\multicolumn{1}{c}{Released} &
        \\multicolumn{1}{c}{Recaptured} &
        \\multicolumn{1}{c}{Efficiency$^*$}\\\\ \n")
cat( "\\hline\n" )

cat( "\\endfirsthead \n" )

cat( "\\caption[]{\\em (continued)} \\\\ \n" )
cat( "\\hline\n" )

cat( "  &
        \\multicolumn{1}{c}{Recap. Period} &
        \\multicolumn{1}{c}{Number} &
        \\multicolumn{1}{c}{Number}\\\\ \n")

cat( "\\multicolumn{1}{c}{Release Date} &
        \\multicolumn{1}{c}{Length (days)} &
        \\multicolumn{1}{c}{Released} &
        \\multicolumn{1}{c}{Recaptured} &
        \\multicolumn{1}{c}{Efficiency$^*$}\\\\ \n")

cat( "\\endhead\\endfoot \n" )
cat( ans )
cat( "\\hline\n" )
cat( paste("\\multicolumn{2}{r}{Total:}&", tot.rel.str, "&", tot.recap.str, "&", rom.te, " \\\\ \\\\ \n", sep=" "))
cat( "\\hline\n" )
cat( "\\multicolumn{5}{l}{\\parbox{4.8in}{\\tiny $^*$Bailey's bias corrected efficiency estimator: $\\hat{E} = \\frac{r+1}{m+1}$,
where $r$ = recaptures and $m$ = number released.}} \\\\ \n" )
cat( "\\multicolumn{5}{l}{\\parbox{4.8in}{\\tiny $^\\dagger$Bias corrected ratio of totals: $\\hat{E} = \\frac{R+1}{M+1}$,
where $R$ = total recaptures and $M$ = total number released.}} \\\\ \n" )



}
