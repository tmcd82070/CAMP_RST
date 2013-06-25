F.odt.biweekly.table <- function( df = grand.df, ctch = catch.df ){
#
#   Write ODT code to produce a table in the biweekly report
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
pimp <- paste( pimp, "%", sep="" )

minfl <- as.character(round( df$min.fl ))
maxfl <- as.character(round( df$max.fl ))

minfl[ is.na(minfl) ] <- "--"
maxfl[ is.na(maxfl) ] <- "--"

minfl <- format(minfl)
maxfl <- format(maxfl)



#   Totals
tot.pass <- format(round(sum(df$passage, na.rm=T)), big.mark=",")
tot.pimp <- format(round(mean(df$p.imp, na.rm=T)*100) )
tot.pimp <- paste(tot.pimp, "%", sep="")
tot.minfl <- paste( round( min(df$min.fl, na.rm=T) ), round( max(df$min.fl, na.rm=T)), sep="--")
tot.maxfl <- paste( round( min(df$max.fl, na.rm=T) ), round( max(df$max.fl, na.rm=T)), sep="--")



#   --- Now write out the ODT for the table

out.df <- data.frame( Date=dt, Estimated.Passage=fallpass, Percent.Imputed=pimp, Min=minfl, Max=maxfl )
tot.df <- data.frame( Date="Totals and Ranges:", Estimated.Passage=tot.pass, Percent.Imputed=tot.pimp, Min=tot.minfl, Max=tot.maxfl )
out.df <- rbind( out.df, tot.df ) 

capt <- paste( "Preliminary estimates of daily passage for unmarked juvenile", SPECIES,  " captured by rotary-screw traps at ",
    SITE.NAME, "on the", STREAM, ".  'Percent Imputed' is the percentage of that day's passage that has been imputed by predictive models.")
odfTableCaption( capt )

#tab.style <- tableStyles( out.df, header=c("Date","Estimated Passage", "Percent Imputed", "Min Fork Len (mm)", "Max Fork Len (mm)") )
#rr <- nrow(tab.style$text)
#cc <- ncol(tab.style$text)
#print(dim(tab.style$text))
#tab.style$text <- matrix("ArialRight", nrow(tab.style$text), ncol(tab.style$text))
#tab.style$header <- matrix("ArialRightBold", nrow(tab.style$header), ncol(tab.style$header))
#odfTable( out.df, styles=tab.style, useRowNames = F )

odfTable( out.df, useRowNames = F, colnames=c("Date","Estimated Passage", "Percent Imputed", "Min Fork Len (mm)", "Max Fork Len (mm)") )

}

