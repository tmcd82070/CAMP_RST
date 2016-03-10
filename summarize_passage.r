F.summarize.passage <- function( df, summarize.by ){
#
#   Average passage over all traps at a site, then sum by summarize.by
#
#   df must have the following columns: $batchDate, $imputed.catch
#   df <- grand.df
#   summarize.by <- sum.by
  
#   ---- First, average over traps
index <- list(batchDate= format( df$batchDate, "%Y-%m-%d" ))

n <- c(tapply( df$passage, index, FUN=mean, na.rm=T ))
dt <- c(tapply( df$batchDate, index, FUN=function(x, narm){ min(x, na.rm=narm) }, narm=T ))
class(dt) <- class(df$batchDate)  # the tapply above strips class information.  make this back into a POSIX date
attr(dt, "tzone") <- attr(df$batchDate, "tzone")   
p.imp <- c(tapply( df$imputed.catch, index, FUN=mean, na.rm=T ))
  
#   ---- Now, summarize passage by time period.  
index <- F.summarize.index( dt, summarize.by )

n <- c(tapply( n, index, FUN=sum, na.rm=T ))
dt <- c(tapply( dt, index, FUN=function(x, narm){ min(x, na.rm=narm) }, narm=T ))
p.imp <- c(tapply( p.imp, index, FUN=mean, na.rm=T ))

class(dt) <- class(df$batchDate)

#   Put catch and aux information into data frame
n <- data.frame( s.by=names(n),
    passage=n,
    date=dt,
    pct.imputed.catch=p.imp,
    stringsAsFactors=F, row.names=NULL)

n

}
