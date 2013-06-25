F.weekly.effort <- function( site, min.date, max.date, output.file ){
#
#   compute weekly effort.  Effort is number of days/hours that trap was fishing.
#
#   site = site to do summary for
#   min.date = minimum date to include
#   max.date = maximum date to include
#   output.file = root name of output file(s)

f.prop.fished <- function( df ){
    # compute proportion of min(sample start) to max(sample start) that trap 
    # was fishing
    off.hrs <- sum(as.numeric(df$sampleGapLenHrs), na.rm=TRUE )
    on.hrs <- sum(as.numeric(df$sampleLengthHrs), na.rm=TRUE )
    prop <- on.hrs / (on.hrs + off.hrs)
    prop
}


#   ================================================================================================
#   Fetch the visit table.

visit <- F.get.indiv.visit.data( site, NA, min.date, max.date )


weeks <- format( visit$batchDate, "%U" )   # Sunday is first day of the week, Weeks go from 0 to 51 
yrs <- format( visit$batchDate, "%Y" )


eff <- by( visit, list(yrs=yrs, weeks=weeks), f.prop.fished )


obs.weeks <- dimnames(eff)$weeks
obs.yrs <- dimnames(eff)$yrs
obs.eff <- as.numeric(eff)
obs.eff <- data.frame(expand.grid(yr=as.numeric(obs.yrs), week=as.numeric(obs.weeks)), prop.on=obs.eff )
obs.eff <- obs.eff[ !is.na(obs.eff$prop.on), ]
obs.eff <- obs.eff[ order(obs.eff$yr, obs.eff$week), ]

min.dt <- as.POSIXct( min.date, format="%Y-%m-%d", tz="America/Los_Angeles" )
max.dt <- as.POSIXct( max.date, format="%Y-%m-%d", tz="America/Los_Angeles" )
all.weeks <- seq( min.dt, max.dt, by=7*24*60*60 )
all.weeks <- data.frame( yr=as.numeric(format(all.weeks, "%Y")), week=as.numeric(format(all.weeks, "%U")) )

eff.df <- merge( obs.eff, all.weeks, by=c("yr", "week"), all.y=TRUE )
eff.df$prop.on[ is.na(eff.df$prop.on) ] <- 0
eff.df <- eff.df[ order(eff.df$yr, eff.df$week), ]
eff.df$prop.off <- 1 - eff.df$prop.on


eff.bars <- t(as.matrix(eff.df[,c("prop.on", "prop.off")]))

#   ========================================================================
#   Now plot the bars

out.fn <- paste(output.file, "_effort.png", sep="")
tryCatch({png(file=out.fn,width=7,height=7,units="in",res=600)}, error=function(x){png(file=out.fn)})
layout( matrix(c(1,2), ncol=1), height=c(1,1), widths=1)
par(mar=c(2.1,4.1,0,0))

mid.time <- round((1+ncol(eff.bars))/2)
eff1.bars <- eff.bars[,1:mid.time]
eff2.bars <- eff.bars[,(mid.time+1):ncol(eff.bars)]

wk1.bars <- eff.df$week[1:mid.time]
wk2.bars <- eff.df$week[(mid.time+1):ncol(eff.bars)]

yr1.bars <- eff.df$yr[1:mid.time]
yr2.bars <- eff.df$yr[(mid.time+1):ncol(eff.bars)]

#   Upper plot
barplot(eff1.bars, 
    space=0, 
    col=c("brown", "white"), 
    legend.text=c("Fished", "Not fished"), 
    args.legend=list(x="topright", horiz=T, bty="n"), 
    ylab="", 
    xlab="", 
    ylim=c(0,1.15), 
    yaxt = "n", 
    xaxt = "n")

mtext( side=2, text="Proportion of\nweek fished", line=2 )    
axis(2, at=seq(0, 1, by=.2))

#   Compute locations for, and labels of, months'
mon.cuts <- as.POSIXct( paste( yr1.bars, (wk1.bars)*7 + 1), format="%Y %j" )
mon.u <- format(mon.cuts, "%m")
mon.y <- yr1.bars[ !duplicated(mon.u) ]
mon.m <- tapply(1:length(mon.u), list(yr1.bars,mon.u), mean)     # This sorts by month number, which gets labels out of sync -> resort
mon.m <- sort(mon.m) - 0.5
mon.u <- mon.u[ !duplicated(mon.u) ]
mon.cuts <- as.POSIXct( paste(mon.y, mon.u, "15"), format="%Y %m %d")
mon.labs<- format(mon.cuts, "%b%y")
ind <- mon.m > 1
axis(1, at=mon.m[ind], labels=mon.labs[ind], las=1, tick=F, line=-.5 )


#   Add tick marks
mon.cuts <- as.POSIXct( paste( yr1.bars, (wk1.bars)*7 + 1), format="%Y %j" )
mon.u <- format(mon.cuts, "%m")
mon.cuts <- sort(tapply(1:length(mon.u), list(yr1.bars,mon.u), max))
axis(1, at=mon.cuts, labels=rep("",length(mon.cuts)))


#   Lower plot
par(mar=c(3.1,4.1,0,0))
barplot(eff2.bars, 
    space=0, 
    col=c("brown", "white"), 
    legend.text=c("Fished", "Not fished"), 
    args.legend=list(x="topright", horiz=T, bty="n"), 
    ylab="", 
    xlab="", 
    ylim=c(0,1.15), 
    yaxt = "n", 
    xaxt = "n")
    
mtext( side=2, text="Proportion of\nweek fished", line=2 )    
axis(2, at=seq(0, 1, by=.2))


#   Compute locations for, and labels of, months'
mon.cuts <- as.POSIXct( paste( yr2.bars, (wk2.bars)*7 + 1), format="%Y %j" )
mon.u <- format(mon.cuts, "%m")
mon.y <- yr2.bars[ !duplicated(mon.u) ]
mon.m <- tapply(1:length(mon.u), list(yr2.bars, mon.u), mean)     # This sorts by month number, which gets labels out of sync -> resort
mon.m <- sort(mon.m) - 0.5
mon.u <- mon.u[ !duplicated(mon.u) ]
mon.cuts <- as.POSIXct( paste(mon.y, mon.u, "15"), format="%Y %m %d")
mon.labs<- format(mon.cuts, "%b%y")

ind <- mon.m > 1
axis(1, at=mon.m[ind], labels=mon.labs[ind], las=1, tick=F, line=-.5 )


#   Add tick marks
mon.cuts <- as.POSIXct( paste( yr2.bars, (wk2.bars)*7 + 1), format="%Y %j" )
mon.u <- format(mon.cuts, "%m")
mon.cuts <- sort(tapply(1:length(mon.u), list(yr2.bars,mon.u), max))
axis(1, at=mon.cuts, labels=rep("",length(mon.cuts)))




dev.off(dev.cur())


#   ---- Send messages back to the interface
cat("SUCCESS - F.weekly.effort\n\n")
cat(paste("Working directory:", getwd(), "\n"))
cat(paste("R data frames saved in file:", "(none)", "\n\n"))
cat("Number of files created in working directory = 1\n")
cat(paste(out.fn, "\n"))
cat("\n")


invisible(eff)

}
