#' @export
#' 
#' @title F.plot.passage
#' 
#' @description Plot a bar graph of the passage estimates in the temporal units
#'   specified by the user in the original passage function call.
#' 
#' @param df A data frame containing passage estimates calculated via function 
#'   \code{F.est.passage} per one of day, week, month, or year.
#' @param out.file The name of the file prefix under which output is to be
#'   saved.  Set to \code{NA} to plot to the Plot window.
#'   
#' @return Output includes a \code{png} graphical display of total estimated 
#'   passage by specified temporal unit.  Temporal units, or bars in the graph, 
#'   display both the number of observed fish and imputed fish.  The total 
#'   number of estimated fish over the entire temporal range is also reported.  
#'   
#' @details It is assumed that data frame \code{df} contains at the least 
#'   variables \code{<temporal time frame>}, \code{passage}, \code{date}, and 
#'   \code{pct.imputed.catch}. Variable \code{<temporal time frame>} is either 
#'   one of "\code{year}," "\code{month}," "\code{week}," or "\code{day}." Weeks
#'   are reported in a modified Julian fashion as recorded in the "Dates" table
#'   in any associated Access database.  Passage estimates for
#'   each reported time period are rounded to the nearest whole fish.  Variable
#'   \code{date} is a POSIX date, with data formatted as \code{\%Y-\%m-\%d},
#'   i.e., the ISO 8601 date format.  Variable \code{pct.imputed.catch} contains
#'   the estimated imputed proportion, and so takes on values between zero and
#'   one, inclusive.
#'   
#'   Other variables included as part of dataframe \code{df} are not utilized 
#'   \emph{per se} in the function, but do pass through.
#'   
#' @seealso 
#' 
#' @author Trent McDonald (tmcdonald@west-inc.com)
#'   
#' @examples 
#' \dontrun{
#' #   ---- Plot passage estimates per temporal time unit to
#' #   ---- the plot window.
#' F.plot.passage( df, out.file=NA )
#' }

#   ---- Save for possibly use later.  The function needs Julian dats housed in 
#   ---- the Dates table of an Access mdb.  
# #   ---- Create a data frame.
# df <- data.frame(week=c("2013-04","2013-05","2013-06","2013-07","2013-08","2013-09","2013-10" ),
#                  passage=c(170440,451627,516025,712524,1808704,1009422,330961),
#                  date=c(as.POSIXct(strftime(seq(from=c(ISOdate(2013,1,24)),by="7 days",length.out=7),format="%F"),tz="America/Los_Angeles")),
#                  pct.imputed.catch=c(0,0,0.07142857,0.03571429,0,0.09523810,0.17857143),
#                  lower.95=c(145533.4,338947.2,338707.6,610530.4,1617157.8,908780.0,284323.1),
#                  upper.95=c(211384.9,505920.6,617590.4,772284.6,2108123.6,1183509.0,366553.2),
#                  nForkLenMM=c(1267,1800,1298,1900,1800,1891,1300),
#                  meanForkLenMM=c(36.30466,36.52500,36.56086,36.56105,36.58111,36.42041,36.37154),
#                  sdForkLenMM=c(1.183525,1.121683,1.098411,1.195978,1.233118,1.309681,1.454088),
#                  sampleLengthHrs=c(235.2500,336.7833,313.7000,453.5000,496.3000,450.8167,288.8833),
#                  sampleLengthDays=c(9.802083,14.032639,13.070833,18.895833,20.679167,18.784028,12.036806))
# attr(df,"run.name") <- "Fall"
# attr(df,"lifestage.name") <- "All lifestages"
# attr(df,"summarized.by") <- "week"
# attr(df,"site.name") <- as.factor(c("A River in the Central Valley"))

F.plot.passage <- function( df, out.file="passage.png" ){

  #   df       <- passby             # df <- example
  #   out.file <- 'passage.png'
  
  if( !is.na(out.file) ){
    graphics.off()
    
    #   ---- Open PNG device.
    out.pass.graphs <- paste(out.file, run.name,"_passage.png", sep="")    # added run.name to make all runs program work.  this will affect one run report, if it needs to be turned back on.
    if(file.exists(out.pass.graphs)){
        file.remove(out.pass.graphs)
    }
    tryCatch({png(filename=out.pass.graphs,width=7,height=7,units="in",res=600)}, error=function(x){png(filename=out.pass.graphs)})  # produces hi-res graphs unless there's an error, then uses default png settings
  }

  #   ---- Construct matrix of bar heights.
  pass <- matrix( c(df$passage * df$pct.imputed.catch, df$passage * (1 - df$pct.imputed.catch)), ncol=nrow(df), byrow=T )
  dimnames(pass) <- list(NULL, df$date)
  pass[ is.na(pass) ] <- 0

  #   ---- Check for non-zero passage estimates.  
  if( all(pass == 0) ){
    plot( c(0,1), c(0,1), xaxt="n", yaxt="n", type="n", xlab="", ylab="")
    text( .5,.5, "All Zeros.\nCheck dates.\nCheck that finalRunID is assigned to >=1 fish per visit.\nEnsure subsites were operating between dates.")
    dev.off(dev.cur())
    ans <- out.pass.graphs
    return(ans)
  }

  #   ---- Compute extent of y-axis.
  hgts <- colSums( pass )
  lab.y.at <- pretty( hgts )

  #   ---- Graph using barplot.
  mp <- barplot( pass, beside=FALSE, space=0, col=c("lightblue","darkorange"), 
    legend.text=F, ylab="", xaxt="n", yaxt="n", ylim=range(c(hgts, lab.y.at)), 
    xlab="", cex.lab=1.5 )

  #   ---- Place label.
  mtext( "Passage estimate (# fish)", side=2, line=2.25, cex=1.5)

  #   --- Add x-axis labels.
  s.by <- capwords(attr(df,"summarized.by"))
  jason.s.by <<- s.by

  #   --- Determine temporal-dependent labels. 
  if( casefold(s.by) == "day" ){
    season.len <- difftime( max(df$date), min(df$date), units="days")
    cat(paste("Total length of season = ", season.len, "\n"))
    if( season.len > 40 ){
      
      #   --- Just label 1st of every month.
      dt <- format(df$date, "%m-%y")
      dt <- dt[ !duplicated(dt) ]
      dt <- as.POSIXct( strptime( paste( "1", dt, sep="-" ), format="%d-%m-%y"))
    
      ind <- as.numeric(cut( dt, df$date ))
      dt <- dt[ !is.na(ind) ]
      ind <- ind[!is.na(ind)]   # because first of some month may be less than first date

      lab.x.at <- mp[ind]
      lab.x.lab <- format(dt, "%d%b%y")

      axis( side=1, at=lab.x.at, labels=lab.x.lab )
    } else {
      
      #   ---- Label every day.
      dt <- df$date
      ind <- rep(T, length(dt))
      my.las <- 2
      my.line <- 0.65
      my.adj <- 1
      my.cex <- 0.75

      lab.x.at <- mp[ind]
      lab.x.lab <- format(dt, "%d%b%y")
        
      axis( side=1, at=lab.x.at, labels=rep("", length(lab.x.at)) )
      for( i in 1:length(dt) ){
        mtext( side=1, at=lab.x.at[i], text=lab.x.lab[i], las=my.las, line=my.line, adj=my.adj, cex=my.cex )
      }
    }
  } else {
    print( df )
    if( casefold(s.by) == "month" ){
      dt <-  format(df$date, "%b %Y") 
      my.cex <- 1
    } else {
      
      #   ---- Label weekly (yearly does not get plotted).  
      jDates <- subset(the.Jdates, as.Date(uniqueDate) >= min.date & as.Date(uniqueDate) <= max.date,c(uniqueDate,julianWeek,julianWeekLabel))

      #   ---- Can't figure out how to join on POSIX dates.  So cheating. 
      df$date.alone <- strftime(df$date,format="%x")
      jDates$date.alone <- strftime(jDates$uniqueDate,format="%x")
      df2 <- merge(df,jDates,by = c("date.alone"),all.x=TRUE)
      df2 <- df2[order(df2$uniqueDate),]
      dt <- df2$julianWeekLabel  
      my.cex <- .75
      for( i in 1:length(dt) ){
        mtext( side=1, at=mp[i], text=dt[i], las=2, line=0.1, adj=1, cex=my.cex )
      }     
    }

    for( i in 1:length(dt) ){
      mtext( side=1, at=mp[i], text=dt[i], las=2, line=0.1, adj=1, cex=my.cex )
    }
  }

  #   ---- Add y axis labels.
  lab.y.lab <- format( lab.y.at, scientific=F, big.mark=",", trim=T )
  axis( side=2, at=lab.y.at, labels=lab.y.lab )

  #   ---- Add total passage.
  N <- round(sum( df$passage ))
  mtext( side=3, at=max(mp),  text=paste("N =", format(N, scientific=F, big.mark="," )), adj=1, cex=1 )

  #   ---- Add title.
  mtext( side=3, at=max(mp), text=attr(df,"site.name"), adj=1, cex=1.5, line=2 )
  mtext( side=3, at=max(mp), text= paste("Chinook Salmon, ", attr(df, "run.name"), " run, ", attr(df,"lifestage.name"), sep=""), adj=1, cex=.75, line=1 )

  #   ---- Add legend.  
  legend( "topright", legend=c("Observed","Imputed"),
    fill=c("darkorange", "lightblue"), cex=1)

  #   ---- Output results, based on user desire.
  if( !is.na(out.file) ){
    dev.off(dev.cur())
    ans <- out.pass.graphs
  } else {
    ans <- NULL
  }

  ans

}
