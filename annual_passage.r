F.annual.passage <- function( site, taxon, run, min.date, max.date, output.file, ci=TRUE ){
#
#   Estimate passage for all years in the data base.
#
#   site = site number
#   taxon = taxon number
#   run = run number
#   output.file = name of .CSV file to put passage estimates into.  Full path is allowed.
#
#   Annual production is estimated for every year in the data base, from minimum date to maximum 
#   date.  "years" are (min.date to min.date+365days), (min.date+365days, min.date+2(365)days), etc...
#

#   ---- Pull the min and max dates from the data base

#   Retrieve db file name and table names
tables <- get( "table.names", env=.GlobalEnv )
db <- get( "db.file", env=.GlobalEnv ) 

#   Open ODBC channel
ch <- odbcConnectAccess(db)

s.tab <- tables["sites"]
ss.tab <- tables["subsites"]
tv.tab <- tables["trap.visit"]

strt.date <- as.POSIXct( min.date, format="%Y-%m-%d" )
end.date <- as.POSIXct( max.date, format="%Y-%m-%d" )
    
##   Pull all visits where fish were processed in order to determine min and max dates
#   USE THIS CODE TO COMPUTE ESTIMATES FOR EVERY YEAR IN THE DATA BASE
#sql.visit <- paste( "SELECT visitTime",  
#    " FROM (", s.tab, " INNER JOIN ", ss.tab, " ON ", s.tab, ".siteID", " = ", ss.tab, ".siteID", ")", 
#    " INNER JOIN ", tv.tab, " ON ", ss.tab, ".subsiteID", "=", tv.tab, ".trapPositionID", 
#    " WHERE (((", s.tab, ".siteID)=", site, ") ",
#    "AND ((", tv.tab, ".visitTypeID)=2 Or (", tv.tab, ".visitTypeID)=4) ",
#    "AND ((", tv.tab, ".includeCatchID)=1 Or (", tv.tab, ".includeCatchID)>250)); ",
#    sep="")
#
#visit <- sqlQuery(ch, sql.visit)
#F.sql.error.check(visit)
#
#strt.date <- min( visit$visitTime )
#end.date <- max( visit$visitTime )



#   ---- Call F.passage, once for each year, to estimate passage.  This is done so that catch and efficiency models 
#        are estimated within year only.
secs.in.year <- round(60*60*24*365.25)
yr.cuts <- seq( strt.date, end.date+secs.in.year, by=secs.in.year )
class(yr.cuts) <- class(strt.date)

null<- rep(NA, length(yr.cuts)-1)
ans <- data.frame(year=null, passage=null, pct.imputed=null, lower.ci=null, upper.ci=null)

#       Loop over years
for( i in 1:(length(yr.cuts)-1) ){
    min.date <- format(yr.cuts[i], "%Y-%m-%d")
    max.date <- format(yr.cuts[i+1], "%Y-%m-%d")
 
    cat("\n\n\n\n\n===========================================================\n")
    cat(paste("==============", min.date, "-", max.date, "===================\n"))
    cat("===========================================================\n\n")
       
    pass <- F.passage( site, taxon, run, min.date, max.date, "year", NA, ci )
    
    if( nrow(pass) != 1 ){
        print(pass)
    }
    
    ans$year[i] <- as.numeric(pass$year)
    ans$passage[i] <- as.numeric(pass$passage)
    ans$pct.imputed[i] <- as.numeric(pass$pct.imputed)

    ci.col <- grep("lower", names(pass))  # must do this because CI col name is actually 'lower.XX' where XX is confidence level. 
    ans$lower.ci[i] <- as.numeric(pass[,ci.col])
    ci.col <- grep("upper", names(pass))  # must do this because CI col name is actually 'upper.XX' where XX is confidence level. 
    ans$upper.ci[i] <- as.numeric(pass[,ci.col])
}    


##   ---- Summarize by year.  
##        Note: a 'year' is defined as 365 days from min.date.  If min.date = 1 Sep 1999, the annual 
##        estimates are for 1Sep99 - 31Aug00, 1Sep00 - 31Aug01, etc.  Not calender years (unless min.date = 1Jan)
#print("okay")
#
#strt.date <- as.POSIXct( min.date, format="%Y-%m-%d" )
#print(strt.date)
#
#end.date <- as.POSIXct( max.date, format="%Y-%m-%d" )
#print(end.date)
#
#secs.in.year <- round(60*60*24*365.25)
#print(secs.in.year)
#
#yr.cuts <- c(-Inf, seq( strt.date, end.date+secs.in.year, by=secs.in.year ))
#print(yr.cuts)
#
#
#
#year.ind <- findInterval( pass$meanDate, yr.cuts )
#class(yr.cuts) <- class(strt.date)
#yr.labs <- format( yr.cuts, "%Y" )
#print(yr.labs)
#
#year <- yr.labs[year.ind]
#
#print(cbind(pass, year)[1:50,])
#
#year.est <- tapply( pass$passage, year, sum )
#print(year.est)
#
##   ---- Done
#return(data.frame( year=names(year.est), passage=year.est))
    #   ---- Plot results
    if( !is.na(output.file) ){
        #   ---- Open PNG device
        out.pass.graphs <- paste(output.file, "_ann_passage.png", sep="")
        if(file.exists(out.pass.graphs)){
            file.remove(out.pass.graphs)
        }
        png(file=out.pass.graphs,width=7,height=7,units="in",res=600)
    }
    
    plot( ans$year, ans$passage, xlab="Year", ylab="Estimated passage (# fish)", pch=16 )
    if( nrow(ans) > 5 ){
        lines( supsmu(ans$year, ans$passage), col="darkred" )
    } else {
        lines( ans$year, ans$passage, col="darkred", type="b" )
    }
    
    if( !is.na(output.file) ){
        dev.off(dev.cur())
    }



    #   ---- Write annual passage table to a file, if called for
    if( !is.na(output.file) ){
        out.pass.table <- paste(output.file, "_Annual_passage_table.csv", sep="")
        rs <- c(strt.date, end.date)
        rs <- paste( format(rs[1], "%d-%b-%Y"), "to", format(rs[2], "%d-%b-%Y"))
        nms <- paste( names(ans), collapse=",")
    
        cat(paste("Writing annual passage estimates to", out.pass.table, "\n"))
        
        sink(out.pass.table)
        cat(paste("Site=,", attr(pass,"site.name"), "\n", sep=""))
        cat(paste("Site abbreviation=,", attr(pass,"site.abbr"), "\n", sep=""))
        cat(paste("Site ID=,", attr(pass, "siteID"), "\n", sep=""))
        cat(paste("Species=,", attr(pass,"species.name"), "\n", sep=""))
        cat(paste("Species ID=,", attr(pass,"taxonID"), "\n", sep=""))
        cat(paste("Run=,", attr(pass,"run.name"), "\n", sep=""))
        cat(paste("Run ID=,", attr(pass,"runID"), "\n", sep=""))
        cat(paste("Dates included=,", rs, "\n", sep=""))
        cat(paste("Summarized by=,", attr(pass,"summarized.by"), "\n", sep=""))
        cat("\n")
        cat(nms)
        cat("\n")
        sink()
    
        write.table( ans, file=out.pass.table, sep=",", append=TRUE, row.names=FALSE, col.names=FALSE)
        
        #   ---- Send messages back to the interface
        cat("SUCCESS - F.annual.passage\n\n")
        cat(paste("Working directory:", getwd(), "\n"))
        cat(paste("R data frames saved in file:", "none", "\n\n"))
        cat("Number of files created in working directory = 2\n")
        cat(paste(out.pass.table, "\n"))
        cat(paste(out.pass.graphs, "\n"))
        cat("\n")
    }



invisible(ans)

}

