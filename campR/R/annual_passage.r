#' @export F.annual.passage
#' 
#' @title F.annual.passage
#' 
#' @description
#' 
#'    Estimate passage for all years in the data base.
#' 
#'    site = site number
#'    taxon = taxon number
#'    run = run number
#'    output.file = name of .CSV file to put passage estimates into.  Full path is allowed.
#' 
#'    Annual production is estimated for every year in the data base, from minimum date to maximum 
#'    date.  "years" are (min.date to min.date+365days), (min.date+365days, min.date+2(365)days), etc...
#' 
#' 
#'    ---- Pull the min and max dates from the data base
#' 
#' @param  site <describe argument>
#' @param  taxon <describe argument>
#' @param  run <describe argument>
#' @param  min.date <describe argument>
#' @param  max.date <describe argument>
#' @param  output.file <describe argument>
#' @param  ci=TRUE  <describe argument>
#' 
#' @details <other comments found in file>
#'    Open ODBC channel
#'    Pull all visits where fish were processed in order to determine min and max dates
#'    USE THIS CODE TO COMPUTE ESTIMATES FOR EVERY YEAR IN THE DATA BASE
#' sql.visit <- paste( "SELECT visitTime",  
#'     " FROM (", s.tab, " INNER JOIN ", ss.tab, " ON ", s.tab, ".siteID", " = ", ss.tab, ".siteID", ")", 
#'     " INNER JOIN ", tv.tab, " ON ", ss.tab, ".subsiteID", "=", tv.tab, ".trapPositionID", 
#'     " WHERE (((", s.tab, ".siteID)=", site, ") ",
#'     "AND ((", tv.tab, ".visitTypeID)=2 Or (", tv.tab, ".visitTypeID)=4) ",
#'     "AND ((", tv.tab, ".includeCatchID)=1 Or (", tv.tab, ".includeCatchID)>250)); ",
#'     sep="")
#' 
#' visit <- sqlQuery(ch, sql.visit)
#' F.sql.error.check(visit)
#' 
#' strt.date <- min( visit$visitTime )
#' end.date <- max( visit$visitTime )
#'    ---- Call F.passage, once for each year, to estimate passage.  This is done so that catch and efficiency models 
#'         are estimated within year only.
#' secs.in.year <- round(60*60*24*365.25)
#' yr.cuts <- seq( strt.date, end.date+secs.in.year, by=secs.in.year )
#' class(yr.cuts) <- class(strt.date)
#'        Loop over years
#'    ---- Plot results
#'    ---- Open PNG device
#'    ---- Write annual passage table to a file, if called for
#'    ---- Send messages back to the interface
#' 
#' @return <describe return value>
#' 
#' @author WEST Inc.
#' 
#' @seealso \code{\link{<related routine>}}, \code{\link{<related routine>}}
#' 
#' @examples
#' <insert examples>
#' 
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
    strt.year <- as.numeric( format( strt.date, "%Y" ) )
    strt.day <- as.numeric( format( strt.date, "%d" ) )
    strt.mon <- as.numeric( format( strt.date, "%m" ) )
    end.year <- as.numeric( format( end.date, "%Y" ) )
    yrs.used <- strt.year:end.year
    
    yr.cuts <- as.POSIXct( strptime( paste(yrs.used, strt.mon, strt.day), "%Y %m %d" ) )
    
    
    #secs.in.year <- round(60*60*24*365.25)
    #yr.cuts <- seq( strt.date, end.date+secs.in.year, by=secs.in.year )
    #class(yr.cuts) <- class(strt.date)
    
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
        
        ans$year[i] <- as.numeric(format(yr.cuts[i], "%Y"))
        ans$passage[i] <- as.numeric(pass$passage)
        ans$pct.imputed[i] <- as.numeric(pass$pct.imputed)
    
        ci.col <- grep("lower", names(pass))  # must do this because CI col name is actually 'lower.XX' where XX is confidence level. 
        ans$lower.ci[i] <- as.numeric(pass[,ci.col])
        ci.col <- grep("upper", names(pass))  # must do this because CI col name is actually 'upper.XX' where XX is confidence level. 
        ans$upper.ci[i] <- as.numeric(pass[,ci.col])
    }    
    
    tmp.1 <<- ans

    if( all(ans$passage == 0) ){
        cat("FAILURE - F.annual.passage - All zero passage estimates\nCheck dates and finalRunID's.\n")
        cat(paste("Working directory:", getwd(), "\n"))
        cat(paste("R data frames saved in file:", "none", "\n\n"))
        cat("Number of files created in working directory = 0\n")
        cat("\n")
        return(0)
    }
    
    #   ---- Plot results
    if( !is.na(output.file) ){
        #   ---- Open PNG device
        out.pass.graphs <- paste(output.file, "_ann_passage.png", sep="")
        if(file.exists(out.pass.graphs)){
            file.remove(out.pass.graphs)
        }
        png(file=out.pass.graphs,width=7,height=7,units="in",res=600)
    }

    rs <- c(strt.date, end.date)
    rs <- paste( format(rs[1], "%d-%b-%Y"), "to", format(rs[2], "%d-%b-%Y"))

    ans.2 <- ans
    ans.2 <- ans.2[ !is.na(ans.2$passage), ]
    ans.2 <- ans.2[ ans.2$passage > 0, ]
    rng.y <- range(ans.2$passage, ans.2$lower.ci, ans.2$upper.ci, na.rm=T)
    rng.x <- c(min(ans.2$year, na.rm=T)-1, max(ans.2$year, na.rm=T)+1)    
    if( max(rng.y,na.rm=T) > 10*max(ans.2$passage,na.rm=T) ){
        tmp.1 <- ans.2$passage[ -which.max(ans.2$upper.ci) ]
        rng.y[2] <- max(tmp.1, na.rm=T)  # second max
    }
    
    plot( rng.x, rng.y, type="n", xlab="Year", ylab="Passage estimate (# fish)", yaxt="n", xaxt="n", cex.lab=1.5 )

    mtext( side=3, text=attr(pass,"site.name"), adj=1, cex=1.5, line=2 )
    mtext( side=3, text= paste(attr(pass,"species.name"), ",", attr(pass,"run.name"), "run,", rs), adj=1, cex=.75, line=1 )

    lab.y.at <- axTicks(2)
    lab.y.lab <- formatC( lab.y.at, big.mark=",", digits=9 )
    axis( side=2, at=lab.y.at, label=lab.y.lab )
    
    axis( side=1, at=ans.2$year )

    for( i in 1:nrow(ans.2)){
        points( ans.2$year[i], ans.2$passage[i], pch=16, cex=2 )
        lines( rep(ans.2$year[i],2), c(ans.2$lower.ci[i], ans.2$upper.ci[i]) )
        lines( rep(ans.2$year[i],2)+c(-0.1,0.1), rep(ans.2$lower.ci[i],2),  )
        lines( rep(ans.2$year[i],2)+c(-0.1,0.1), rep(ans.2$upper.ci[i],2),  )
    }
    
    if( nrow(ans.2) > 5 ){
        lines( supsmu(ans.2$year, ans.2$passage), col="darkred" )
    } else {
        lines( ans.2$year, ans.2$passage, col="darkred", type="b" )
    }
    
    if( !is.na(output.file) ){
        dev.off(dev.cur())
    }



    #   ---- Write annual passage table to a file, if called for
    if( !is.na(output.file) ){
        out.pass.table <- paste(output.file, "_Annual_passage_table.csv", sep="")
        rs <- c(strt.date, end.date)
        rs <- paste( format(rs[1], "%d-%b-%Y"), "to", format(rs[2], "%d-%b-%Y"))
    
        cat(paste("Writing annual passage estimates to", out.pass.table, "\n"))

        tmp.df <- ans
        tmp.df$pct.imputed <- round(tmp.df$pct.imputed, 3)
        tmp.df$passage <- round(tmp.df$passage)
        tmp.df$lower.ci <- round(tmp.df$lower.ci)
        tmp.df$upper.ci <- round(tmp.df$upper.ci)
        names(tmp.df)[ names(tmp.df) == "pct.imputed" ] <- "propImputedCatch"
        names(tmp.df)[ names(tmp.df) == "lower.ci" ] <- "lower95pctCI"
        names(tmp.df)[ names(tmp.df) == "upper.ci" ] <- "upper95pctCI"
        nms <- paste( names(tmp.df), collapse=",")

        
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
    
        write.table( tmp.df, file=out.pass.table, sep=",", append=TRUE, row.names=FALSE, col.names=FALSE)
        
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

