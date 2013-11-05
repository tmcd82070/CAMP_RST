F.passage <- function( site, taxon, run, min.date, max.date, by, output.file, ci=TRUE ){
#
#   This is the outer wrapper function to estimate passage at a particular site, for a particular run, for a particular taxon for a particular year.
#
#   Inputs:
#   site = site number
#   taxon = taxon number
#   run = run number
#   min.date and max.date = minimum and maximum dates of visits to include in the analysis. This is a text 
#           string in the format %Y-%m-%d, or YYYY-MM-DD.
#   by = "day", "week", "month", or "year".  Estimates are produce "by" this time frame.
#   output.file = name of .CSV file to put passage estimates into.  Assume it's the file name only.  Pre-appended with output.dir.
#   ci = T or F for whether to run or not run bootstrap confidence intervals for passage.
#
#   NOTE: because we want to estimate GLM models by year, this routine should only estimate
#   passage for at most one year.  Check this.

    #   ---- Check that times are less than 1 year apart
    strt.dt <- as.POSIXct( min.date, format="%Y-%m-%d" )
    end.dt <- as.POSIXct( max.date, format="%Y-%m-%d" )
    dt.len <- difftime(end.dt, strt.dt, units="days")
    if( dt.len > 366 )  stop("Cannot specify more than 365 days in F.passage. Check min.date and max.date.")

    #   ---- Start a progress bar
    progbar <<- winProgressBar( "Passage estimates", label="Reading catch data and assigning plus-counts" )

    #   ---- Fetch the catch data
    catch.df   <- F.get.catch.data( site, taxon, run, min.date, max.date  )

    if( nrow(catch.df) > 0 ){
        
        cat("\n\n")
        cat(paste(rep("+",150), collapse="")); cat("\n")
        cat("\n\n")
    
        #   ---- Fetch efficiency data
        setWinProgressBar( progbar, getWinProgressBar(progbar)*.7 + .3 , label="Reading efficiency data" )
        release.df <- F.get.release.data( site, taxon, min.date, max.date  )
    
        cat("\n\n")
        cat(paste(rep("+",150), collapse="")); cat("\n")
        cat("\n\n")
    
        #   ---- Compute passage
        output.fn <- output.file 
        setWinProgressBar( progbar, getWinProgressBar(progbar)*.7 + .3, label="Computing passage" )
    
        pass <- F.est.passage( catch.df, release.df, by, output.fn, ci )
    
        names(pass)[ names(pass) == "imputed" ] <- "pct.imputed"
        out.fn.list <- attr(pass, "out.fn.list")

    } else {
        #   No visits found, dates are probably wrong
        output.fn <- output.file 
        out.fn.list <- NULL
        tzn <- "America/Los_Angeles"
        cat("No visits found, check the dates\n")
        pass.nms <- c("year","passage","date","pct.imputed.catch","lower.95","upper.95",
            "nForkLenMM","meanForkLenMM","sdForkLenMM","sampleLengthHrs","sampleLengthDays")
        pass <- as.data.frame( matrix(NA, 1, length(pass.nms)) )
        names(pass) <- pass.nms
        pass$date <- as.POSIXct( strptime( "1970-01-01", "%Y-%m-%d", tz=tzn),tz=tzn)

        attr(pass,"site.name") <- attr(catch.df, "site.name")
        attr(pass,"site.abbr") <- attr(catch.df, "site.abbr")
        attr(pass,"siteID") <- site
        attr(pass,"species.name") <- attr(catch.df, "species.name")
        attr(pass,"taxonID") <- taxon
        attr(pass,"run.name") <- attr(catch.df, "run.name")
        attr(pass,"runID") <- attr(catch.df, "runID")
        attr(pass,"summarized.by") <- by
        attr(pass,"run.season") <- as.POSIXct( strptime(c(min.date, max.date), "%Y-%m-%d") ) 
    }

    cat("\n\n")
    cat(paste(rep("+",150), collapse="")); cat("\n")
    cat("\n\n")


    #   ---- Save .RData for later plotting (grand.df is produced by F.est.passage, and saved in .GlobalEnv
    #RData.fname <- format(Sys.time(), "%Y-%m-%d_%H%M%S") 
    #RData.fname <- paste("Passage_", RData.fname, ".RData", sep="") 
    #save( list=c("pass", "catch.df", "release.df", "grand.df"), file=RData.fname)
    RData.fname <- "<none>"


    #   ---- Write passage table to a file, if called for
    if( !is.na(output.fn) ){
        setWinProgressBar( progbar, getWinProgressBar(progbar)*.7 + .3, label="Writing to disk" )
    
        out.pass.table <- paste(output.fn, "_passage_table.csv", sep="")
        rs <- attr(pass,"run.season")
        rs <- paste( format(rs[1], "%d-%b-%Y"), "to", format(rs[2], "%d-%b-%Y"))

        #   Fix up the pass table to pretty the output
        tmp.df <- pass
        tmp.df$pct.imputed.catch <- round(tmp.df$pct.imputed.catch, 3)
        
        #tmp.ind <- is.na(tmp.df$date)
        #print(class(tmp.df$date))

        tzn <- attr( catch.df$visitTime, "tz" )
        if( is.null(tzn) ){
            tzn <- "America/Los_Angeles"
        }
        tmp.df$date <- as.POSIXct( strptime( format(tmp.df$date, "%Y-%m-%d"), "%Y-%m-%d", tz=tzn),tz=tzn)
        
        tmp.df$passage <- round(tmp.df$passage)
        tmp.df$lower.95 <- round(tmp.df$lower.95)
        tmp.df$upper.95 <- round(tmp.df$upper.95)
        tmp.df$meanForkLenMM <- round(tmp.df$meanForkLenMM,1)
        tmp.df$sdForkLenMM <- round(tmp.df$sdForkLenMM,2)
        tmp.df$sampleLengthHrs <- round(tmp.df$sampleLengthHrs,1)
        tmp.df$sampleLengthDays <- round(tmp.df$sampleLengthDays,2)
        names(tmp.df)[ names(tmp.df) == "pct.imputed.catch" ] <- "propImputedCatch"
        names(tmp.df)[ names(tmp.df) == "lower.95" ] <- "lower95pctCI"
        names(tmp.df)[ names(tmp.df) == "upper.95" ] <- "upper95pctCI"
        names(tmp.df)[ names(tmp.df) == "nForkLenMM" ] <- "numFishMeasured"


        nms <- paste(names(tmp.df), collapse=",")  # do this to avoid warning "Appending column names to existing file"

    
        cat(paste("Writing passage estimates to", out.pass.table, "\n"))
        
        sink(out.pass.table)
        cat(paste("Site=,", attr(pass,"site.name"), "\n", sep=""))
        cat(paste("Site abbreviation=,", attr(pass,"site.abbr"), "\n", sep=""))
        cat(paste("Site ID=,", attr(pass,"siteID"), "\n", sep=""))
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

        #   Write out the prettied table    
        write.table( tmp.df, file=out.pass.table, sep=",", append=TRUE, row.names=FALSE, col.names=FALSE)
        out.fn.list <- c(out.fn.list, out.pass.table)

        #   ---- Plot the final passage estimates
        if( by != "year" ){
            out.f <- F.plot.passage( pass, max.date = max(catch.df$visitTime2,na.rm=T), out.file=output.fn )
            out.fn.list <- c(out.fn.list, out.f)
        }
        nf <- length(out.fn.list)
        
        
        #   ---- Send messages back to the interface
        cat("SUCCESS - F.passage\n\n")
        cat(paste("Working directory:", getwd(), "\n"))
        cat(paste("R data frames saved in file:", RData.fname, "\n\n"))
        cat(paste("Number of files created in working directory =", nf, "\n"))
        for(i in 1:length(out.fn.list)){
            cat(paste(out.fn.list[i], "\n", sep=""))
        }
        cat("\n")

        setWinProgressBar( progbar, getWinProgressBar(progbar), label="SUCCESS" )
        close( progbar )


    }    
  
    
    invisible(pass)
}
