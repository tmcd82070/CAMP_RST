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
    
    cat("\n\n")
    cat(paste(rep("+",150), collapse="")); cat("\n")
    cat("\n\n")

    #   ---- Fetch efficiency data
    setWinProgressBar( progbar, getWinProgressBar(progbar)*.7 + .3 , label="Reading efficiency data" )
    release.df <- F.get.release.data( site, run, min.date, max.date  )

    cat("\n\n")
    cat(paste(rep("+",150), collapse="")); cat("\n")
    cat("\n\n")

    #   ---- Compute passage
    output.fn <- output.file 
    setWinProgressBar( progbar, getWinProgressBar(progbar)*.7 + .3, label="Computing passage" )
    pass <- F.est.passage( catch.df, release.df, by, output.fn, ci )
    names(pass)[ names(pass) == "imputed" ] <- "pct.imputed"
    out.fn.list <- attr(pass, "out.fn.list")


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
        nms <- names(pass)[1]
        for( i in 2:length(names(pass))) nms <- paste(nms, ",", names(pass)[i], sep="")
    
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
    
        write.table( pass, file=out.pass.table, sep=",", append=TRUE, row.names=FALSE, col.names=FALSE)
        out.fn.list <- c(out.fn.list, out.pass.table)

        #   ---- Plot the final passage estimates, just for good measure. 
        if( by != "year" ){
            out.f <- F.plot.passage( pass, out.file=output.fn )
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
