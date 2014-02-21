F.passage <- function( site, taxon, run, min.date, max.date, by, output.file, ci=TRUE ){
#
#   This is the outer wrapper function to estimate passage at a particular site, for a particular run, for a particular taxon.
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
#
#   All lifestages of the run are combined here.

    #   ---- Check that times are less than 1 year apart
    strt.dt <- as.POSIXct( min.date, format="%Y-%m-%d" )
    end.dt <- as.POSIXct( max.date, format="%Y-%m-%d" )
    run.season <- data.frame( start=strt.dt, end=end.dt )
    dt.len <- difftime(end.dt, strt.dt, units="days")
    if( dt.len > 366 )  stop("Cannot specify more than 365 days in F.passage. Check min.date and max.date.")


    #   ---- Upon input, run is the runID (i.e., 3,4,etc.).  We changed the SQL to Connie's code, 
    #        and the catch data comes in as run.name (not code).  It is easiest to translate run 
    #        to the name here.  A bit inefficient, but not much.
    ch <- odbcConnectAccess(db.file)
    luRun <- sqlFetch(ch, "luRun")
    run.name <- luRun$run[ luRun$runID == run ]
    close(ch)



    #   ---- Start a progress bar
    progbar <<- winProgressBar( "Production estimate", label="Reading catch data and assigning plus-counts" )


    #   ---- Fetch the catch and visit data 
    tmp.df   <- F.get.catch.data( site, taxon, min.date, max.date  )
    
    catch.df <- tmp.df$catch   # All positive catches, all FinalRun and lifeStages, inflated for plus counts.  Zero catches (visits without catch) are NOT here.
    visit.df <- tmp.df$visit   # the unique trap visits.  This will be used in a merge to get 0's later
    
    if( nrow(catch.df) == 0 ){
        stop( paste( "No catch records between", min.date, "and", max.date, ". Check dates and taxon."))
    }

    cat("\n\n")
    cat(paste(rep("+",150), collapse="")); cat("\n")
    cat("\n\n")


    #   ---- Fetch efficiency data
    setWinProgressBar( progbar, getWinProgressBar(progbar)*.7 + .3 , label="Reading efficiency data" )
    release.df <- F.get.release.data( site, taxon, min.date, max.date  )

    if( nrow(release.df) == 0 ){
        stop( paste( "No efficiency trials between", min.date, "and", max.date, ". Check dates."))
    }

    cat("\n\n")
    cat(paste(rep("+",150), collapse="")); cat("\n")
    cat("\n\n")

    #   ================================================================= 
    #   by here, we have all the catch, visit, and release data for all runs and lifestages 
    #          of the species between min and max dates.    
    #   ================================================================= 

    
    #   ---- Summarize catch data by batchDate. Upon return, catch.df has one line per trapPosition-batchDate combo during time trap was operating. missing times pre and post season
    catch.df <- F.summarize.fish.visit( catch.df )    

    runs.found <- unique(catch.df$FinalRun)
    runs.found <- runs.found[ !is.na(runs.found) ]
    cat("\nRuns found between", min.date, "and", max.date, ":\n")
    print(runs.found)

    lstages <- unique(catch.df$lifeStage)
    lstages <- lstages[ !is.na(lstages) ]   #   Don't need this,  I am pretty sure lifeStage is never missing here.
    cat("\nLife stages found between", min.date, "and", max.date, ":\n")
    print(lstages)

    cat("\n\n")
    cat(paste(rep("+",150), collapse="")); cat("\n")
    cat("\n\n")

    #   ---- Print the number of non-fishing periods
    cat( paste("\nNumber of non-fishing intervals at all traps:", sum(visit.df$TrapStatus == "Not fishing"), "\n\n"))


    #   ---- Compute passage
    output.fn <- output.file 
    setWinProgressBar( progbar, getWinProgressBar(progbar)*.7 + .3, label="Computing passage" )


    cat(paste(rep("*",80), collapse=""))
    tmp.mess <- paste("Processing run", run, "-", run.name)
    cat(paste("\n", tmp.mess, "\n"))
    cat(paste(rep("*",80), collapse=""))
    cat("\n\n")

    barinc <- 1 / (length(lstages) * 6)
    assign( "progbar", progbar, pos=.GlobalEnv ) 


    indRun <- (catch.df$FinalRun == run.name ) & !is.na(catch.df$FinalRun)   # Don't need is.na clause.  FinalRun is never missing here.

    if( !any( indRun ) ){
        #   Zero fish in this run found.  Write out 0's to output file.
        tmp <- matrix(0,ncol=10)
        tmp <- as.data.frame( tmp )
        names(tmp) <- c("passage","date","pct.imputed.catch","lower.95","upper.95",
            "nForkLenMM", "meanForkLenMM","sdForkLenMM","sampleLengthHrs","sampleLengthDays")
        pass <- data.frame(month="0", tmp)
        
    } else {
        #   We have some of this run, compute production        
        catch.df.ls <- catch.df[ indRun , c("trapVisitID", "FinalRun", "lifeStage", "n.tot", "mean.fl", "sd.fl")]

        #   ---- Merge in the visits to get zeros
        catch.df.ls <- merge( visit.df, catch.df.ls, by="trapVisitID", all.x=T )
        setWinProgressBar( progbar, getWinProgressBar(progbar)+barinc )

        #   ---- Update the constant variables.  Missing n.tot when trap was fishing should be 0.
        catch.df.ls$FinalRun[ is.na(catch.df.ls$FinalRun) ] <- run
        catch.df.ls$lifeStage <- "All"
        catch.df.ls$n.tot[ is.na(catch.df.ls$n.tot) & (catch.df.ls$TrapStatus == "Fishing") ] <- 0

        #   ---- Compute passage
        out.fn.root <- paste0(output.file, "_", run.name )
        pass <- F.est.passage( catch.df.ls, release.df, by, out.fn.root, ci )
        out.fn.roots <- attr(pass, "out.fn.list")        
        
    }

    cat("Final run estimate: ")
    cat(formatC(sum(pass$passage,na.rm=T),big.mark=",", digits=20))
    cat("\n\n")


    cat("\n\n")
    cat(paste(rep("+",150), collapse="")); cat("\n")
    cat("\n\n")



    #   ---- Save .RData for later plotting (grand.df is produced by F.est.passage, and saved in .GlobalEnv
    RData.fname <- "<none>"


    #   ---- Write passage table to a file, if called for
    if( !is.na(output.fn) ){
    
        #   Fix up the pass table to pretty the output
        tmp.df <- pass
        
        
        
        tzn <- get("time.zone", .GlobalEnv )
        tmp.df$date <- as.POSIXct( strptime( format(tmp.df$date, "%Y-%m-%d"), "%Y-%m-%d", tz=tzn),tz=tzn)
        
        tmp.df$passage <- round(tmp.df$passage)
        tmp.df$lower.95 <- round(tmp.df$lower.95)
        tmp.df$upper.95 <- round(tmp.df$upper.95)
        tmp.df$meanForkLenMM <- round(tmp.df$meanForkLenMM,1)
        tmp.df$sdForkLenMM <- round(tmp.df$sdForkLenMM,2)
        tmp.df$pct.imputed.catch <- round(tmp.df$pct.imputed.catch, 3)
        tmp.df$sampleLengthHrs <- round(tmp.df$sampleLengthHrs,1)
        tmp.df$sampleLengthDays <- round(tmp.df$sampleLengthDays,2)
        names(tmp.df)[ names(tmp.df) == "pct.imputed.catch" ] <- "propImputedCatch"
        names(tmp.df)[ names(tmp.df) == "lower.95" ] <- "lower95pctCI"
        names(tmp.df)[ names(tmp.df) == "upper.95" ] <- "upper95pctCI"
        names(tmp.df)[ names(tmp.df) == "nForkLenMM" ] <- "numFishMeasured"

        #   Merge in the trapsOperating column
        tO <- attr(pass, "trapsOperating")
        tmp.df <- merge( tmp.df, tO, by.x="date", by.y="batchDate", all.x=T )

        #   For aesthetics, change number fish measured on days in gaps from NA to 0
        tmp.df$numFishMeasured[ is.na(tmp.df$numFishMeasured) & (tmp.df$nTrapsOperating == 0) ] <- 0

        #   Open file and write out header.
        out.pass.table <- paste(output.fn, "_passage_table.csv", sep="")

        rs <- paste( format(run.season[1], "%d-%b-%Y"), "to", format(run.season[2], "%d-%b-%Y"))
        nms <- names(tmp.df)[1]
        for( i in 2:length(names(tmp.df))) nms <- paste(nms, ",", names(tmp.df)[i], sep="")
    
        cat(paste("Writing passage estimates to", out.pass.table, "\n"))
        
        sink(out.pass.table)
        cat(paste("Site=,", catch.df$siteName[1], "\n", sep=""))
        cat(paste("Site ID=,", catch.df$siteID[1], "\n", sep=""))
        cat(paste("Species ID=,", taxon, "\n", sep=""))
        cat(paste("Run =,", run.name, "\n", sep=""))
        cat(paste("Lifestage =,", catch.df.ls$lifeStage[1], "\n", sep=""))
        cat(paste("Summarized by=,", by, "\n", sep=""))
        cat(paste("Dates included=,", rs, "\n", sep=""))

        cat("\n")
        cat(nms)
        cat("\n")
        sink()
    
        #   Write out the table    
        write.table( tmp.df, file=out.pass.table, sep=",", append=TRUE, row.names=FALSE, col.names=FALSE)
        
        #   Write out the total passage. No other totals in file.
        sink(out.pass.table, append=TRUE)
        cat(paste("Total:,", round(sum(pass$passage,na.rm=T)), "\n"))
        sink()
        
        
        out.fn.roots <- c(out.fn.roots, out.pass.table)
        
        
        

        #   ---- Plot the final passage estimates
        if( by != "year" ){
            attr(pass,"summarized.by") <- by 
            
            tmp.pass <<- pass  # debugging
            
            out.f <- F.plot.passage( pass, max.date = max(catch.df.ls$EndTime,na.rm=T), out.file=output.fn )
            out.fn.roots <- c(out.fn.roots, out.f)
        }
        nf <- length(out.fn.roots)
        
        
        #   ---- Send messages back to the interface
        cat("SUCCESS - F.passage\n\n")
        cat(paste("Working directory:", getwd(), "\n"))
        cat(paste("R data frames saved in file:", RData.fname, "\n\n"))
        cat(paste("Number of files created in working directory =", nf, "\n"))
        for(i in 1:length(out.fn.roots)){
            cat(paste(out.fn.roots[i], "\n", sep=""))
        }
        cat("\n")

        setWinProgressBar( progbar, getWinProgressBar(progbar), label="SUCCESS" )
        close( progbar )


    }    
  
    
invisible(tmp.df)
}
