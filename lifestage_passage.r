F.lifestage.passage <- function( site, taxon, min.date, max.date, output.file, ci=TRUE ){
#
#   ANNUAL PRODUCTION ESTIMATES BY LIFE STAGE AND RUN – TABULAR SUMMARY
#   A table of passage estimates, with lifestages down the rows, and runs across the columns.
#
#   Input:
#   site = site ID of the place we want, trap locaton
#   taxon = taxon number (from luTaxon) to retrieve
#

    #   ********
    #   Check that times are less than 1 year apart
    strt.dt <- as.POSIXct( min.date, format="%Y-%m-%d" )
    end.dt <- as.POSIXct( max.date, format="%Y-%m-%d" )
    run.season <- data.frame( start=strt.dt, end=end.dt )
    dt.len <- difftime(end.dt, strt.dt, units="days")
    if( dt.len > 366 )  stop("Cannot specify more than 365 days in F.passage. Check min.date and max.date.")

    #   ---- Fetch efficiency data
    release.df <- F.get.release.data( site, run, min.date, max.date  )

    if( nrow(release.df) == 0 ){
        stop( paste( "No efficiency trials between", min.date, "and", max.date, ". Check dates."))
    }


    #   ---- Fetch the catch data (This has all FinalRun and lifeStages, inflated for plus counts)
    catch.df   <- F.get.catch.data( site, taxon, min.date, max.date  )

    if( nrow(catch.df) == 0 ){
        stop( paste( "No catch records between", min.date, "and", max.date, ". Check dates and taxon."))
    }
    
    #   ---- Summarize catch data by trapVisitID X FinalRun X lifeStage. Upon return, catch.df has one line per combination of these variables 
    catch.df <- F.summarize.fish.visit( catch.df )    

    runs <- unique(catch.df$FinalRun)
    cat("Runs found between", min.date, "and", max.date, ":\n")
    print(runs)


    lstages <- unique(catch.df$lifeStage)
    cat("Life stages found between", min.date, "and", max.date, ":\n")
    print(lstages)

    #   ---- Extract the unique trap visits.  This will be used in merge to get 0's later
    ind <- !duplicated( catch.df$trapVisitID ) & !is.na(catch.df$trapVisitID)
    visit.df <- catch.df[ind, ]
    visit.df <- visit.df[, !(names(visit.df) %in% c("FinalRun", "lifeStage", "n.tot", "mean.fl", "sd.fl"))] 

    #   ********
    #   Loop over runs
    ans <- lci <- uci <- matrix(0, length(lstages), length(runs))
    dimnames(ans)<-list(lstages, runs)
    
    
    out.fn.roots <- NULL
    for( j in 1:length(runs) ){

        run.name <- runs[j]

        tmp.mess <- paste("Processing ", run.name)
        cat(paste(tmp.mess, "\n\n"))

        progbar <- winProgressBar( tmp.mess, label="Lifestage X run processing" )
        barinc <- 1 / (length(lstages) * 6)
        assign( "progbar", progbar, pos=.GlobalEnv ) 

        indRun <- (catch.df$FinalRun == run.name ) & !is.na(catch.df$FinalRun)
            
        #   ---- Loop over lifestages
        for( i in 1:lstages ){

            ls <- lstages[i]
            
            #   ---- Subset to just one life stage and run
            indLS <- (catch.df$lifeStage == ls) & !is.na(catch.df$lifeStage) 

            cat(paste("Lifestage=", ls, "; Run=", run.name, "; num records=", sum(indRun & indLS), "\n"))
            tmp.mess <- paste("Lifestage=", ls )
            setWinProgressBar( progbar, getWinProgressBar(progbar)+barinc, label=tmp.mess )                
                                
            #   ---- If we caught this run and lifestage, compute passage estimate. 
            if( any( indRun & indLS ) ){

                catch.df.ls <- catch.df[ indRun & indLS, c("trapVisitID", "FinalRun", "lifeStage", "n.tot", "mean.fl", "sd.fl")]

                #   ---- Merge in the visits to get zeros
                catch.df.ls <- merge( visit, catch.fl, by="trapVisitID", all.x=T )
                setWinProgressBar( progbar, getWinProgressBar(progbar)+barinc )

                catch.df.ls$n.tot[ is.na(catch.df.ls$n.tot) ] <- 0

                #   ---- Add back in the missing trapVisitID rows.  These identify the gaps in fishing
                catch.df.ls <- rbind( catch.df.ls, catch.df[ is.na(catch.df$trapVisitID), ] )
                
                #   ---- Compute passage
                out.fn.root <- paste0(output.file, ls, run.name ) 
                setWinProgressBar( progbar, getWinProgressBar(progbar)+barinc )

                pass <- F.est.passage( catch.df.ls, release.df, "year", out.fn.root, ci )

                setWinProgressBar( progbar, getWinProgressBar(progbar)+barinc )
                out.fn.roots <- c(out.fn.roots, attr(pass, "out.fn.list"))

                #print(pass)
                
                #   ---- Save
                ans[ i, j ] <- pass$passage
                lci[ i, j ] <- pass$lower.95
                uci[ i, j ] <- pass$upper.95
                setWinProgressBar( progbar, getWinProgressBar(progbar)+barinc )
                
            } 
            
        }
        
        
        close(progbar)
    }
    
#    ans <<- ans
#    ans <- get("ans")
    
    cat("Final lifeStage X run estimates:\n")
    print(ans)
    
    #   ---- compute percentages of each life stage
    ans.pct <- matrix( colSums( ans ), byrow=T, ncol=ncol(ans), nrow=nrow(ans))
    ans.pct <- ans / ans.pct
    ans.pct[ is.na(ans.pct) ] <- NA
    
    
    #   ---- Write out the table
    df <- data.frame( dimnames(ans)[[1]], ans.pct[,1], ans[,1], lci[,1], uci[,1], stringsAsFactors=F )
    for( j in 2:ncol(ans) ){
        df <- cbind( df, data.frame( ans.pct[,j], ans[,j], lci[,j], uci[,j], stringsAsFactors=F ))
    }
    names(df) <- c("LifeStage", paste( rep(runs$run, each=4), rep( c(".propOfPassage",".passage",".lower95pctCI", ".upper95pctCI"), nrow(runs)), sep=""))
    
 
    #   ---- Append totals to bottom
    tots <- data.frame( "Total", matrix( colSums(df[,-1]), nrow=1), stringsAsFactors=F)
    names(tots) <- names(df)
    tots[,grep("lower.95", names(tots),fixed=T)] <- NA
    tots[,grep("upper.95", names(tots),fixed=T)] <- NA
    df <- rbind( df, Total=tots )
 
    if( !is.na(output.file) ){
        out.pass.table <- paste(output.file, "_lifestage_passage_table.csv", sep="")
        rs <- paste( format(run.season[1], "%d-%b-%Y"), "to", format(run.season[2], "%d-%b-%Y"))
        nms <- names(df)[1]
        for( i in 2:length(names(df))) nms <- paste(nms, ",", names(df)[i], sep="")
    
        cat(paste("Writing passage estimates to", out.pass.table, "\n"))
        
        sink(out.pass.table)
        cat(paste("Site=,", catch.df$siteName[1], "\n", sep=""))
        cat(paste("Site ID=,", catch.df$siteID[1], "\n", sep=""))
        cat(paste("Species ID=,", taxon, "\n", sep=""))
        cat(paste("Dates included=,", rs, "\n", sep=""))

        cat("\n")
        cat(nms)
        cat("\n")
        sink()
    
        write.table( df, file=out.pass.table, sep=",", append=TRUE, row.names=FALSE, col.names=FALSE)
        out.fn.roots <- c(out.fn.roots, out.pass.table)
        
        
        # Produce pie or bar charts
        fl <- F.plot.lifestages( df, output.file, plot.pies=F )
        if( fl == "ZEROS" ){
            cat("FAILURE - F.lifestage.passage - ALL ZEROS\nCheck dates and finalRunId's\n")
            cat(paste("Working directory:", getwd(), "\n"))
            cat(paste("R data frames saved in file:", "<none>", "\n\n"))
            nf <- length(out.fn.roots)
            cat(paste("Number of files created in working directory = ", nf, "\n"))
            for(i in 1:length(out.fn.roots)){
                 cat(paste(out.fn.roots[i], "\n", sep=""))
            }    
            cat("\n")
            return(0)    
        
        } else {
            out.fn.roots <- c(out.fn.roots, fl)
        }
        
        #fl <- F.plot.runs( df, output.file, plot.pies=F )
        #out.fn.roots <- c(out.fn.roots, fl)
    }
    
    #   ---- Write out message
    cat("SUCCESS - F.lifestage.passage\n\n")
    cat(paste("Working directory:", getwd(), "\n"))
    cat(paste("R data frames saved in file:", "<none>", "\n\n"))
    nf <- length(out.fn.roots) 
    cat(paste("Number of files created in working directory = ", nf, "\n"))
    for(i in 1:length(out.fn.roots)){
         cat(paste(out.fn.roots[i], "\n", sep=""))
     }    
     cat("\n")    
 
df    
}

