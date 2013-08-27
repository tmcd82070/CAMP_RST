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



    #   *******
    #   Open ODBC channel and retrieve tables we need
    ch <- odbcConnectAccess(db.file)

    sites <- sqlQuery( ch, paste("SELECT siteName, siteAbbreviation, siteID, streamName FROM", table.names["sites"], 
            "WHERE (siteID =", site, ")" ))
    F.sql.error.check(sites)        
    site.abbr <- as.character(sites$siteAbbreviation)
    site.name <- as.character(sites$siteName)

    sp.codes <- sqlQuery(ch, paste("SELECT taxonID, commonName FROM", table.names["species.codes"]))
    F.sql.error.check(sp.codes)
    sp.commonName <- as.character(sp.codes$commonName[ sp.codes$taxonID %in% taxon ])


    tmp <- F.get.life.stages( )    
    CAMP.life.stages <- tmp$stages
    CAMP.life.stageIDs <- tmp$IDs
    rst.life.stages <- tmp$rst.ls
    

    runs <- sqlFetch(ch, table.names["run.codes"] )
    runs <- runs[ runs$runID < 200, ]
    runs <- runs[order(runs$runID),]
    
    
    cat("Runs under consideration:\n")
    print(runs)


    cat("Life stages under consideration:\n")
    print(cbind(CAMP.life.stageIDs, CAMP.life.stages))

    close(ch)


    #   ********
    #   An internal function to convert lifestages to CAMP lifestages
    f.to.camp.lifestages <- function(lstage, rst.life.stage){
        u.l.s <- sort(unique(lstage))
        for( l.s in u.l.s ){
            camp.l.s <- rst.life.stage$lifeStageCAMPID[ rst.life.stage$lifeStageID == l.s ]
            lstage[ lstage == l.s ] <- camp.l.s
        }
        lstage
    }


    #   ********
    #   Loop over runs
    ans <- lci <- uci <- matrix(0, length(CAMP.life.stages), length(runs$runID))
    dimnames(ans)<-list(CAMP.life.stages, runs$run)
    
    
    out.fn.roots <- NULL
    for( run in runs$runID ){

        run.name <- runs$run[which(run==runs$runID)]

        tmp.mess <- paste("Processing run =", run, "-", run.name)
        cat(paste(tmp.mess, "\n\n"))

        progbar <- winProgressBar( tmp.mess, label="Reading catch data" )
        barinc <- 1 / (length(CAMP.life.stageIDs) * 6)
        assign( "progbar", progbar, pos=.GlobalEnv ) 

        #   ---- Fetch the catch data
        catch.df   <- F.get.indiv.fish.data( site, taxon, run, min.date, max.date, keep="unmarked" )
        #   the subsites attributes of catch.df may be wrong. IF they did not catch the taxon-run of interest at 
        #   a particular subsite, it does not show in subsites attribute.  Use the subsites attribute of visit data frame.
        
        cat(paste('Number of fish records for run', run, "-", run.name, "=", nrow(catch.df), "\n"))

        if( nrow(catch.df) > 0 ){
            cat(paste('Number of missing lifestages=', sum(is.na(catch.df$lifeStageID)), ".  All converted to '251'.\n"))

            #   ---- get visits during the run.  Will need this later.
            visit <- F.get.indiv.visit.data( site, run, min.date, max.date )

            #   ---- Fetch efficiency data
            release.df <- F.get.release.data( site, run, min.date, max.date  )
            
            #   ---- Convert R's missing to Camp's missing
            catch.df$lifeStageID[ is.na(catch.df$lifeStageID) ] <- 251
            
            #   ---- Convert to CAMP lifestages
            catch.df$lifeStageID <- f.to.camp.lifestages( catch.df$lifeStageID, rst.life.stages )
            
            #   ---- Loop over lifestages
            for( ls in CAMP.life.stageIDs ){
    
                #   ---- Subset to just one life stage
                catch.df.ls <- catch.df[ catch.df$lifeStageID == ls, ]

                species.name <- attr(catch.df, "species.name")
                ls.name <- CAMP.life.stages[CAMP.life.stageIDs == ls]
                        
                cat(paste("Lifestage=", ls.name, "; Run=", run.name, "; nrow(catch.df.ls)=", nrow(catch.df.ls), "\n"))
                tmp.mess <- paste("Lifestage=", ls, "-", ls.name )
                setWinProgressBar( progbar, getWinProgressBar(progbar)+barinc, label=tmp.mess )                
                                    
                #   ---- Replicate some of the computations made in F.get.catch.  Imput missing catches.
                if( nrow(catch.df.ls) > 0 ){

                    catch.fl <- F.summarize.fish.visit( catch.df.ls )
                    setWinProgressBar( progbar, getWinProgressBar(progbar)+barinc )

                    catch.df.ls <- merge( visit, catch.fl, by="trapVisitID", all.x=T )
                    setWinProgressBar( progbar, getWinProgressBar(progbar)+barinc )

                    catch.df.ls$n.tot[ is.na(catch.df.ls$n.tot) ] <- 0
                    catch.df.ls$n.hatchery[ is.na(catch.df.ls$n.hatchery) ] <- 0
                    catch.df.ls$n.wild[ is.na(catch.df.ls$n.wild) ] <- 0
                    catch.df.ls$n.morts[ is.na(catch.df.ls$n.morts) ] <- 0

                    
                    cat("\nSubsites visited during time period:\n")
                    print(attr(visit, "subsites"))
                    cat(paste("Subsites that caught ", species.name, ", run=", run.name, ", lifestage=", ls.name, "\n", sep=""))
                    print(attr(catch.df, "subsites"))
                                        
                    attr(catch.df.ls, "run.season") <- run.season
                    attr(catch.df.ls, "site.abbr") <- site.abbr 
                    attr(catch.df.ls, "subsites") <- attr(visit, "subsites")
                    attr(catch.df.ls, "site.name") <- attr(visit, "site.name")
                    attr(catch.df.ls, "species.name") <- attr(catch.df, "species.name")
                    attr(catch.df.ls, "run.name") <- paste(ls.name, run.name)
                
                    #   ---- Compute passage
                    out.fn.root <- paste0(output.file, ls.name, run.name ) 
                    setWinProgressBar( progbar, getWinProgressBar(progbar)+barinc )


                    pass <- F.est.passage( catch.df.ls, release.df, "year", out.fn.root, ci )

                    setWinProgressBar( progbar, getWinProgressBar(progbar)+barinc )
                    out.fn.roots <- c(out.fn.roots, attr(pass, "out.fn.list"))

                    #print(pass)
                    
                    #   ---- Save
                    ans[ which(ls==CAMP.life.stageIDs), which(run == runs$runID) ] <- pass$passage
                    lci[ which(ls==CAMP.life.stageIDs), which(run == runs$runID) ] <- pass$lower.95
                    uci[ which(ls==CAMP.life.stageIDs), which(run == runs$runID) ] <- pass$upper.95
                    print(ans)
                    setWinProgressBar( progbar, getWinProgressBar(progbar)+barinc )
                    
                } 
                
            }
        }
        
        close(progbar)
    }
    
#    ans <<- ans
#    ans <- get("ans")
    
    cat("Final answer:\n")
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
        cat(paste("Site=,", site.name, "\n", sep=""))
        cat(paste("Site abbreviation=,", site.abbr, "\n", sep=""))
        cat(paste("Site ID=,", site, "\n", sep=""))
        cat(paste("Species=,", sp.commonName, "\n", sep=""))
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

