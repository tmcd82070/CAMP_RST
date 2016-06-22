#' @export F.lifestage.passage.other
#' 
#' @title F.lifestage.passage.other
#' 
#' @description
#' 
#'    ANNUAL PRODUCTION ESTIMATES BY LIFE STAGE AND RUN ? TABULAR SUMMARY
#'    A table of passage estimates, with lifestages down the rows, and runs across the columns.
#' 
#'    Input:
#'    site = site ID of the place we want, trap locaton
#'    taxon = taxon number (from luTaxon) to retrieve
#' 
#' 
#' @param  site describe argument
#' @param  taxon describe argument
#' @param  min.date describe argument
#' @param  max.date describe argument
#' @param  output.file describe argument
#' @param  ci=TRUE  describe argument
#' 
#' @details other comments found in file
#' 
#' @return describe return value
#' 
#' @author WEST Inc.
#' 
#' @seealso \code{\link{related routine}}, \code{\link{related routine}}
#' 
#' @examples
#' \dontrun{
#' insert examples
#' 
#' }
F.lifestage.passage.other <- function( site, taxon, min.date, max.date, output.file, ci=TRUE ){
  #
  #   ANNUAL PRODUCTION ESTIMATES BY LIFE STAGE AND RUN ? TABULAR SUMMARY
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
  release.df <- F.get.release.data( site, taxon, min.date, max.date  )
  
  if( nrow(release.df) == 0 ){
    stop( paste( "No efficiency trials between", min.date, "and", max.date, ". Check dates."))
  }
  
  
  #   ---- Fetch the catch and visit data 
  tmp.df   <- F.get.catch.data( site, taxon, min.date, max.date  )
  
  catch.df <- tmp.df$catch   # All positive catches, all FinalRun and lifeStages, inflated for plus counts.  Zero catches (visits without catch) are NOT here.
  visit.df <- tmp.df$visit   # the unique trap visits.  This will be used in a merge to get 0's later
  
  #   Debugging
  #    tmp.catch0 <<- catch.df
  #    tmp.visit0 <<- visit.df
  #    print( table(catch.df$TrapStatus))
  
  if( nrow(catch.df) == 0 ){
    stop( paste( "No catch records between", min.date, "and", max.date, ". Check dates and taxon."))
  }
  
  #   ---- Summarize catch data by trapVisitID X FinalRun X lifeStage. Upon return, catch.df has one line per combination of these variables 
  
  #catch.df <- F.summarize.fish.visit( catch.df )       jason turns off 4/15/2015
  
  catch.df0 <- F.summarize.fish.visit( catch.df, 'unassigned' )   # jason - 5/20/2015 - we summarize over lifeStage, wrt to unassigned. 
  catch.df1 <- F.summarize.fish.visit( catch.df, 'inflated' )     # jason - 4/14/2015 - we summarize over lifeStage, w/o regard to unassigned.  this is what has always been done.
  catch.df2 <- F.summarize.fish.visit( catch.df, 'assigned')      # jason - 4/14/2015 - we summarize over assigned.  this is new, and necessary to break out by MEASURED, instead of CAUGHT.
  #                   - the only reason we do this again is to get a different n.tot.
  
  
  #   Debugging
  #    tmp.catch <<- catch.df
  #    print( table(catch.df$TrapStatus))
  #    cat("in lifestage_passage.r (hit return) ")
  #    readline()
  
#   #   ---- Compute the unique runs we need to do
#   runs <- unique(c(catch.df1$FinalRun,catch.df2$FinalRun))    # get all instances over the two df.  jason change 4/17/2015 5/21/2015: don't think we need to worry about catch.df0.
#   runs <- runs[ !is.na(runs) ]
#   cat("\nRuns found between", min.date, "and", max.date, ":\n")
#   print(runs)
  
  
  #   ---- Compute the unique life stages we need to do
  lstages <- unique(c(catch.df1$lifeStage,catch.df2$lifeStage))   # get all instances over the two df.  jason change 4/17/2015 5/21/2015: don't think we need to worry about catch.df0.
  lstages <- lstages[ !is.na(lstages) ]   #   Don't need this,  I am pretty sure lifeStage is never missing here.
  cat("\nLife stages found between", min.date, "and", max.date, ":\n")
  print(lstages)
  
  #   ---- Print the number of non-fishing periods
  cat( paste("\nNumber of non-fishing intervals at all traps:", sum(visit.df$TrapStatus == "Not fishing"), "\n\n"))
  
  #   ---- Extract the unique trap visits.  This will be used in merge to get 0's later
  #    ind <- !duplicated( catch.df$trapVisitID ) & !is.na(catch.df$trapVisitID)
  #    visit.df <- catch.df[ind, ]
  #    visit.df <- visit.df[, !(names(visit.df) %in% c("FinalRun", "lifeStage", "n.tot", "mean.fl", "sd.fl"))] 
  
  #   ********
  #   Loop over runs
  ans <- lci <- uci <- matrix(0, length(lstages), 1)
  dimnames(ans)<-list(lstages, 1)
  
  
  out.fn.roots <- NULL
#   for( j in 1:length(runs) ){
#     
    run.name <<- "All"#runs[j]
#     
    # jason puts together the catches based on total, unassigned, assigned.
    assd <- catch.df2[catch.df2$Unassd != 'Unassigned' ,c('trapVisitID','lifeStage','n.tot','mean.fl','sd.fl')]    
    colnames(assd) <- c('trapVisitID','lifeStage','n.Orig','mean.fl.Orig','sd.fl.Orig')
    catch.dfA <- merge(catch.df1,assd,by=c('trapVisitID','lifeStage'),all.x=TRUE)
    unassd <- catch.df0[catch.df0$FinalRun == run.name,c('trapVisitID','lifeStage','n.tot')]
    colnames(unassd) <- c('trapVisitID','lifeStage','n.Unassd')
    catch.df <- merge(catch.dfA,unassd,by=c('trapVisitID','lifeStage'),all.x=TRUE)
    
    catch.df <- catch.df[order(catch.df$trapPositionID,catch.df$batchDate),]
#     
#     cat(paste(rep("*",80), collapse=""))
#     tmp.mess <- paste("Processing ", run.name)
#     cat(paste("\n", tmp.mess, "\n"))
#     cat(paste(rep("*",80), collapse=""))
#     cat("\n\n")
#     
    progbar <- winProgressBar( tmp.mess, label="Lifestage X run processing" )
    barinc <- 1 / (length(lstages) * 6)
    assign( "progbar", progbar, pos=.GlobalEnv ) 
#     
    catch.df$FinalRun <- 'All'
    indRun <- (catch.df$FinalRun == run.name ) & !is.na(catch.df$FinalRun)   # Don't need is.na clause.  FinalRun is never missing here.
    
    #   ---- Loop over lifestages
    for( i in 1:length(lstages) ){
      
      ls <- lstages[i]
      
      #   ---- Subset to just one life stage and run
      indLS <- (catch.df$lifeStage == ls) & !is.na(catch.df$lifeStage) #  Don't need is.na clause.  I don't think lifeStage can be missing here.
      
#       cat(paste("Lifestage=", ls, "; Run=", run.name, "; num records=", sum(indRun & indLS), "\n"))
      tmp.mess <- paste("Lifestage=", ls )
      setWinProgressBar( progbar, getWinProgressBar(progbar)+barinc, label=tmp.mess )                
      
      #   ---- If we caught this run and lifestage, compute passage estimate. 
      if( any( indRun ) ){ #indRun & indLS ) ){
        
        catch.df.ls <- catch.df[ indRun & indLS, c("trapVisitID", "FinalRun", "lifeStage", 'n.Orig','mean.fl.Orig','sd.fl.Orig',"n.tot", "mean.fl", "sd.fl","n.Unassd")]
        
        #   ---- Merge in the visits to get zeros
        catch.df.ls <- merge( visit.df, catch.df.ls, by="trapVisitID", all.x=T )
        setWinProgressBar( progbar, getWinProgressBar(progbar)+barinc )
        
        #   ---- Update the constant variables.  Missing n.tot when trap was fishing should be 0.
        catch.df.ls$FinalRun[ is.na(catch.df.ls$FinalRun) ] <- 'All'
        catch.df.ls$lifeStage[ is.na(catch.df.ls$lifeStage) ] <- ls
        catch.df.ls$n.tot[ is.na(catch.df.ls$n.tot) & (catch.df.ls$TrapStatus == "Fishing") ] <- 0
        catch.df.ls$n.Orig[ is.na(catch.df.ls$n.Orig) & (catch.df.ls$TrapStatus == "Fishing") ] <- 0                
        catch.df.ls$n.Unassd[ is.na(catch.df.ls$n.Unassd) & (catch.df.ls$TrapStatus == "Fishing") ] <- 0                
        
        #   ---- Add back in the missing trapVisitID rows.  These identify the gaps in fishing
        #catch.df.ls <- rbind( catch.df.ls, catch.df[ is.na(catch.df$trapVisitID), ] )
        
        #   ---- Update progress bar
        out.fn.root <- paste0(output.file, ls, run.name ) 
        setWinProgressBar( progbar, getWinProgressBar(progbar)+barinc )
        
        #   Debugging
        #                tmp.c <<- catch.df.ls
        #                tmp.r <<- release.df
        
        #   Debugging
        #                print(dim(visit.df))
        #                print(dim(catch.df.ls))
        #                print( table( tmp.c$FinalRun, useNA="always" ))
        #                print( table( tmp.c$lifeStage, useNA="always" ))
        #                print( table( tmp.c$trapVisitID, useNA="always" ))
        #                cat("in lifestage_passage (hit return) ")
        #                readline()
        
        #   ---- Compute passage
        pass <- F.est.passage( catch.df.ls, release.df, "year", out.fn.root, ci )
        
        #   ---- Update progress bar
        setWinProgressBar( progbar, getWinProgressBar(progbar)+barinc )
        out.fn.roots <- c(out.fn.roots, attr(pass, "out.fn.list"))
        
        #print(pass)
        
        #   ---- Save
        ans[ i, 1 ] <- pass$passage
        lci[ i, 1 ] <- pass$lower.95
        uci[ i, 1 ] <- pass$upper.95
        setWinProgressBar( progbar, getWinProgressBar(progbar)+barinc )
        
      } 
      
    }
    
    
    close(progbar)
#   }
  
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
  if( ncol(ans) > 1 ){
    #   We have more than one run
    for( j in 2:ncol(ans) ){
      df <- cbind( df, data.frame( ans.pct[,j], ans[,j], lci[,j], uci[,j], stringsAsFactors=F ))
    }
  }
  names(df) <- c("LifeStage", paste( rep(runs, each=4), rep( c(".propOfPassage",".passage",".lower95pctCI", ".upper95pctCI"), length(runs)), sep=""))
  
  
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
    
    ls.pass.df <<- df
    
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
