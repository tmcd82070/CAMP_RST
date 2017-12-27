#' @export 
#'
#' @title F.passageWithLifeStageAssign
#'
#' @description Estimate annual passage / production estimates between days
#' specified. 
#' 
#' @param site The identification number of the site for which estimates are 
#'   required.
#' @param taxon The species identifier indicating the type of fish of interest. 
#'   This is always \code{161980}; i.e., Chinook Salmon.
#' @param min.date The start date for data to include. This is a text string in 
#'   the format \code{\%Y-\%m-\%d}, or \code{YYYY-MM-DD}.
#' @param max.date The end date for data to include.  Same format as 
#'   \code{min.date}.
#' @param output.file The name of the file prefix under which output is to be 
#'   saved.  Set to NA to plot to the Plot window.
#' @param ci A logical indicating if 95\% bootstrapped confidence intervals 
#'   should be estimated along with passage estimates.
#' @param autols Default of \code{FALSE} leads to no assigning of no analytical
#'   life stage. If \code{TRUE}, assignment of analytical life stage is done. 
#'   See Details.
#' @param nls Number of life stage groups to estimate. Ignored if 
#'   \code{autols=FALSE}.  See Details.
#' @param weightuse A logical indicating if variable \code{weight} should be used for
#'   the analytical life stage assignment;  the default is \code{NULL}. Ignored
#'   if \code{autols=FALSE}.  See Details.
#'   
#' @details The date range difference specified via \code{max.date} and
#'   \code{min.date} must be less than or equal to 366 days.  Note that this
#'   cutoff allows for leap-year annual estimates.
#'   
#'   Passage requires the division of a non-zero catch by a non-zero efficiency.
#'   A non-zero catch numerator ensures that passage estimates are greater than
#'   zero, while a non-zero efficiency denominator ensures that passage 
#'   estimates are less than infinity.  The program exits if either of these 
#'   conditions are violated.
#'   
#'   Assuming validity, catch data are summarized via unique combinations of 
#'   \code{trapVisitID}, \code{FinalRun}, and \code{lifeStage}.  After 
#'   summarizing, all catch data are collapsed so as to have one line per 
#'   combination of these variables.  The use of a plus-count algorithm divvies
#'   up non-randomly selected fish into appropriate \code{FinalRun} and 
#'   \code{lifeStage} categories.  In this way, all fish are ultimately utilized
#'   in all pasasge estimates by life stage and run.
#'   
#'   Observed fish are also corrected for half-cone operations, in which the
#'   intake of fish is reduced to half of a cone's aperture being covered, so as
#'   to reduce flow, and thus captured fish.  Generally, all fish captured 
#'   during half-cone operations are multiplied by the value of the global 
#'   \code{halfConeMulti} variable, which serves as a multiplier to correct for 
#'   fish missed.  Variable \code{halfConeMulti} is set to \code{2}.
#'   
#'   The unique combinations of run and life stage in the catch data dictate the
#'   reports generated. Thus, reports spanning different time periods and sites
#'   may report different run and life stage passage combinations.  
#'   
#'   In the case when \code{autols=TRUE}, the life stage is assigned 
#'   analytically.  Note that the only numbers of groups allowed by argument 
#'   (\code{nls}) is \code{two} or \code{three}. If \code{NULL}, the function 
#'   \code{assignLifeStage} determines the number of groups utilized.
#'   
#'   If \code{weightuse} is \code{FALSE}, any recorded weight measurements are
#'   not used in the analytical life stage assignment. If \code{weightuse} is
#'   \code{NULL}, the function \code{assignLifeStage} will determine if variable
#'   \code{weight} will be used or not.
#'   
#' @return A data frame containing daily passage estimates, corrected for times 
#'   not fishing, along with associated standard errors.
#'   
#'   A bar chart displaying relative proportions over both run and life stage.
#'   
#'   For each unique combination of run and life stage containing at least one 
#'   fish, a graph \code{png} of catch over time, a \code{csv} of daily catch 
#'   and passage.
#'   
#'   A graphical display of efficiency over time, for each trap, along with an 
#'   accompanying \code{csv} tabular datasheet.
#'   
#'   In the case when \code{autols=TRUE}, a plot (\code{pdf}) and a confusion 
#'   matrix (\code{csv}) comparing the analytical and morphometric life stage 
#'   assignments.
#'   
#' @author WEST Inc.
#'   
#' @seealso \code{assignLifeStage}, \code{assignlsCompare}, \code{F.run.passage}
#'
#' @examples
#' \dontrun{
#' #   ---- Estimate passage on the American by life stage and run.
#' site <- 57000
#' taxon <- 161980
#' min.date <- "2013-01-01"
#' max.date <- "2013-06-01"
#' output.file <- "American"
#' ci <- TRUE
#' nls <- NULL
#' weightuse <- NULL
#' autols <- FALSE
#' passageWithLifeStageAssign(site,taxon,min.date,max.date,output.file,ci,
#'   weightuse,autols)
#' }


F.passageWithLifeStageAssign.enh <- function(site, taxon, min.date, max.date, output.file, ci=TRUE,autols=FALSE,nls=NULL,weightuse=NULL){
  
#   site <- 12345
#   taxon <- 161980
#   min.date <- "2013-01-16"
#   max.date <- "2013-06-30"
#   output.file <- "here"
#   ci <- TRUE
#   nls <- NULL
#   weightuse <- NULL
#   autols <- FALSE
  
  #   ---- Obtain necessary variables from the global environment.  
  fishingGapMinutes <- get("fishingGapMinutes",envir=.GlobalEnv)
  passageRounder <- get("passageRounder",envir=.GlobalEnv)
  
  #   ---- Check that times are less than or equal to 366 days apart.
  strt.dt <- as.POSIXct( min.date, format="%Y-%m-%d" )
  end.dt <- as.POSIXct( max.date, format="%Y-%m-%d" )
  run.season <- data.frame( start=strt.dt, end=end.dt )
  dt.len <- difftime(end.dt, strt.dt, units="days")
  if( dt.len > 366 )  stop("Cannot specify more than 365 days in F.passage. Check min.date and max.date.")
  
  #   ---- Identify the type of passage report we're doing.
  assign("passReport","lifeStage",envir=.GlobalEnv)
  passReport <- get("passReport",envir=.GlobalEnv)
  
  #   ---- Start a progress bar.
  progbar <<- winProgressBar( "Production estimate for lifestage + runs", label="Fetching efficiency data" )
  
  #   ---- Fetch efficiency data.
  release.df <- F.get.release.data( site, taxon, min.date, max.date  )
  
  #   ---- Fetch all efficiency data over all time.  
  min.date2 <<- "1990-01-01"
  max.date2 <<- "2017-05-22"
  release.df.enh <<- F.get.release.data( site, taxon, min.date2, max.date2)
  
  #   ---- Check if we can estimate an efficiency (denominator).  
  if( nrow(release.df) == 0 ){
    stop( paste( "No efficiency trials between", min.date, "and", max.date, ". Check dates."))
  }
  
  #   ---- Start an indicator bar.  
  setWinProgressBar( progbar, 0.1 , label=paste0("Fetching catch data, while using a ",round(fishingGapMinutes / 24 / 60,2),"-day fishing gap.") )
  
  #   ---- Fetch the catch and visit data.  
  tmp.df   <- F.get.catch.data( site, taxon, min.date, max.date,output.file,autols=autols,nls=nls,weightuse=weightuse,reclassifyFL=FALSE)
  
  #   ---- All positive catches, all FinalRun and lifeStages, inflated for plus counts.  Zero catches (visits without catch) are NOT here.
  catch.df <- tmp.df$catch   
  
  #   ---- Unique trap visits.  This will be used in a merge to get zeros later.
  visit.df <- tmp.df$visit  
  
  #   ---- Save for below.  Several dfs get named catch.df, so need to call this something else.
  catch.dfX <- catch.df      
  
  #   ---- Check if we can estimate catch (numerator).  
  if( nrow(catch.df) == 0 ){
    stop( paste( "No catch records between", min.date, "and", max.date, ". Check dates and taxon."))
  }
  
  #   ---- Summarize catch data by trapVisitID X FinalRun X lifeStage. 
  #   ---- Upon return, catch.df has one line per combination of these variables.
  #   ---- We run this separately to get different n.tot, and other statistics. 
  catch.df0 <- F.summarize.fish.visit( catch.df, 'unassigned' )
  catch.df1 <- F.summarize.fish.visit( catch.df, 'inflated' )
  catch.df2 <- F.summarize.fish.visit( catch.df, 'assigned')
  catch.df3 <- F.summarize.fish.visit( catch.df, 'halfConeAssignedCatch' )
  catch.df4 <- F.summarize.fish.visit( catch.df, 'halfConeUnassignedCatch' )
  catch.df5 <- F.summarize.fish.visit( catch.df, 'assignedCatch' )
  catch.df6 <- F.summarize.fish.visit( catch.df, 'unassignedCatch' )
  catch.df7 <- F.summarize.fish.visit( catch.df, 'modAssignedCatch' )
  catch.df8 <- F.summarize.fish.visit( catch.df, 'modUnassignedCatch' )

  #   ---- Compute the unique runs we need to do.
  runs <- unique(c(catch.df1$FinalRun,catch.df2$FinalRun))    
  runs <- runs[ !is.na(runs) ]
  cat("\nRuns found between", min.date, "and", max.date, ":\n")
  print(runs)
  
  #   ---- Compute the unique life stages we need to do.
  lstages <- unique(c(catch.df1$lifeStage,catch.df2$lifeStage))
  
  #   ---- Get rid of the unassigned.  Possible issue with half-cone 
  #   ---- operations and the plus-count algorithm (which is run twice).
  lstages <- lstages[lstages != 'Unassigned']
  
  #   ---- Probably don't need this, as doubtful lifeStage never missing here.
  lstages <- lstages[ !is.na(lstages) ]  
  cat("\nLife stages found between", min.date, "and", max.date, ":\n")
  print(lstages)
  
  #   ---- Print the number of non-fishing periods.  
  cat( paste("\nNumber of non-fishing intervals at all traps:", sum(visit.df$TrapStatus == "Not fishing"), "\n\n"))
  
  #   ---- Loop over runs.
  ans <- lci <- uci <- matrix(0, length(lstages), length(runs))
  dimnames(ans)<-list(lstages, runs)
  
  out.fn.roots <- NULL
  for( j in 1:length(runs) ){

    assign("run.name",runs[j],envir=.GlobalEnv)
    run.name <- get("run.name",envir=.GlobalEnv)
    
    #   ---- Assemble catches based on total, unassigned, assigned.
    assd <- catch.df2[catch.df2$Unassd != 'Unassigned' & catch.df2$FinalRun == run.name,c('trapVisitID','lifeStage','n.tot','mean.fl','sd.fl')]
    colnames(assd) <- c('trapVisitID','lifeStage','n.Orig','mean.fl.Orig','sd.fl.Orig')
    catch.dfA <- merge(catch.df1,assd,by=c('trapVisitID','lifeStage'),all.x=TRUE)
    unassd <- catch.df0[catch.df0$FinalRun == run.name,c('trapVisitID','lifeStage','n.tot')]
    colnames(unassd) <- c('trapVisitID','lifeStage','n.Unassd')
    catch.df <- merge(catch.dfA,unassd,by=c('trapVisitID','lifeStage'),all.x=TRUE)
    
    #   ---- Bring in halfcone counts.
    names(catch.df3)[names(catch.df3) == 'n.tot'] <- 'halfConeAssignedCatch'
    names(catch.df4)[names(catch.df4) == 'n.tot'] <- 'halfConeUnassignedCatch'
    names(catch.df5)[names(catch.df5) == 'n.tot'] <- 'assignedCatch'
    names(catch.df6)[names(catch.df6) == 'n.tot'] <- 'unassignedCatch'
    names(catch.df7)[names(catch.df7) == 'n.tot'] <- 'modAssignedCatch'
    names(catch.df8)[names(catch.df8) == 'n.tot'] <- 'modUnassignedCatch'
    
    catch.df <- merge(catch.df,catch.df3[,c('trapVisitID','lifeStage','FinalRun','halfConeAssignedCatch')],by=c('trapVisitID','lifeStage','FinalRun'),all.x=TRUE)
    catch.df <- merge(catch.df,catch.df4[,c('trapVisitID','lifeStage','FinalRun','halfConeUnassignedCatch')],by=c('trapVisitID','lifeStage','FinalRun'),all.x=TRUE)
    catch.df <- merge(catch.df,catch.df5[,c('trapVisitID','lifeStage','FinalRun','assignedCatch')],by=c('trapVisitID','lifeStage','FinalRun'),all.x=TRUE)
    catch.df <- merge(catch.df,catch.df6[,c('trapVisitID','lifeStage','FinalRun','unassignedCatch')],by=c('trapVisitID','lifeStage','FinalRun'),all.x=TRUE)
    catch.df <- merge(catch.df,catch.df7[,c('trapVisitID','lifeStage','FinalRun','modAssignedCatch')],by=c('trapVisitID','lifeStage','FinalRun'),all.x=TRUE)
    catch.df <- merge(catch.df,catch.df8[,c('trapVisitID','lifeStage','FinalRun','modUnassignedCatch')],by=c('trapVisitID','lifeStage','FinalRun'),all.x=TRUE)
    
    #   ---- Do some fish accounting.  
    #theSumsBefore <<- accounting(catch.df,"byRun")
    
    catch.df <- catch.df[order(catch.df$trapPositionID,catch.df$batchDate),]
    
    cat(paste(rep("*",80), collapse=""))
    tmp.mess <- paste("Processing ", run.name)
    cat(paste("\n", tmp.mess, "\n"))
    cat(paste(rep("*",80), collapse=""))
    cat("\n\n")
    
    #   ---- Update progress bar.
    progbar <- winProgressBar( tmp.mess, label="Lifestage X run processing" )
    barinc <- 1 / (length(lstages) * 6)
    assign( "progbar", progbar, pos=.GlobalEnv )
    
    #   ---- Create indicator of records to keep for this run.  
    #   ---- Likely don't need is.na clause.  FinalRun never missing here.
    indRun <- (catch.df$FinalRun == run.name ) & !is.na(catch.df$FinalRun)   
    
    #   ---- Loop over lifestages.  
    for( i in 1:length(lstages) ){
      
      ls <- lstages[i]
      
      #   ---- Subset to just one life stage and run.  
      #   ---- Likely don't need is.na clause.  LifeStage never missing here.
      indLS <- (catch.df$lifeStage == ls) & !is.na(catch.df$lifeStage) 
      
      cat(paste("Lifestage=", ls, "; Run=", run.name, "; num records=", sum(indRun & indLS), "\n"))
      tmp.mess <- paste("Lifestage=", ls )
      setWinProgressBar( progbar, getWinProgressBar(progbar)+barinc, label=tmp.mess )
      
      #   ---- If we caught this run and lifestage, compute passage estimate.
      if( any( indRun & indLS ) ){ 
        
        catch.df.ls <- catch.df[ indRun & indLS, c("trapVisitID", "FinalRun", "lifeStage", 'n.Orig','mean.fl.Orig','sd.fl.Orig',"n.tot", "mean.fl", "sd.fl","n.Unassd",'halfConeAssignedCatch','halfConeUnassignedCatch','assignedCatch','unassignedCatch','modAssignedCatch','modUnassignedCatch')]
        
        #   ---- Merge in the visits to get zeros.  
        catch.df.ls <- merge( visit.df, catch.df.ls, by="trapVisitID", all.x=T )
        setWinProgressBar( progbar, getWinProgressBar(progbar)+barinc )
        
        #   ---- Update the constant variables.  Missing n.tot when trap was fishing should be 0.
        catch.df.ls$FinalRun[ is.na(catch.df.ls$FinalRun) ] <- run.name
        catch.df.ls$lifeStage[ is.na(catch.df.ls$lifeStage) ] <- ls
        catch.df.ls$n.tot[ is.na(catch.df.ls$n.tot) & (catch.df.ls$TrapStatus == "Fishing") ] <- 0
        catch.df.ls$n.Orig[ is.na(catch.df.ls$n.Orig) & (catch.df.ls$TrapStatus == "Fishing") ] <- 0
        catch.df.ls$n.Unassd[ is.na(catch.df.ls$n.Unassd) & (catch.df.ls$TrapStatus == "Fishing") ] <- 0
        catch.df.ls$halfConeAssignedCatch[ is.na(catch.df.ls$halfConeAssignedCatch) & (catch.df.ls$TrapStatus == "Fishing") ] <- 0
        catch.df.ls$halfConeUnassignedCatch[ is.na(catch.df.ls$halfConeUnassignedCatch) & (catch.df.ls$TrapStatus == "Fishing") ] <- 0
        catch.df.ls$assignedCatch[ is.na(catch.df.ls$assignedCatch) & (catch.df.ls$TrapStatus == "Fishing") ] <- 0
        catch.df.ls$unassignedCatch[ is.na(catch.df.ls$unassignedCatch) & (catch.df.ls$TrapStatus == "Fishing") ] <- 0
        catch.df.ls$modAssignedCatch[ is.na(catch.df.ls$modAssignedCatch) & (catch.df.ls$TrapStatus == "Fishing") ] <- 0
        catch.df.ls$modUnassignedCatch[ is.na(catch.df.ls$modUnassignedCatch) & (catch.df.ls$TrapStatus == "Fishing") ] <- 0
        

        #   ---- Add back in the missing trapVisitID rows.  These identify the gaps in fishing
        #catch.df.ls <- rbind( catch.df.ls, catch.df[ is.na(catch.df$trapVisitID), ] )

        #   ---- Update progress bar.
        out.fn.root <- paste0(output.file, ls, run.name )
        setWinProgressBar( progbar, getWinProgressBar(progbar)+barinc )
        
        #   ---- Deal with traps with all zero fish.  Have to deal with this here 
        #   ---- since we now get rid of antecedent and precedent zeros.  
#         theSums <- tapply(catch.df.ls[!is.na(catch.df.ls$n.Orig),]$n.Orig,list(catch.df.ls[!is.na(catch.df.ls$n.Orig),]$trapPositionID),FUN=sum)
#         theZeros <- names(theSums[theSums == 0])
#         catch.df.ls <- catch.df.ls[!(catch.df.ls$trapPositionID %in% theZeros),]

        #   ---- Set these attributes so they can be passed along.
        attr(catch.df.ls,"min.date") <- min.date
        attr(catch.df.ls,"max.date") <- max.date
        
        #   ---- Compute passage
        if(nrow(catch.df.ls) > 0){#} & sum(as.numeric(theSums)) > 0){
          pass <- F.est.passage.enh( catch.df.ls, release.df, "year", out.fn.root, ci )
        } else {
          
          #   ---- We need something for the matrix down below if ALL traps have zero fish in data frame theZeros above.
          pass <- data.frame(passage=0,lower.95=0,upper.95=0)  
        }

        #   ---- Update progress bar.
        setWinProgressBar( progbar, getWinProgressBar(progbar)+barinc )
        out.fn.roots <- c(out.fn.roots, attr(pass, "out.fn.list"))
        
        #   ---- Save.
        ans[ i, j ] <- signif(round(pass$passage,0),passageRounder)
        lci[ i, j ] <- signif(round(pass$lower.95,0),passageRounder)
        uci[ i, j ] <- signif(round(pass$upper.95,0),passageRounder)
        setWinProgressBar( progbar, getWinProgressBar(progbar)+barinc )
      }
    }
    close(progbar)
  }
  
  cat("Final lifeStage X run estimates:\n")
  print(ans)
  
  #   ---- compute percentages of each life stage
  ans.pct <- matrix( colSums( ans ), byrow=T, ncol=ncol(ans), nrow=nrow(ans))
  ans.pct <- ans / ans.pct
  ans.pct[ is.na(ans.pct) ] <- NA

  #   ---- Write out the table
  df <- data.frame( dimnames(ans)[[1]], ans.pct[,1], ans[,1], lci[,1], uci[,1], stringsAsFactors=F )
  if( ncol(ans) > 1 ){
    
    #   ---- We have more than one run.
    for( j in 2:ncol(ans) ){
      df <- cbind( df, data.frame( ans.pct[,j], ans[,j], lci[,j], uci[,j], stringsAsFactors=F ))
    }
  }
  names(df) <- c("LifeStage", paste( rep(runs, each=4), rep( c(".propOfPassage",".passage",".lower95pctCI", ".upper95pctCI"), length(runs)), sep=""))


  #   ---- Append totals to bottom.
  tots <- data.frame( "Total", matrix( colSums(df[,-1]), nrow=1), stringsAsFactors=F)
  names(tots) <- names(df)
  tots[,grep("lower.95", names(tots),fixed=T)] <- NA
  tots[,grep("upper.95", names(tots),fixed=T)] <- NA
  df <- rbind( df, Total=tots )

  #   ---- Output csv report.  
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

    ls.pass.df <- df

    #   ---- Produce pie or bar charts.
    rownames(df) <- df$LifeStage
    fl <- F.plot.lifestages( df, output.file, plot.pies=F )
    if( fl == "ZEROS" ){
      cat("FAILURE - F.passageWithLifeStageAssign - ALL ZEROS\nCheck dates and finalRunId's\n")
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
  }

  #   ---- Write out message.
  cat("SUCCESS - F.passageWithLifeStageAssign\n\n")
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