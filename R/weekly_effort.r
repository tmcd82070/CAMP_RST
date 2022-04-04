#' @export
#'   
#' @title F.weekly.effort - Plot fishing and non-fishing effort.
#'   
#' @description Compute weekly "Fishing" and 
#'   "Not fishing" hours, then plot.
#'   
#' @inheritParams F.size.by.date
#' 
#' @return A \code{png} graphical display and \code{csv} of underlying data, for
#'   each unique trap with fishing data between the specified \code{min.date}
#'   and \code{max.date}. An additional \code{png} and \code{csv} is output 
#'   summarizing all traps together.
#'   
#' @details Dates provided by variables \code{min.date} and \code{max.date} 
#'   cannot span more than 366 days.  Additionally, the code is only set up to 
#'   calculate fishing with a provided \code{taxon} of \code{161980}, i.e., 
#'   Chinook Salmon.
#'   
#'   Function \code{F.weekly.effort} utilizes the Build Report Criteria Release 
#'   and Sample Period query series to first identify appropriate fishing 
#'   instances between \code{min.date} and \code{max.date}.  Series Weekly
#'   Effort then massages the fishing time data into the format necessary for 
#'   output.  See section Structured Query Language (SQL) Queries in
#'   \code{F.run.sqlFile} for more information on query series.
#'   
#'   Allocation of time spent fishing is to one of three possibilities. 
#'   The first tabulates time during which a trap was not fishing; i.e., the
#'   trap was out of the water.  The second and third summarize time for which
#'   the trap was deployed but for which fishing was unsuccessful or successful,
#'   respectively.  Variable \code{includeCatchID} determines whether or not a 
#'   fishing instance (identifiable via a unique \code{trapVisitID}) was 
#'   successful or not.
#'   
#'   All vertical bars in the graph cover seven calendar 
#'   days, or 168 hours, with the exception of Julian week 53, which covers 
#'   December 30th to December 31st in leap years, and December 31st alone in 
#'   non-leap years. In this case, the "week" spans only 2 or 1 day(s), 
#'   respectively.  Gray coloring is used to flesh out the remaining 5 or 6 
#'   days, respectively, for this special week.  In this way, all weeks cover a 
#'   7-day period within the bar-chart graphical output.
#'   
#'   The output graph and \code{csv}s  for summary fishing over
#'   all unique traps between the provided \code{min.date} and \code{max.date}
#'   calculate total minutes on a per-day basis.  Thus, if three traps fished on
#'   one day, but only two the next, the program utilizes \eqn{1440*3=4320}
#'   total minutes for the first day, but only \eqn{1440*2=2880} total minutes
#'   for the second. Note there are \eqn{1440 = 24*60} minutes in one day.
#'   
#' @seealso \code{F.run.sqlFile}
#'   
#' @inheritSection F.size.by.date Author
#'   
#' @examples
#' \dontrun{
#' #   ---- Estimate the weekly effort on the American River for all 
#' #   ---- inclusive traps, from Jan. 16, 2013 through June 8, 2013.  
#' site <- 57000
#' taxon <- 161980
#' min.date <- "2016-01-16"
#' max.date <- "2013-06-08"
#' output.file <- "American River"
#' F.weekly.effort(site,taxon,min.date,max.date,output.file)
#' }
F.weekly.effort <- function( site, taxon, min.date, max.date, output.file ){
  
  #   ---- A constant used in this routine
  minPerDay <- 24*60
  
  #   ---- Make sure we have all temp tables.
  tableChecker()
  
  #   ---- Check that times are less than 1 year apart.
  strt.dt <- as.POSIXct( min.date, format="%Y-%m-%d" )
  end.dt <- as.POSIXct( max.date, format="%Y-%m-%d" )
  dt.len <- difftime(end.dt, strt.dt, units="days")
  dt.len.min <- difftime(end.dt, strt.dt, units="mins")
  if( dt.len > 366 )  stop("Cannot specify more than 365 days in F.passage. Check min.date and max.date.")
  
  #   ---- Check that taxon is Chinook salmon.  
  if( taxon != 161980 ) stop("Cannot specify any species other than Chinook salmon, code 161980.")
  
  #   ---- Start a progress bar.
  progbar <- winProgressBar( "Weekly effort estimate", label=paste0("Reading data and accounting for the ",dt.len.min[1]," minutes your time range specified." ), width=500 )
  
  # Utilize this construction to avoid NOTEs about assigning variables to the 
  # .GlobalEnv when running devtools::check().  
  pos <- 1
  envir <- as.environment(pos)
  assign("progbar",progbar,envir=envir)
  
  nvisits <- F.buildReportCriteria( site, min.date, max.date )
  
  if( nvisits == 0 ){
    warning("Your criteria returned no trapVisit table records.")
    return()
  }
  
  #   ---- Open ODBC channel.
  db <- get( "db.file", envir=.GlobalEnv ) 
  ch <- RODBC::odbcConnectAccess(db)
  
  #   ---- Develop the hours fished and TempSamplingSummary table.
  F.run.sqlFile( ch, "QrySamplePeriod.sql", R.TAXON=taxon ) 
  
  #   ---- Generate the daily allocation of minutes, so they sum to 1440.
  F.run.sqlFile( ch, "QryWeeklyEffort.sql" ) 
  
  #   ---- Fetch the result.
  newDF <- RODBC::sqlFetch( ch, "TempEffortSummary_b" )
  F.sql.error.check(newDF)
  
  #   ---- Set time zone here so it propagates through all calculations
  newDF <- newDF %>% 
    dplyr::mutate(EffortDate = as.POSIXct(format(EffortDate), tz = get("time.zone", envir = .GlobalEnv)))
    
  #   ---- Obtain Julian dates so days can be mapped to specialized Julian weeks. 
  Site <- RODBC::sqlFetch( ch, "Site")
  close(ch) 
  
  setWinProgressBar( get("progbar",envir=.GlobalEnv), getWinProgressBar(get("progbar",envir=.GlobalEnv))*.7 + .3 , label="Formatting results." )
  
  #   ---- Given data, compile per-trap, per-day summaries of the three different
  #   ---- types of fishing.  
  eff.df2 <- newDF %>% 
    dplyr::select(Position, Year, JWeek, EffortDate, FishingEffort, Minutes) %>% 
    dplyr::group_by(Position, Year, JWeek, EffortDate, FishingEffort) %>% 
    dplyr::summarise(Minutes = sum(Minutes), .groups = 'drop') %>% 
    dplyr::mutate(EffortID = dplyr::case_when(
      FishingEffort == "Excluded" ~ 2,
      FishingEffort == "Included" ~ 1,
      FishingEffort == "Not fishing" ~ 3,
      TRUE ~ NA_real_)) %>% 
    dplyr::select(-FishingEffort) %>% 
    dplyr::arrange(Position, EffortDate, EffortID) %>% 
    dplyr::rename(Date = EffortDate)
  
  # Recompute JWeek ----
  # The JWeek column returned from Access and computed by the sequence of 
  # queries is wrong.  I think it is a simple (mod 7) calculation of 1:365, 
  # which makes Jan 1 the first day of the first week always, and week 
  # breaks are somewhere in the middle of normal weeks. Here, I recalculate
  # JWeek using R's built-in ISO 8601 compliant week of the year computation, 
  # with the week starting on Monday (the UK convention).  
  # If the week (starting on Monday) containing 1 January has four or 
  # more days in the new year, then it is considered week 1. Otherwise, it 
  # is the last week of the previous year, and the next week is week 1. 
  # Make sure to use same calculation in 'theDates' below, i.e., use %V
  eff.df2 <- eff.df2 %>% 
    dplyr::mutate(JWeek = as.numeric(format(Date, "%V")), 
                  Year = as.numeric(format(Date, "%Y")))  # just make sure Year is correct too
      
  #   Set up data frame of dates and time possibilities.  Get min and max over all ----
  #   traps so all resulting data frames are the same size.
  minDate <- min( eff.df2$Date )
  maxDate <- max( eff.df2$Date )
   
  # Make data frame with JWeek labels. This will be merged in later. ----
  theDates <- data.frame(Date=seq(minDate,maxDate,by="days")) %>%
    dplyr::mutate(JWeek = as.numeric(format(Date, "%V")),
                  mon = format(Date, "%b"),
                  day = format(Date, "%d")) %>%
    dplyr::group_by(JWeek) %>%
    dplyr::summarise(julianWeekLabel = paste(dplyr::first(mon), dplyr::first(day),
                                             "-",
                                             dplyr::last(mon), dplyr::last(day)),
                     Year = as.numeric(format(min(Date), "%Y")),
                     .groups = "keep")
  
  # Make sure all weeks appear in effort dataframe ----
  eff.df2 <- eff.df2 %>% 
    dplyr::group_by(Position) %>% 
    dplyr::summarise(JWeek = c(JWeek, theDates$JWeek), 
                     Minutes = c(Minutes, rep(0, nrow(theDates))), 
                     EffortID = c(EffortID, rep(3, nrow(theDates))))
  
  #   ---- Get site label.
  siteLabel <- Site %>%
    dplyr::filter(siteID == site) %>%
    dplyr::pull(siteName)

  eff.df3 <- eff.df2 %>% 
    dplyr::group_by(Position, JWeek, EffortID) %>% 
    dplyr::summarise(Minutes = sum(Minutes), 
                     Effort = Minutes / 60) 
  
  eff.df3 <- eff.df3 %>% 
    tidyr::pivot_wider(id_cols = c("Position", "EffortID"), 
                       names_from = JWeek, 
                       names_prefix = "Week.", 
                       values_from = Effort, 
                       values_fill = 0) %>% 
    dplyr::arrange(Position, EffortID)
    
    
    # Increase NoFishing bars so each bar sums to hours in a week (168) ----
    # DF MUST be sorted by EffortID (1,2,3)
    hrsPerWeek <- minPerDay * 7 / 60
    bumpNoFishing <- function(x, totHrs){
      x[3] <- totHrs - sum(x[1:2])
      if( x[3] < 0 ){
        # if sum(x) > 169 (I don't see how, but...), scale 
        # the first to elements to sum to 168 exactly
        x[3] <- 0
        x[1:2] <- totHrs * (x[1:2] / sum(x[1:2]))
      }
      x
    }
    eff.df3 <- eff.df3 %>% 
      dplyr::group_by(Position) %>% 
      dplyr::mutate(dplyr::across(dplyr::starts_with("Week"), bumpNoFishing, totHrs = hrsPerWeek))

    # Compute "All Trap" fishing hours ----
    nTraps <- length(unique(eff.df2$Position))
    if( nTraps > 1){
      eff.df3 <- eff.df3 %>% 
        dplyr::group_by(EffortID) %>% 
        dplyr::summarise(Position = "All Traps", 
                         across(starts_with("Week"), sum)) %>% 
        dplyr::bind_rows(eff.df3)
    }
  

    # Plot Effort ----
    theCols <- c("blue","red","white")
    theLegd <- c("Fishing successful","Fishing unsuccessful","Trap not fished")
    
    plotEff <- function(.x, .y, 
                        jWeekLabs,
                        output.file = output.file, 
                        cols = theCols, 
                        legnd = theLegd, 
                        sitelab = siteLabel, 
                        minDate = minDate, 
                        maxDate = maxDate, 
                        nTraps){

      .x <- .x %>% 
        dplyr::select(dplyr::starts_with("Week")) %>% 
        as.matrix()

      out.fn <- gsub(" ", "", paste0("_EffortPlot_",.y$Position[1],".png"))
      out.fn <- paste(output.file, out.fn, sep="")
      tryCatch({png(filename=out.fn,width=7,height=7,units="in",res=1000)}, 
               error=function(x){png(filename=out.fn)})

      print(paste0("Graph: ", siteLabel, ", ", .y$Position ))

      #   ---- Layout plotting area. 
      z <- layout( matrix(c(1,2,3), ncol=1), heights=c(0.1,0.45,0.45), widths=1)
      
      par(mar = c(0.2,0.2,0.2,0.2)); 
      plot(1,1,type = "n",frame.plot = FALSE,axes = FALSE)
      u <- par("usr")
      text(u[4]-0.07*(u[4]-u[3]),u[1] +   3*(u[2]-u[1])/5,adj=c(1,NA),siteLabel,cex=2.25)
      text(u[4]-0.07*(u[4]-u[3]),u[1] + 1.5*(u[2]-u[1])/5,adj=c(1,NA),
           paste0("Weekly Effort from ",minDate,' through ',maxDate,": ",.y$Position[1]),cex=1.0)
      
      par(mar=c(1.0,6,0,2))
      
      #   ---- Finds where to cut bars and make top plot matrix and bottom plot matrix.
      mid.time <- round((1+ncol(.x))/2)
      eff1.bars <- .x[,1:mid.time]
      eff2.bars <- .x[,(mid.time+1):ncol(.x)]
      
      #   ---- Set plot area. 
      wk1.Labels <- jWeekLabs[1:mid.time]
      wk2.Labels <- jWeekLabs[(mid.time+1):ncol(.x)]
      
      if( grepl("All", .y$Position) ){
        yText <- paste0("Hours, summed over ", nTraps, " traps")
        yTickInterval <- 24 * nTraps
        yMin <- -(60 * nTraps)
      } else {
        yText <- "Hours per week"
        yTickInterval <- 24
        yMin <- -60
      }
      
      yMax <- max(colSums(eff1.bars)) # is same for eff2.bars
      
      #   ---- Actually make the top plot. 
      barplot(eff1.bars, 
              space=0, 
              col=theCols, 
              legend.text=theLegd, 
              args.legend=list(x="top", horiz=T, bty="n"), 
              ylab="", 
              xlab="", 
              ylim=c(yMin,yMax*1.15), 
              yaxt = "n", 
              xaxt = "n")
      
      #   ---- Format top plot. 
      mtext( side=2, text=yText, line=3, cex=0.7, at=c(NULL,yMax/2) )
      axis(2, at=seq(0, yMax, by=yTickInterval), cex.axis=0.8)
      text(x=1:mid.time - 0.5, y = yMin/2, labels=wk1.Labels, cex=0.7, srt = 90)
      
      #   ---- Actually make the bottom plot. 
      par(mar=c(1.0,6,0,2))
      barplot(eff2.bars, 
              space=0, 
              col=theCols, 
              legend.text=theLegd, 
              args.legend=list(x="top", horiz=T, bty="n"), 
              ylab="", 
              xlab="", 
              ylim=c(yMin,yMax*1.15),
              yaxt = "n", 
              xaxt = "n")
      
      #   ---- Format the bottom plot.  
      mtext( side=2, text=yText, line=3, cex=0.7, at=c(NULL,yMax/2) )    
      axis(2, at=seq(0, yMax, by=yTickInterval), cex.axis=0.8)
      text(x=1:mid.time - 0.5, y = yMin/2, labels=wk2.Labels, cex=0.7, srt = 90)
      
      dev.off()
      
      out.fn
            
    }

    out.graphs <- eff.df3 %>% 
      dplyr::group_by(Position) %>% 
      dplyr::group_map(.f = plotEff, 
                        jWeekLabs = theDates$julianWeekLabel,
                        output.file = output.file, 
                        cols = theCols, 
                        legnd = theLegd, 
                        sitelab = siteLabel, 
                        minDate = minDate, 
                        maxDate = maxDate, 
                        nTraps = nTraps
                        )
    out.graphs <- do.call(c, out.graphs)

    # Computations for the  CSVs ----
    # CSV has different format than that required by graph
    
    eff.df3 <- eff.df2 %>% 
      dplyr::group_by(Position, JWeek, EffortID) %>% 
      dplyr::summarise(Minutes = sum(Minutes)) 
    
    eff.df3 <- eff.df2 %>% 
      dplyr::group_by(JWeek, EffortID) %>% 
      dplyr::summarise(Minutes = sum(Minutes), 
                       Position = "All Traps") %>% 
      dplyr::bind_rows(eff.df3)
    
    eff.df3 <- eff.df3 %>% 
      tidyr::pivot_wider(id_cols = c("Position", "JWeek"), 
                         names_from = EffortID, 
                         names_prefix = "Minutes.", 
                         names_sort = TRUE,
                         values_from = Minutes, 
                         values_fill = 0) %>% 
      dplyr::arrange(Position, JWeek)
    
    eff.df3 <- eff.df3 %>% 
      dplyr::left_join(theDates, by = "JWeek") %>% 
      dplyr::mutate(Total = Minutes.1 + Minutes.2 + Minutes.3,
                    Diff = ifelse(Position == "All Traps", nTraps*hrsPerWeek*60 - Total, hrsPerWeek*60 - Total)) %>% 
      dplyr::rename(FishSuccess = Minutes.1, 
                    FishUnsuccess = Minutes.2, 
                    TrapNotFished = Minutes.3, 
                    UnaccountedForMins = Diff,
                    WeekOfYear = JWeek,
                    Dates = julianWeekLabel) %>% 
      dplyr::select(Position, Year, WeekOfYear, Dates, 
         FishSuccess, FishUnsuccess,TrapNotFished,
         Total, UnaccountedForMins) 
    
    # Write out the CSVs ----
    
    writeEffort <- function(.x, .y, 
                            out.file = out.file, 
                            siteLabel, 
                            minDate, 
                            maxDate){
      
      #   ---- Output csv of minutes for the ith trap.
      out.fn <- gsub(" ", "", paste0("_EffortSummaryTable_",.y$Position[1],".csv"))
      out.week.table <- paste(output.file, out.fn, sep="")
      
      print(paste0("CSV: ", siteLabel, ", ", .y$Position ))
      
      rs <- paste0(minDate,' through ',maxDate)
      
      sink(out.week.table)
      cat(paste("Site=,", siteLabel, "\n", sep=""))
      cat(paste("Site ID=,", .y$Position[1], "\n", sep=""))
      cat(paste("Species ID=,", 161980, "\n", sep=""))
      cat(paste("Summarized by=, week\n", sep=""))
      cat(paste("Dates included=,", rs, "\n", sep=""))
      cat(paste("Note:, All time units in minutes.\n"))  
      
      cat("\n")
      cat("\n")
      sink()
      suppressWarnings(write.table( .x, file=out.week.table, sep=",", 
                                    append=TRUE, row.names=FALSE, col.names=TRUE))
      
      out.week.table
    }
    
  out.csv <- eff.df3 %>% 
    dplyr::group_by(Position) %>% 
    dplyr::group_map( .f = writeEffort, 
                      out.file = output.file, 
                      siteLabel = siteLabel, 
                      minDate = minDate,
                      maxDate = maxDate)
  out.csv <- do.call(c, out.csv)
  results <- c(out.graphs, out.csv)

  # Clean up and send message back ----
  
  tableDeleter()
  
  cat("SUCCESS - F.weekly.effort\n\n")
  cat(paste("Working directory:", getwd(), "\n"))
  cat(paste("R data frames saved in file:", "<No RData saved>", "\n\n"))
  cat(paste("Number of files created in working directory =", length(results), "\n"))
  cat(paste(results, "\n", sep=""))
  cat("\n")
  
  setWinProgressBar( get("progbar",envir=.GlobalEnv), 1 , label="SUCCESS" )
  close(progbar)
  
  invisible(eff.df3)

}