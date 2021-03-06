#' @export F.get.indiv.visit.data
#' 
#' @title F.get.indiv.visit.data
#' 
#' @description Fetch the visit data for a particular site and run from an Access data base. The resulting data
#   set should have one line for each visit to a trap at the site.
#' 
#' @param site The identification number of the site for which estimates are 
#'   required.
#' @param run The text seasonal identifier.  This is a one of \code{"Spring"}, \code{"Fall"},
#' \code{"Late Fall"}, or \code{"Winter"}.
#' @param min.date The start date for data to include. This is a text string in 
#'   the format \code{\%Y-\%m-\%d}, or \code{YYYY-MM-DD}.  
#' @param max.date The end date for data to include.  Same format as 
#'   \code{min.date}.
#'   
#' @details This routine is very similar to function \code{F.get_catch_data},
#'   except that it does not average over subsites.  Instead, raw catches are
#'   returned.  Additionally, contrary to most functions, the run need not be
#'   specified.
#'   
#'   Here is the way to interpret visitTime and visitTime2
#'     "visit start" field (named "visitTime")
#'     for visitTypeID = 1 Date/time of arrival at trap
#'     for visitTypeID = 2 Date/time the trap was emptied
#'     for visitTypeID = 3 Date/time the trap was emptied
#'     for visitTypeID = 4 Date/time the trap was emptied
#'     for visitTypeID = 5 Date/time of arrival at trap
#'     for visitTypeID = 6 Date/time of arrival at trap
#' 
#' 
#'     "visit end" field (named "visitTime2")
#'     for visitTypeID = 1 Date/time the trap began fishing
#'     for visitTypeID = 2 Date/time the trap began fishing
#'     for visitTypeID = 3 Date/time the trap began fishing
#'     for visitTypeID = 4 Date/time of the end of the visit
#'     for visitTypeID = 5 Date/time of the end of the visit
#'     for visitTypeID = 6 Date/time of the end of the visit
#'   
#' @return
#' 
#' @author WEST Inc.
#'   
#' @seealso \code{\link{<related routine>}}, \code{F.sql.error.check}
#'   
#' @examples
#' \dontrun{
#' <insert examples>
#' 
#' }
F.get.indiv.visit.data <- function( site, run, min.date, max.date ){

  # site <- 57000
  # run <- "Fall"
  # min.date <- "2015-01-01"
  # max.date <- "2015-06-01"

  #   ---- Put the start and stop dates of the run into a list to filter visits.
  strt.dt <- as.POSIXct( min.date, format="%Y-%m-%d" )
  end.dt <- as.POSIXct( max.date, format="%Y-%m-%d" )
  run.season <- data.frame( start=strt.dt, end=end.dt )

  #   ---- Retrieve database file name and table names.
  tables <- get( "table.names", envir=.GlobalEnv )
  db <- get( "db.file", envir=.GlobalEnv ) 
  
  #   Open ODBC channel.
  ch <- odbcConnectAccess(db)
 
  #   ---- Retreive site info to get common names.  
  site.info <- sqlQuery( ch, paste("SELECT siteName, siteAbbreviation, streamName FROM", tables["sites"], 
          "WHERE (siteID =", site, ")" ))
          
  F.sql.error.check(site.info)
  
  site.name <- as.character(site.info$siteName)
  site.stream <- as.character(site.info$streamName)
  site.abbr <- as.character(site.info$siteAbbreviation)
  
  #   ---- Communicate to the Console and/or out file.  
  cat( paste(site.abbr, site.name, site.stream, sep=":") )
  cat("\n")
  cat(paste("Site = ", site, "\n"))



  #   ---- Identify the run name associate with the provided run number.  
  if( !is.na(run) ){
    runs <- sqlQuery(ch, paste( "SELECT run, runID FROM", tables["run.codes"] ))
    F.sql.error.check(runs)
    run.name <- as.character(runs$run[ runs$runID == run ])
  } else {
    run.name = "NA"
  }
  
  #   ---- Communicate to the Console and/or out file.  
  cat(paste("Run = ", run.name, "\n"))
  cat(paste("Start date = ", run.season$start, "\n"))
  cat(paste("End date = ", run.season$end, "\n"))



  #   ---- Obtain visits information.  

  #   ---- Make variable vectors one long string for SQL statement.
  visit.vars <- c("visitTime",
                  "visitTime2",
                  "trapVisitID",
                  "visitTypeID",
                  "fishProcessedID",
                  "trapPositionID",
                  "batchDate",
                  "trapFunctioningID", 
                  "includeCatchID",
                  "dataCollectedID")
  visit.vars.sql <- F.vec2sqlstr(tables["trap.visit"], visit.vars)
  visit.vars.sql <- paste( "Site.siteID,", visit.vars.sql )
    
  #  ---- Fetch additional helpful tables.  
  s.tab <- tables["sites"]
  ss.tab <- tables["subsites"]
  tv.tab <- tables["trap.visit"]



  #   ---- Note: query pulls visits from 7 days prior to start of run.  We do this to compute sampleStart 
  #   ---- and sampleEnd.  Must delete these records later.
  sql.visit <- paste( "SELECT ", visit.vars.sql, 
      " FROM (", s.tab, " INNER JOIN ", ss.tab, " ON ", s.tab, ".siteID", " = ", ss.tab, ".siteID", ")", 
      " INNER JOIN ", tv.tab, " ON ", ss.tab, ".subsiteID", "=", tv.tab, ".trapPositionID", 
      " WHERE (((", s.tab, ".siteID)=", site, ") ",
      "AND ((", tv.tab, ".visitTime)>=#", format(run.season$start-(7*24*60*60), "%m/%d/%Y"), "#) ",
      "AND ((", tv.tab, ".visitTime2)<=#", format(run.season$end, "%m/%d/%Y"), "#) ",
      "AND ((", tv.tab, ".visitTypeID)=2 Or (", tv.tab, ".visitTypeID)=4) ",
      "AND ((", tv.tab, ".includeCatchID)=1 Or (", tv.tab, ".includeCatchID)>250)); ",
      sep="")
    
  #   ---- Communicate with Console and/or out file, replacing text in SQL statement with other
  #   ---- text to facilitate reading by humans.  
  cat("\nVISITS where fish were processed SQL statement: \n")
  out.sql <- gsub( ",", "\n,", sql.visit, fixed=T)
  out.sql <- gsub( "FROM", "\nFROM", out.sql, fixed=T)
  out.sql <- gsub( ") ", ")\n", out.sql, fixed=T)
  cat(out.sql)
  cat("\n")

  #   ---- Pull the data from the query.  
  visit <- sqlQuery(ch, sql.visit)
  F.sql.error.check(visit)
  cat(paste("\nNumber of fish processing visits found:", nrow(visit), "\n"))

  #   ---- Parse the results from the query.  
  if( nrow(visit) == 0 ){
    
    #   ---- No visits found, so exit nicely.
    odbcClose(ch)
    attr(visit, "siteDescriptionID" ) <- site
    attr(visit, "site.name") <- site.name
    attr(visit, "site.abbr") <- site.abbr
    attr(visit, "runID") <- run
    attr(visit, "run.name") <- run.name
    attr(visit, "run.season") <- run.season
    cat("Check dates...\n")
    return(visit)   
  }


  #   ---- This query pulls the trap re-starts.
  sql.visit <- paste( "SELECT ", visit.vars.sql, 
      " FROM (", s.tab, " INNER JOIN ", ss.tab, " ON ", s.tab, ".siteID", " = ", ss.tab, ".siteID", ")", 
      " INNER JOIN ", tv.tab, " ON ", ss.tab, ".subsiteID", "=", tv.tab, ".trapPositionID", 
      " WHERE (((", s.tab, ".siteID)=", site, ") ",
      "AND ((", tv.tab, ".visitTime)>=#", format(run.season$start-(7*24*60*60), "%m/%d/%Y"), "#) ",
      "AND ((", tv.tab, ".visitTime2)<=#", format(run.season$end, "%m/%d/%Y"), "#) ",
      "AND ((", tv.tab, ".visitTypeID)=1 Or (", tv.tab, ".visitTypeID)=3));",
      sep="")

  #   ---- Communicate with Console and/or out file, replacing text in SQL statement with other
  #   ---- text to facilitate reading by humans.  
  cat("\nVISITS where trap was stopped or started SQL statement: \n")
  out.sql <- gsub( ",", "\n,", sql.visit, fixed=T)
  out.sql <- gsub( "FROM", "\nFROM", out.sql, fixed=T)
  out.sql <- gsub( ") ", ")\n", out.sql, fixed=T)
  cat(out.sql)
  cat("\n")

  #   ---- Pull the data from the query. 
  restarts <- sqlQuery(ch, sql.visit)
  F.sql.error.check(restarts)
  cat(paste("\nNumber of start-stop visits found:", nrow(restarts), "\n"))


  #   ---- Append restarts to end of visits.  Times will be sorted in assign_sample_period.r.
  visit <- rbind(visit, restarts) 

  #   ---- Make sure time zones are Pacific. In case analysis is run in another time zone.
  visit$visitTime <- as.POSIXct( format(visit$visitTime, format="%Y-%m-%d %H:%M:%S"), format="%Y-%m-%d %H:%M:%S", tz="America/Los_Angeles") 
  visit$visitTime2 <- as.POSIXct( format(visit$visitTime2, format="%Y-%m-%d %H:%M:%S"), format="%Y-%m-%d %H:%M:%S", tz="America/Los_Angeles")  
  visit$batchDate <- as.POSIXct( format(visit$batchDate, format="%Y-%m-%d %H:%M:%S"), format="%Y-%m-%d %H:%M:%S", tz="America/Los_Angeles")  

  #   ---- Assign sample period start and stop.
  visit <- F.assign.sample.period(visit)


#   Get rid of missing sampleEnd's.  sampleEnd's are missing for lines when trap restarted.  
visit <- visit[!is.na(visit$sampleEnd),]

#   Get rid of missing sampleStarts's.  This is an error.  Happens when trap stopped (visitType=4), but then next record trap is running (visitType=2 or 3) without an intervening trap start (visitType=1)
if( sum(is.na(visit$sampleStart)) > 0 ){
    #warning("Trap stopped, but no trap start record.  See LOG.")
    cat("\n\n\nObservations with no trap start records (these were dropped):\n")
    print(visit[ is.na(visit$sampleStart), ] )
    cat("\n\n")
    #   Drop them, this will create a gap, that will later be imputed.
    visit <- visit[!is.na(visit$sampleStart),]
}


#   Assign batch date if missing
visit <- F.assign.batch.date(visit)


#   Delete the "extra" records outside the run period
ind <- (run.season$start <= visit$sampleEnd) & (visit$sampleEnd <= run.season$end)
visit <- visit[ind,]


#   Assign gap lengths
visit <- F.assign.gaplen( visit )

cat(paste("\nNumber of trap visits:", nrow(visit), "\n"))
cat(paste("First 10 visit records:\n"))
print(visit[1:10,])

#   Find subsiteID names
#   Fetch subsite names
subsite.names <- sqlQuery( ch, paste("SELECT subSiteID, subSiteName FROM", tables["subsites"], 
        "WHERE (siteID =", site, ")" ))
F.sql.error.check(subsite.names)
subsite.names$subSiteName <- as.character(subsite.names$subSiteName)        

subSites.found <- data.frame(subSiteID=sort(unique(visit$trapPositionID)))
subSites.found <- merge( subSites.found, subsite.names, by="subSiteID", all.x=T )

#   Store values as attribute for convienance.
attr(visit, "siteDescriptionID" ) <- site
attr(visit, "site.name") <- site.name
attr(visit, "site.abbr") <- site.abbr
attr(visit, "subsites" ) <- subSites.found
attr(visit, "runID") <- run
attr(visit, "run.name") <- run.name
attr(visit, "run.season") <- run.season

odbcClose(ch)

visit

}

