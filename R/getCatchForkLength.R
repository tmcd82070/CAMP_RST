#' @export
#' 
#' @title getCatchForkLength - Retrieve catch data with fork length information attaches.
#'   
#' @description Retrieve a data frame from the CAMP Access dBAse containing
#' catches and fork length.  This routine includes fish from both \code{includeCatch = TRUE}
#' and \code{includeCatch = FALSE} catches. 
#' 
#' @inheritParams F.size.by.date
#'   
#' @inheritSection F.size.by.date Details
#' 
#'   
#' @return Either a single histogram, if variable \code{by.lifestage} is set to
#'   \code{FALSE}. Otherwise, a histogram for each individual life stage present
#'   within the data.
#'   
#' @inheritSection F.size.by.date Author
#'   
#' @seealso \code{F.get.indiv.fish.data} 
#'   
#' @examples
#' \dontrun{
#' #   ---- Obtain graphical histograms for the American. 
#' site <- 57000
#' taxon <- 161980
#' run <- 3
#' min.date <- "2014-01-01"
#' max.date <- "2014-06-06"
#' output.file <- "American"
#' by.lifestage <- TRUE
#' 
#' F.length.frequency(site,taxon,run,min.date,max.date,output.file,by.lifestage)
#' }


getCatchForkLenth <- function( site, taxon, run, min.date, max.date ){
  
  #   ---- Make sure we have all temp tables.
  tableChecker()
  
  #   ---- Get global environment stuff.
  db.file <- get("db.file",envir=.GlobalEnv)
  table.names <- get("table.names",envir=.GlobalEnv)
  
  #   ---- Retrieve basic data set, with one line per fish or group of fish of same length.
  catch.df  <- F.get.indiv.fish.data( site, taxon, run, min.date, max.date, keep="unmarked" )
  
  #   ---- Remove unnecessary variables.  
  catch.df <- catch.df %>% dplyr::select( -preUnmarked, 
                                          -halfConeAssignedCatch, 
                                          -oldtrapPositionID,
                                          -halfConeUnassignedCatch,
                                          -assignedCatch,
                                          -unassignedCatch,
                                          -modUnassignedCatch,
                                          -modAssignedCatch )
  #   ---- When catch.df has no data, it doesn't get batchDates added.  Add this.
  if(nrow(catch.df) == 0){
    catch.df <- catch.df %>% 
      dplyr::rename(batchDate = SampleDate)
  }
  
  #   ---- Grab non-valid catch, while preserving the attributes from the catch query.
  attributesSafe <- attributes(catch.df)
  ch <- RODBC::odbcConnectAccess(db.file)
  
  F.run.sqlFile( ch, "QryNonValidFishing.sql", R.TAXON=taxon )   
  nvCatch <- RODBC::sqlFetch( ch, "TempSumUnmarkedByTrap_Run_X_final" )       
  F.sql.error.check(nvCatch)
  
  #   ---- Fetch run name for use in reporting and query restrictions.
  runs <- RODBC::sqlQuery(ch, paste( "SELECT run, runID FROM", table.names["run.codes"] ))
  F.sql.error.check(runs)
  run.name <- runs %>% 
    dplyr::filter( runID == !!run ) %>% 
    dplyr::pull(run)
  
  close(ch)
  
  #   ---- Construct the data frames needed for the plot.  In other words, 
  #   ---- subset the catches to just positives.  Toss the 0 catches.
  nvCatch <- nvCatch %>% 
    dplyr::filter( (Unmarked > 0) & (FinalRun == run.name) ) 
  
  #   ---- Check if there is any non-valid catch.  
  if(nrow(nvCatch) > 0){
    nvCatch <- nvCatch %>% 
      dplyr::mutate(Unassd = lifeStage)
    namesToKeep <- names(catch.df)
    nvCatch <- F.expand.plus.counts( nvCatch )
    nvCatch$includeCatchID <- 2
    nvCatch <- F.assign.batch.date( nvCatch )
    nvCatch <- nvCatch %>% 
      dplyr::select( !!namesToKeep )
    
    #   ---- Check if we also have valid catch.  
    if(nrow(catch.df) > 0 & nrow(nvCatch) > 0){
      catch.df <- dplyr::bind_rows(catch.df,nvCatch)  # attributes of catch.df are copied over here
      disc <- "Fork lengths from both 'include' = TRUE (successful) and 'include' = FALSE (unsuccessful) trap occasions."
      
      #   ---- Check if we have non-valid catch alone.  
    } else if(nrow(catch.df) == 0 & nrow(nvCatch) > 0){
      # the following dplyr method preserves attributes of catch.df
      catch.df <- catch.df %>% 
        dplyr::filter(FALSE) %>% 
        dplyr::bind_rows(nvCatch)
      
      disc <- "Fork lengths from only 'include' = FALSE (unsuccessful) trap occasions."   
    } 
    
    #   ---- Check if we only have valid catch data, and no non-valid data.  
  } else if(nrow(catch.df) > 0 & nrow(nvCatch) == 0){
    disc <- "Fork lengths from only 'include' = TRUE (successful) trap occasions."        
  } 
  
  #   ---- Deal with the situation when no records ever found.
  if( nrow(catch.df) == 0 ){
    return(catch.df)
  }
  
  #   ---- In the case when lifeStage is a factor, convert to character.  
  if(class(catch.df$lifeStage) == 'factor'){
    catch.df$lifeStage <- as.character(droplevels(catch.df$lifeStage))
  }   
  
  #   ---- Prevent records from plotting if any critical data are missing.  This means
  #   ---- limit the lifestages to fry, parr, and smolt.
  catch.df <- catch.df %>% 
    dplyr::filter( !is.na(EndTime) ) %>% 
    dplyr::filter( !is.na(forkLength) ) %>% 
    dplyr::filter( !is.na(lifeStage) ) %>% 
    dplyr::filter( !is.na(Unmarked) ) 
  
  if( (length(taxon) == 1) & (taxon == 161980) ){
    catch.df <- catch.df %>% 
      dplyr::filter( lifeStage %in% c('Fry','Parr','Smolt') ) 
  }  
 
  #   ---- Make some labels for later plots and return them as attributes
  attr(catch.df, "disc") <- disc
  
  #   lifestage labels - must translate using lookup table
  ch <- RODBC::odbcConnectAccess(db.file)
  CAMP.life.stage <- RODBC::sqlFetch(ch, table.names["CAMP.life.stages"])
  close(ch)
  life.stages <- sort(unique( catch.df$lifeStage ))  # observed lifestages, maybe different than c('Fry','Parr','Smolt')
  myleg <- CAMP.life.stage %>% 
    dplyr::filter( lifeStageCAMP %in% life.stages ) %>% 
    dplyr::pull(lifeStageCAMP) %>% 
    as.character()
  attr(catch.df, "legendEntries") <- myleg
  
  catch.df
}