#' @export
#' 
#' @title tableChecker
#' 
#' @description Query the CAMP.mdb for all internal
#'   tables, and in the case any identified "temp" tables are missing, insert
#'   "fake" ones, so as to ensure those temp tables are available for deletion
#'   in subsequent SQL queries.
#'   
#' @details The function requires no arguments.  The location of the CAMP.mdb of
#'   interest is known via externally defined global variable \code{db.file}.
#'   
#'   Additionally, the current set of temp tables used for checking is
#'   hard-coded within the function, so this set of tables does not require
#'   specification.
#'   
#' @return The function is silent.  It simply reports messages to the log
#'   file indicating copied tables.
#'   
#' @author WEST, Inc.
#'   
tableChecker <- function(){
  
  #   ---- See what tables we have in our current db.file, or CAMP.mdb.  
  db <- get( "db.file", envir=.GlobalEnv )
  ch <- RODBC::odbcConnectAccess(db)
  tables <- RODBC::sqlTables(ch)
  tables <- tables[tables$TABLE_TYPE == "TABLE",]$TABLE_NAME
  close(ch)
  
  #   ---- Define the set of all tables.  This should perhaps be in GlobalVars.  
  datTables <- c("CatchRaw","CustomQueries","Dates","EnvDataDaily",                     
                 "EnvDataDailyXTargetSite","EnvDataRaw",       
                 "EnvDataRawXTargetSite","LengthAtDate",
                 "MarkApplied","MarkExisting","MarkShortcuts","People",                           
                 "PostHandleMort","PostHandleMortMark","ProjectDescription","ReadMe",                           
                 "Release","ReleaseFish","ReleaseXTargetSite","Settings",                         
                 "Site","Specimen","StreamFlow","SubSite",                          
                 "TaxonImages","TaxonNative","TrapVisit","Version")
  luTables  <- c("luActive","luAgency",                       
                 "luBodyPart","luColor","luConeDebrisVolumeCat","luDebrisVolumeCat",                
                 "luFishOrigin","luFishProcessed","luLifeStage","luLifeStageCAMP",                  
                 "luLightCondition","luMarkType","luNoYes","luQueryType",                      
                 "luReleasePurpose","luRights","luRun","luRunMethod",                      
                 "luSampleGear","luSpecimenType","luSubsampleMethod","luTaxon",                          
                 "luTrapFunctioning","luUnit","luVisitType")
  tmpTables <- c("EnvDataRaw_Standardized","EnvDataRaw_StandardSelect","NonTrapSample1","TrapSample1",
                 "TempChinookSampling_a","TempChinookSampling_b",            
                 "TempChinookSampling_c","TempChinookSampling_d1","TempChinookSampling_d2","TempChinookSampling_d3",           
                 "TempChinookSampling_e","TempChinookSampling_f","TempChinookSampling_g","TempChinookSampling_h",            
                 "TempChinookSampling_i_final","TempEffortSummary_a","TempEffortSummary_b","TempNonSamplingSummary",           
                 "TempRelRecap_1","TempRelRecap_3","TempRelRecap_final","TempReportCriteria_Release",       
                 "TempReportCriteria_Trapvisit","tempSamplingSumCHN","TempSamplingSummary","TempSamplingSummary_Final",        
                 "TempSumUnmarkedByTrap_Run_a","TempSumUnmarkedByTrap_Run_b","TempSumUnmarkedByTrap_Run_Final","TempSumUnmarkedByTrap_Run_Final2", 
                 "TempSumUnmarkedByTrap_Run_X_Final")
  
  #   ---- Check to make sure tables in queried mdb in allTables check vector.  
  if(any(!(tmpTables %in% tables))){
    
    #   ---- We are missing a tmp table somehow.  Make a copy of a table that exists.  Only copy a Temp table, although
    #   ---- I guess any table could be used, and we could probably just use a CREATE TABLE type of statement; i.e.,
    #   ---- not use an existing table at all.  
    for(i in 1:sum(!(tmpTables %in% tables))){
      
      #   ---- Go through all the missing tables and make a copy, one-by-one.  
      needThisTmpTable <- tmpTables[!(tmpTables %in% tables)][i]
      
      #   ---- Make a fake dataframe to push up to CAMP.mdb.  
      fake <- data.frame(Fake=seq(1,10,1))
      
      #   ---- Connect and push the fake dataframe.  
      ch <- RODBC::odbcConnectAccess(db)
      RODBC::sqlSave(ch,
                     fake,
                     tablename=needThisTmpTable,
                     varTypes=c(Fake="integer"),
                     rownames=FALSE)
      cat(paste0("I copied in a version of temp table ",needThisTmpTable,".\n"))
      close(ch)
    }
  }
  cat(paste0("All expected temp tables present.\n"))
}