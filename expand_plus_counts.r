F.expand.plus.counts <- function( catch ){
#
#   Expand the data set in 'catch' to account for plus counts.  Plus counts are lines with missing finalRunID or lifeStageID.
#

cat("----------- Assigning run and lifestage based on Plus Counts----------\n")
cat("Number of records over season in run X lifestage matrix upon entry:\n")
#tmp <- is.na(catch$forkLength)
#tmp <- factor(tmp, levels=c(FALSE, TRUE), labels=c("Present (non NULL)", "Missing"))
#print( table(runID=catch$finalRunID, lifeStageID=catch$lifeStageID, forkLength=tmp, useNA="ifany") )
print( table(finalRunID=catch$finalRunID, lifeStageID=catch$lifeStageID, useNA="ifany") )
cat("Lifestage codes >= 250 will be reset and assigned a lifestage based on plus counts.\n")
cat("Run codes 250, 251, 253, 254, and 255 (all except 'not applicable') will be reset and assigned a run based on plus counts.\n\n")


##   Pull the run and lifestage codes from the data base
#db <- get( "db.file", env=.GlobalEnv ) 
#tables <- get( "table.names", env=.GlobalEnv )
#ch <- odbcConnectAccess(db)
#
#luRun <- sqlFetch( ch, tables["run.codes"] )
#luLifeStage <- sqlFetch( ch, tables["life.stages"] )
#Camp.luLifeStage <- sqlFetch( ch, tables["CAMP.life.stages"] )
#
#close(ch)



#   Replace CAMP's missing values with R's missing values
catch$lifeStageID[ catch$lifeStageID >= 250 ] <- NA
catch$finalRunID[ catch$finalRunID %in% c(250,251,253,254,255) ] <- NA


#   Assign plus counts one dimension at a time, then for the double missing
#
#   NOTE: catch$n cannot be missing by the time it gets here.  An error is thrown in the calling routine if any n's are missing.
#

#   ---- Deal with missing runID, but present lifestageID (i.e., Assign a run)
cat("---- lifeStageID present, finalRunID absent\n")
catch <- F.assign.1dim( catch, present.var="lifeStageID", absent.var="finalRunID" )


#   ---- Deal with missing lifeStageID, but present finalRunID (i.e., Assign a life stage)
cat("\n")
cat("---- finalRunID present, lifeStageID absent\n")
catch <- F.assign.1dim( catch, present.var="finalRunID", absent.var="lifeStageID" )


#   ---- Deal with missing lifeStageID AND missing finalRunID (i.e., assign a life stage and a run)
cat("\n")
cat("---- finalRunID absent AND lifeStageID absent\n")
catch <- F.assign.2dim( catch, var1="finalRunID", var2="lifeStageID" )

cat("Final number of rows in run X life stage table:\n")
print( table(finalRunID=catch$finalRunID, lifeStageID=catch$lifeStageID, useNA="ifany") )

cat("----------- DONE assigning plus Counts----------\n")

catch
}

#source("assign_1dim.r")
#source("assign_2dim.r")
