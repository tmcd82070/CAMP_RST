F.expand.plus.counts <- function( catch ){
#
#   Expand the data set in 'catch' to account for plus counts.  Plus counts are lines with missing FinalRun or lifeStage.
#

cat("----------- Assigning run and lifestage based on Plus Counts----------\n")
cat("Number of records over season in run X lifestage matrix upon entry:\n")

print( table(FinalRun=catch$FinalRun, lifeStage=catch$lifeStage, useNA="ifany") )



#   Assign plus counts one dimension at a time, then for the double missing
#
#   NOTE: catch$Unmarked cannot be missing by the time it gets here.  
#

#   ---- Deal with missing run, but present lifestage (i.e., Assign a run)
cat("---- lifeStage present, FinalRun absent\n")
catch <- F.assign.1dim( catch, present.var="lifeStage", absent.var="FinalRun" )


#   ---- Deal with missing lifeStage, but present finalRun (i.e., Assign a life stage)
cat("\n")
cat("---- FinalRun present, lifeStage absent\n")
catch <- F.assign.1dim( catch, present.var="FinalRun", absent.var="lifeStage" )


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
