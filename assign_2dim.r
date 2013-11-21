F.assign.2dim <- function(catch, var1, var2 ){
#
#   Values are missing in two dimensions.  I.e., var1 and var2 are missing, but n>0.
#
#   Assign a var1 and var2 value based on joint distribution
#

    #   Idea is to make a new variable with joint levels, then assign1dim
    
    jointLevs <- paste(catch[,var1], catch[,var2], sep=".")
    jointLevs[ jointLevs == "Unassigned.Unassigned" ] <- "SAVED"  # Will change this later.  This makes sure reassignment below with grep does not change these
    jointLevs[ grep("Unassigned", jointLevs) ] <- "NA"  # these are things like Spring.Unassigned, which could not be assiged in 1dim.  we want to leave these
    jointLevs[ grep("NA", jointLevs) ] <- NA  # change NA.NA to just NA
    jointLevs[ jointLevs == "SAVED" ] <- "Unassigned"  # change back to lower case so 1dim function will recognize
    
    jointLevs <- factor(jointLevs)
    
    JointDist <- rep(1,nrow(catch))
    JointDist <- factor(JointDist, levels=1, labels=c("Yes"))
    JointDist[ is.na(jointLevs) ] <- NA  # when joint levels is missing, joint distribution is missing
    
    catch$jointLevs <- jointLevs
    catch$JointDist <- JointDist

    #   Call the routine that assigns plus counts based on a 1 dimensional distribution    
    catch <- F.assign.1dim( catch, present.var="JointDist", absent.var="jointLevs" )


    #   Now, put the new assignments back in FinalRun and lifeStage
    jointLevs <- paste(catch[,var1], catch[,var2], sep=".")
    ind <- jointLevs == "Unassigned.Unassigned" 
    newAssigned <- as.character(catch$jointLevs[ind])
    newAssigned <- strsplit(newAssigned, ".", fixed=T)
    newrun <- unlist(lapply( newAssigned, "[", 1))
    newlife <- unlist(lapply( newAssigned, "[", 2))
    catch$FinalRun[ind] <- newrun
    catch$lifeStage[ind] <- newlife
        
    # Finally, drop the columns we created.     
    catch <- catch[, !(names(catch) %in% c("jointLevs", "JointDist"))]  

    catch

}
