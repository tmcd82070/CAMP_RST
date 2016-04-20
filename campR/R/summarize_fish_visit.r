#' @export F.summarize.fish.visit
#' 
#' @title F.summarize.fish.visit
#' 
#' @description
#' 
#'    Summarize fish count and fork lengths over visits.
#' 
#'    catch = data frame containing one line per fish or fish group with identical lengths.
#'        If they did not catch any member of a particular taxon, catch will have 0 rows.  This 
#'        data frame will usually be the output of F.get.indiv.fish.data()
#' 
#'    variable = summarize over all data (so inflated counts), or assigned counts (measured fish only).
#' 
#'    Output = a data frame with one line per visit, with catch summarized.
#' 
#' 
#' 
#' @param  catch <describe argument>
#' @param variable  <describe argument>
#' 
#' @details <other comments found in file>
#'    variable <- 'unassigned'
#'    variable <- 'halfcone'
#'  jason add - first pass assumed that unassigned fork length is NA.  but...not NA for one day in run i'm investigating.  force this.
#'    These are variables that are constant within a trapVisit, run, and lifestage
#'    indexes = all unique combinations of visit, run, and life stage
#'    indexes = NA for all gaps in the trapping sequence
#'    These are variables that are constant within a trapVisit, run, and lifestage
#'    indexes = all unique combinations of visit, run, and life stage
#'    indexes = NA for all gaps in the trapping sequence
#'    These are variables that are constant within a trapVisit, run, and lifestage
#'    indexes = all unique combinations of visit, run, and life stage
#'    indexes = NA for all gaps in the trapping sequence
#'    These are variables that are constant within a trapVisit, run, and lifestage
#'    indexes = all unique combinations of visit, run, and life stage
#'    indexes = NA for all gaps in the trapping sequence
#'    Because of the gap lines, there are NA's in indexes (because there are NA's in trapVisitID). 
#'    Fix these.  To assure indexes is same length as catch, we cannot simply remove NA's here. 
#'    Solution: set them to -1, then remove -1 from u.ind.  This way the loop below 
#'    just skips them.
#'    Initialize place to store summarized catches. 
#' u.ind <- sort(unique(indexes))  # NA's in indexes are lost here, in the sort
#' catch.fl <- as.data.frame( matrix( NA, length(u.ind), length(const.vars) + 3 ))
#' names( catch.fl ) <- c(const.vars, "n.tot", "mean.fl", "sd.fl")
#'    Count number of fish, compute mean fork length, etc per visitID in catch.  
#'    Copy over constant variables
#'    Take weighted averages of fork lengths using 'n' as weights
#'    Add back in the lines with missing indexes.  These correspond to gaps in trapping
#'     tmp <- catch[ indexes < 0, const.vars ]
#'     tmp <- cbind( tmp, matrix( NA, sum(indexes < 0), 3))
#'     names( tmp ) <- c(const.vars, "n.tot", "mean.fl", "sd.fl")
#'     catch.fl <- rbind( catch.fl,  tmp )
#'    Sort the result by trap positing and trap visit
#'  no measured fish, but both finalrun and lifestage assigned.      
#' 
#' @return <describe return value>
#' 
#' @author WEST Inc.
#' 
#' @seealso \code{\link{<related routine>}}, \code{\link{<related routine>}}
#' 
#' @examples
#' <insert examples>
#' 
F.summarize.fish.visit <- function( catch,variable ){
#
#   Summarize fish count and fork lengths over visits.
#
#   catch = data frame containing one line per fish or fish group with identical lengths.
#       If they did not catch any member of a particular taxon, catch will have 0 rows.  This 
#       data frame will usually be the output of F.get.indiv.fish.data()
#
#   variable = summarize over all data (so inflated counts), or assigned counts (measured fish only).
#
#   Output = a data frame with one line per visit, with catch summarized.
#

  
#   catch <- catch.df
#   variable <- 'unassigned'
#   variable <- 'halfcone'
  

  # jason add - first pass assumed that unassigned fork length is NA.  but...not NA for one day in run i'm investigating.  force this.
  if(nrow(catch[catch$Unassd == 'Unassigned',]) > 0){
    catch[catch$Unassd == 'Unassigned',]$forkLength <- NA
  }
  doit <- 0
  
  
if( nrow(catch) > 0 ){

  if(variable == 'unassigned'){         # fish counts with plus counts, but allow for pulling out the plus counted fish alone by code below.
    
  catch <- catch[catch$Unassd == 'Unassigned',]   # add 10/12/2015 to make sure only true unassiged fish go through the unassigned sequence.
    
    #   These are variables that are constant within a trapVisit, run, and lifestage
    const.vars<-c("ProjID", "trapVisitID", "batchDate", "StartTime", "EndTime", "SampleMinutes", 
                  "TrapStatus", "siteID", "siteName", "trapPositionID", "TrapPosition", "sampleGearID", 
                  "sampleGear", "halfConeID", "HalfCone", "FinalRun", "lifeStage" )                    
    
    #   indexes = all unique combinations of visit, run, and life stage
    #   indexes = NA for all gaps in the trapping sequence
  if(nrow(catch) > 0){
    indexes <- tapply( 1:nrow(catch), list( catch$trapVisitID, catch$FinalRun, catch$lifeStage, catch$Unassd ) )  
    doit <- 1
  } 
    
} else if(variable == 'inflated'){    # fish counts with plus counts.  this is what was originally coded.
    
    #   These are variables that are constant within a trapVisit, run, and lifestage
    const.vars<-c("ProjID", "trapVisitID", "batchDate", "StartTime", "EndTime", "SampleMinutes", 
        "TrapStatus", "siteID", "siteName", "trapPositionID", "TrapPosition", "sampleGearID", 
        "sampleGear", "halfConeID", "HalfCone", "FinalRun", "lifeStage" )                    

    #   indexes = all unique combinations of visit, run, and life stage
    #   indexes = NA for all gaps in the trapping sequence
    indexes <- tapply( 1:nrow(catch), list( catch$trapVisitID, catch$FinalRun, catch$lifeStage ) )  
    doit <- 1
    
  } else if(variable == 'assigned'){     # fish counts without plus counts, so measured == assigned ONLY.  jason add - 4/15/2015.
    
    #   These are variables that are constant within a trapVisit, run, and lifestage
    const.vars<-c("ProjID", "trapVisitID", "batchDate", "StartTime", "EndTime", "SampleMinutes", 
                  "TrapStatus", "siteID", "siteName", "trapPositionID", "TrapPosition", "sampleGearID", 
                  "sampleGear", "halfConeID", "HalfCone", "FinalRun", "lifeStage", "Unassd" )                     
    
    #   indexes = all unique combinations of visit, run, and life stage
    #   indexes = NA for all gaps in the trapping sequence
    
    indexes <- tapply( 1:nrow(catch), list( catch$trapVisitID, catch$FinalRun, catch$Unassd ) ) 
    doit <- 1
  } else if (variable %in% c('halfConeAssignedCatch','halfConeUnassignedCatch','assignedCatch','unassignedCatch','modAssignedCatch','modUnassignedCatch')){    # fish counts of half-cone expansions
    
    #   These are variables that are constant within a trapVisit, run, and lifestage
    const.vars<-c("ProjID", "trapVisitID", "batchDate", "StartTime", "EndTime", "SampleMinutes", 
                  "TrapStatus", "siteID", "siteName", "trapPositionID", "TrapPosition", "sampleGearID", 
                  "sampleGear", "halfConeID", "HalfCone", "FinalRun", "lifeStage", "Unassd" )                     
    
    #   indexes = all unique combinations of visit, run, and life stage
    #   indexes = NA for all gaps in the trapping sequence
    
    indexes <- tapply( 1:nrow(catch), list( catch$trapVisitID, catch$FinalRun, catch$lifeStage ) ) 
    doit <- 1
    
           if(variable == 'halfConeAssignedCatch')  {catch$Unmarked <- catch$halfConeAssignedCatch     # code below expects a variable named Unmarked.  Let's make this easy.
    } else if(variable == 'halfConeUnassignedCatch'){catch$Unmarked <- catch$halfConeUnassignedCatch   # code below expects a variable named Unmarked.  Let's make this easy.
    } else if(variable == 'assignedCatch')          {catch$Unmarked <- catch$assignedCatch             # code below expects a variable named Unmarked.  Let's make this easy.
    } else if(variable == 'unassignedCatch')        {catch$Unmarked <- catch$unassignedCatch           # code below expects a variable named Unmarked.  Let's make this easy.
    } else if(variable == 'modAssignedCatch')       {catch$Unmarked <- catch$modAssignedCatch          # code below expects a variable named Unmarked.  Let's make this easy.
    } else if(variable == 'modUnassignedCatch')     {catch$Unmarked <- catch$modUnassignedCatch        # code below expects a variable named Unmarked.  Let's make this easy.
    }
  }
  
  

  
  
  
  if(doit == 1){
    #   Because of the gap lines, there are NA's in indexes (because there are NA's in trapVisitID). 
    #   Fix these.  To assure indexes is same length as catch, we cannot simply remove NA's here. 
    #   Solution: set them to -1, then remove -1 from u.ind.  This way the loop below 
    #   just skips them.
    indexes[ is.na(indexes) ] <- -1

    #   Initialize place to store summarized catches. 
    #u.ind <- sort(unique(indexes))  # NA's in indexes are lost here, in the sort
    #catch.fl <- as.data.frame( matrix( NA, length(u.ind), length(const.vars) + 3 ))
    #names( catch.fl ) <- c(const.vars, "n.tot", "mean.fl", "sd.fl")
  
    u.ind <- indexes[!duplicated(indexes)]
    catch.fl <- catch[!duplicated(indexes),const.vars]  # these are not the right lines, they will be replaced inside the loop.  This just initializes
    catch.fl <- catch.fl[ u.ind > 0, ]    #  Don't want the former NA's, which are now -1's. 
    u.ind <- u.ind[ u.ind > 0 ]  #  Don't want the former NA's, which are now -1's. 
    catch.fl <- cbind( catch.fl, matrix( NA, nrow(catch.fl), 3))
    names( catch.fl ) <- c(const.vars, "n.tot", "mean.fl", "sd.fl")

    #   Count number of fish, compute mean fork length, etc per visitID in catch.  
    for( i in 1:length(u.ind) ){

        ind <- (u.ind[i] == indexes) & !is.na(indexes)
    
        #   Copy over constant variables
        catch.fl[i,const.vars] <- catch[ind,const.vars][1,]
        
        catch.fl$n.tot[i]       <- sum( catch$Unmarked[ind], na.rm=T ) # Don't think Unmarked can be missing, but rm just in case.

        #   Take weighted averages of fork lengths using 'n' as weights
        if( !is.na(catch.fl$n.tot[i]) & (catch.fl$n.tot[i] > 0) ){ # I don't actually know whether catch.fl$n.tot[i] can be missing, but just in case
            fl <- rep(catch$forkLength[ind & !is.na(catch$forkLength)], catch$Unmarked[ind & !is.na(catch$forkLength)])      # making a sample
            catch.fl$mean.fl[i]           <- mean( fl, na.rm=T )   # could have missing fork length
            catch.fl$sd.fl[i]             <- sd( fl , na.rm=T )
        } else {
            catch.fl$mean.fl[i] <- NA
            catch.fl$sd.fl[i] <- NA
        }
        
    }
    
    #   Add back in the lines with missing indexes.  These correspond to gaps in trapping
#    tmp <- catch[ indexes < 0, const.vars ]
#    tmp <- cbind( tmp, matrix( NA, sum(indexes < 0), 3))
#    names( tmp ) <- c(const.vars, "n.tot", "mean.fl", "sd.fl")
#    catch.fl <- rbind( catch.fl,  tmp )

    #   Sort the result by trap positing and trap visit
    catch.fl <- catch.fl[ order( catch.fl$trapPositionID, catch.fl$EndTime ), ]
  
    if(variable == 'unassigned'){
            
      catch.fl <- catch.fl[is.nan(catch.fl$mean.fl),]# & (catch.fl$FinalRun == 'Unassiged' | catch.fl$lifeStage == 'Unassiged'),] # jason add 10/3/2015 to deal w/ rare situation of having
      # no measured fish, but both finalrun and lifestage assigned.      
      
    }
  } else {
    catch.fl <- data.frame(matrix(vector(), 0, 20,
                           dimnames=list(c(), c("ProjID","trapVisitID","batchDate","StartTime","EndTime","SampleMinutes","TrapStatus","siteID","siteName","trapPositionID","TrapPosition","sampleGearID","sampleGear","halfConeID","HalfCone","FinalRun","lifeStage","n.tot","mean.fl","sd.fl")  )),
                          stringsAsFactors=F)  # need to have at least an empty df with defined column names/vars
  } # end doit

} else {
    catch.fl <- NA
}

catch.fl

}
