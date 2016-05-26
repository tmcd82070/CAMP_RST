#' @export
#' 
#' @title F.summarize.fish.visit 
#' 
#' @description 
#' Summarize fish count and fork lengths over visits.
#' 
#' @param catch A data frame containing one line per fish or fish group with 
#'   identical lengths.  This data frame results from \code{F.get.catch.data}.
#' @param The type of fish for which summary is required.
#'   
#' @return A data frame with one line per visit, with catch summarized. Summary
#'   statistics include N, mean, and standard deviation.
#'   
#' @details Results from the catch-query sequence summarize the number of fish 
#'   per \code{trapVisitID}, \code{lifeStage}, \code{FinalRun}, and 
#'   \code{forkLength};  these need to be summarized, depending on the type of 
#'   fish.
#'   
#'   Assigned fish are summarized via an index comprised of variables 
#'   \code{trapVisitID}, \code{FinalRun}, and \code{Unassd}.  Note that here, 
#'   assigned fish are assumed to have only their run assigned, and not 
#'   necessarily their life stage.  Assigned fish are separate from unassigned
#'   fish, meaning they were randomly selected, and thus form the basis of
#'   sampling proportions of run and life stage utilized via the plus-count
#'   algorithm.
#'   
#'   Unassigned fish are summarized via an index comprised of variables 
#'   \code{trapVisitID}, \code{FinalRun}, \code{lifeStage}, and \code{Unassd}.
#'   
#'   Inflated fish (all captured fish, including plus-counts) are summarized via
#'   an index comprised of \code{trapVisitID}, \code{FinalRun}, and 
#'   \code{lifeStage}.  Because inflated fish include plus-counted fish that 
#'   have been re-assigned to sampled-based counts of run and life stage, 
#'   indexing by variable \code{Unassd} is unnecessary.
#'   
#'   All other fish types for which summaries are required, namely those
#'   involved with the adjustment for half-cone operations, have an index of 
#'   \code{trapVisitID}, \code{FinalRun}, and \code{lifeStage}.
#'   
#'   Weighted fork lengths are recorded to the nearest millimeter.  Each is
#'   calculated over unique indices and weighted on the number of fish.
#'   
#' @examples 
#'
F.summarize.fish.visit <- function( catch,variable ){
  
  # catch <- catch.df
  # variable <- 'unassigned'

  # We assume that unassigned fork length is NA.  Force this.
  if(nrow(catch[catch$Unassd == 'Unassigned',]) > 0){
    catch[catch$Unassd == 'Unassigned',]$forkLength <- NA
  }
  
  #   ---- Set an indicator variable telling us if summary statistics are required. 
  doit <- 0
  
  if( nrow(catch) > 0 ){

    #   ---- Fish counts with plus counts;  however, allow for pulling out the plus-counted
    #   ---- fish alone via code below.
    if(variable == 'unassigned'){         
    
      #   ---- Ensure only true unassiged fish go through the unassigned sequence.
      catch <- catch[catch$Unassd == 'Unassigned',]   
    
      #   ---- These are variables that are constant within a trapVisit, run, and lifestage,  
      const.vars<-c("ProjID", "trapVisitID", "batchDate", "StartTime", "EndTime", "SampleMinutes", 
                    "TrapStatus", "siteID", "siteName", "trapPositionID", "TrapPosition", "sampleGearID", 
                    "sampleGear", "halfConeID", "HalfCone", "FinalRun", "lifeStage" )                    
    
      #   ---- Vector indexes = all unique combinations of visit, run, and life stage.
      #   ---- Vector indexes = NA for all gaps in the trapping sequence.
      if(nrow(catch) > 0){
        indexes <- tapply( 1:nrow(catch), list( catch$trapVisitID, catch$FinalRun, catch$lifeStage, catch$Unassd ) )  
        doit <- 1
      } 
    
    #   ---- Fish counts with plus counts.  This is what was originally coded.
    } else if(variable == 'inflated'){    
    
      #   ---- These are variables that are constant within a trapVisit, run, and lifestage,
      const.vars<-c("ProjID", "trapVisitID", "batchDate", "StartTime", "EndTime", "SampleMinutes", 
                    "TrapStatus", "siteID", "siteName", "trapPositionID", "TrapPosition", "sampleGearID", 
                    "sampleGear", "halfConeID", "HalfCone", "FinalRun", "lifeStage" )                    

      #   ---- Vector indexes = all unique combinations of visit, run, and life stage.
      #   ---- Vector indexes = NA for all gaps in the trapping sequence.
      indexes <- tapply( 1:nrow(catch), list( catch$trapVisitID, catch$FinalRun, catch$lifeStage ) )  
      doit <- 1
      
    #   ---- Fish counts without plus counts, so measured == assigned ONLY. 
    } else if(variable == 'assigned'){    
    
      #   ---- These are variables that are constant within a trapVisit, run, and lifestage.
      const.vars<-c("ProjID", "trapVisitID", "batchDate", "StartTime", "EndTime", "SampleMinutes", 
                    "TrapStatus", "siteID", "siteName", "trapPositionID", "TrapPosition", "sampleGearID", 
                    "sampleGear", "halfConeID", "HalfCone", "FinalRun", "lifeStage", "Unassd" )                     
    
      #   ---- Vector indexes = all unique combinations of visit, run, and life stage.
      #   ---- Vector indexes = NA for all gaps in the trapping sequence.
    
      indexes <- tapply( 1:nrow(catch), list( catch$trapVisitID, catch$FinalRun, catch$Unassd ) ) 
      doit <- 1
      
    #   ---- Fish counts of half-cone expansions and other types.
    } else if (variable %in% c('halfConeAssignedCatch','halfConeUnassignedCatch','assignedCatch','unassignedCatch','modAssignedCatch','modUnassignedCatch')){   
    
      #   These are variables that are constant within a trapVisit, run, and lifestage
      const.vars<-c("ProjID", "trapVisitID", "batchDate", "StartTime", "EndTime", "SampleMinutes", 
                    "TrapStatus", "siteID", "siteName", "trapPositionID", "TrapPosition", "sampleGearID", 
                    "sampleGear", "halfConeID", "HalfCone", "FinalRun", "lifeStage", "Unassd" )                     
      
      #   ---- Vector indexes = all unique combinations of visit, run, and life stage.
      #   ---- Vector indexes = NA for all gaps in the trapping sequence.
    
      indexes <- tapply( 1:nrow(catch), list( catch$trapVisitID, catch$FinalRun, catch$lifeStage ) ) 
      doit <- 1
      
      #   ---- The code below expects a variable named Unmarked.  Force this.  
      if(variable == 'halfConeAssignedCatch')  {catch$Unmarked <- catch$halfConeAssignedCatch     
      } else if(variable == 'halfConeUnassignedCatch'){catch$Unmarked <- catch$halfConeUnassignedCatch
      } else if(variable == 'assignedCatch')          {catch$Unmarked <- catch$assignedCatch
      } else if(variable == 'unassignedCatch')        {catch$Unmarked <- catch$unassignedCatch
      } else if(variable == 'modAssignedCatch')       {catch$Unmarked <- catch$modAssignedCatch
      } else if(variable == 'modUnassignedCatch')     {catch$Unmarked <- catch$modUnassignedCatch
      }
    }
  
    if(doit == 1){
      #   ---- Because of the gap lines, there are NAs in indexes (because there are NAs in trapVisitID). 
      #   ---- Fix these.  To assure indexes is same length as catch, we cannot simply remove NAs here. 
      #   ---- Solution: set them to -1, then remove -1 from u.ind.  This way the loop below 
      #   ---- just skips them.
      indexes[ is.na(indexes) ] <- -1

      #   ---- Initialize place to store summarized catches. 
      u.ind <- indexes[!duplicated(indexes)]
      
      #   ---- These are not the right lines; they will be replaced inside the loop.  This just initializes.
      catch.fl <- catch[!duplicated(indexes),const.vars]  
      
      #   ---- Don't want the former NAs, which are now -1s. 
      catch.fl <- catch.fl[ u.ind > 0, ]    
      u.ind <- u.ind[ u.ind > 0 ] 
      
      catch.fl <- cbind( catch.fl, matrix( NA, nrow(catch.fl), 3))
      names( catch.fl ) <- c(const.vars, "n.tot", "mean.fl", "sd.fl")

      #   ---- Count number of fish, compute mean fork length, etc, per visitID in catch.  
      for( i in 1:length(u.ind) ){

        ind <- (u.ind[i] == indexes) & !is.na(indexes)
    
        #   ---- Copy over constant variables
        catch.fl[i,const.vars] <- catch[ind,const.vars][1,]
        
        #   ---- Don't think Unmarked can be missing, but rm just in case.
        catch.fl$n.tot[i] <- sum( catch$Unmarked[ind], na.rm=T ) 

        #   ---- Take weighted averages of fork lengths using 'n' as weights.  
        #   ---- Not sure that catch.fl$n.tot[i] can be missing, but just in case.
        if( !is.na(catch.fl$n.tot[i]) & (catch.fl$n.tot[i] > 0) ){
          
          #   ---- Making a sample.
          fl <- rep(catch$forkLength[ind & !is.na(catch$forkLength)], catch$Unmarked[ind & !is.na(catch$forkLength)])     
          
          #   ---- Could have missing fork length.
          catch.fl$mean.fl[i] <- mean( fl, na.rm=T )   
          catch.fl$sd.fl[i] <- sd( fl , na.rm=T )
        } else {
          catch.fl$mean.fl[i] <- NA
          catch.fl$sd.fl[i] <- NA
        }
      }

      #   ---- Sort the result by trap position and trap visit,
      catch.fl <- catch.fl[ order( catch.fl$trapPositionID, catch.fl$EndTime ), ]
  
      if(variable == 'unassigned'){
        
        #   ---- Deal with data nuance of unassigned fish.  
        catch.fl <- catch.fl[is.nan(catch.fl$mean.fl),] 
      }
    } else {
      
      #   ---- Need to have at least an empty data frame with defined columns
      catch.fl <- data.frame(matrix(vector(), 0, 20,
                             dimnames=list(c(), c("ProjID","trapVisitID","batchDate","StartTime","EndTime","SampleMinutes","TrapStatus","siteID","siteName","trapPositionID","TrapPosition","sampleGearID","sampleGear","halfConeID","HalfCone","FinalRun","lifeStage","n.tot","mean.fl","sd.fl")  )),
                            stringsAsFactors=F)  
  }

  #   ---- We have none of this particular type of fish.  
  } else {
    catch.fl <- NA
  }
  catch.fl
}
