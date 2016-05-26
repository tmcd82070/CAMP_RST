#' @export
#' 
#' @title F.assign.1dim 
#' 
#' @description Assign the missing counts of either \code{FinalRun} or \code{lifeStage},
#' based on frequency dist of the other.
#' 
#' @param catch A data frame containing records of fish itemized by combinations
#'   of variables \code{FinalRun}, \code{lifeStage}, and \code{forkLength}. 
#'   Variable \code{Unmarked} contains the number of fish represented in each 
#'   record.
#' @param present.var The variable in data frame \code{catch} for which data are
#'   recorded.  This should be one of \code{FinalRun} or \code{lifeStage}.
#' @param absent.var The variable in the data frame \code{catch} for which data 
#'   are not recorded.  This should be either of \code{FinalRun} or 
#'   \code{lifeStage} not utilized in \code{present.var}.
#'   
#' @return A data frame \code{catch} with any \code{"Unassigned"} counts of fish
#'   present in the \code{absent.var} variable proportionally allocated to the 
#'   levels present in the \code{present.var} variable.  See 'Details.'
#'   
#' @details Values in one dimension of the basic \code{FinalRun} X 
#'   \code{lifeStage} table is present, but the other is missing.
#'   
#'   Except in rare cases, only fish for which variable \code{RandomSelection = 
#'   "Yes"} contribute to sampling proportions.  Thus, fish for which either or 
#'   both of \code{FinalRun} and \code{lifeStage} are recorded, but for which 
#'   \code{RandomSelection = "No"}, are excluded.
#'   
#'   Function \code{F.assign.1dim} loops individually over each level found in 
#'   the \code{present.var} variable. Within each level, trap occasions, as 
#'   determined by variable \code{trapVisitID}, are examined one-by-one in order
#'   to estimate relative frequencies of levels in the \code{absent.var} 
#'   variable, following the exclusion of those recorded as \code{"Unassigned"}.
#'   In this way, an estimate of the relative frequencies of the 
#'   \code{absent.var}, given the level of the \code{present.var} and the 
#'   provided \code{trapVisitID}, is obtained.
#'   
#'   Estimation of the underlying frequency distribution in a \code{trapVisitID}
#'   may take place by up to six sequential strategies.  The application of a 
#'   higher strategy only takes place on failure of the one immediately 
#'   preceding it.  The six strategies follow.  In most cases, the first 
#'   applies, and so the others are not considered.  In each, the overall 
#'   relative frequency of the \code{absent.var} is obtained via examination 
#'   of...
#'   
#'   \enumerate{ \item ...the levels of \code{absent.var} with the same
#'   \code{trapVisitID} and level of \code{present.var}, where
#'   \code{RandomSelection = 'yes'}. \item ...the levels of \code{absent.var}
#'   with the same \code{trapVisitID} and level of \code{present.var}. \item
#'   ...the levels of \code{absent.var} with the same \code{SampleDate} and
#'   level of \code{present.var}, where \code{RandomSelection = 'yes'}. \item
#'   ...the levels of \code{absent.var} with the same \code{SampleDate} and
#'   level of \code{present.var}. \item ...the levels of \code{absent.var} with
#'   the same \code{SampleDate}, plus or minus one day, and level of
#'   \code{present.var}, where \code{RandomSelection = 'yes'}. \item ...the
#'   levels of \code{absent.var} with the same \code{SampleDate}, plus or minus
#'   one day, and level of \code{present.var}. }
#'   
#'   Relaxation of the randomly selected condition via the lack of consideration
#'   of variable \code{RandomSelection} in strategies 2, 4, and 6 takes place 
#'   only within a \code{trapVisitID} for which the preceding strategy contained
#'   no \code{RandomSelection = 'yes'} records.  The relaxation allows for the 
#'   possibility that with a small number of caught fish, all are measured.  In 
#'   this case, the entire catch forms the sample and sometimes, variable 
#'   \code{RandomSelection} is set to \code{"no"}.
#'   
#'   When one of strategies 3, 4, 5, 6 is used, the resulting relative frequency
#'   of counts found in other \code{trapVisitID}s or neighboring days may not
#'   evenly divide the \code{"Unassigned"} count of fish recorded in the 
#'   \code{absent.var} variable. In this case, the plus counts are obtained by 
#'   multiplying the number of \code{"Unassigned"} fish by the proportion of 
#'   fish in each level returned by application of the suitable strategy. When 
#'   this occurs, round-off error may occur and accumulate over the various 
#'   levels for which plus-counts are obtained.  The difference between the 
#'   original \code{"Unassigned"} count of fish and the sum of the rounded plus 
#'   counts thus may not equal zero.  Any non-zero "magic fish" are randomly 
#'   allocated to the levels for which rounding occurred via the \code{multinom}
#'   function.  In this way, the data dictate to which levels any magic fish are
#'   allocated.  Magic fish may be positive or negative quantities, with each 
#'   being allocated in the appropriate way to ensure that the sum of the final 
#'   plus-count levels equals the original \code{"Unassigned"} count of fish.
#'   
#' @examples
#' 
F.assign.1dim <- function(catch, present.var, absent.var ){

  # catch <- catch
  # present.var <- "FinalRun"
  # absent.var <- "lifeStage"

  #   ---- Find the levels of the present variable.  
  u <- levels( catch[,present.var] )
  u <- u[ u!="Unassigned" ]

  nonMissTrapVisit <- !is.na(catch$trapVisitID)
  nonMissAbsent <- !is.na(catch[,absent.var])
  RandomlySelected <- (catch$RandomSelection == "Yes") & !(is.na(catch$RandomSelection))
  UnassignedAbsent <- (catch[,absent.var]=="Unassigned") & nonMissAbsent

  #  ---- Loop over each level found in the present variable.  
  for( i in u ){
    thisPresent <- (catch[,present.var] == i) & !is.na(catch[,present.var])
    cat(paste("--------", present.var, "=", i))

    #   ---- See if any of this level are present, but unassigned in the other
    #   ---- variable. If none, just skip to the end -- there's nothing to do.  
    if(any(UnassignedAbsent & thisPresent)) {
      
      #   ---- We need to assign a run based on frequencies in the present var.  
      cat("\n")
      cat(paste("Number of total fish before expanding and assigning", absent.var , "= "))
      cat(sum(catch$Unmarked[thisPresent]))
      cat("\n")

      #   ---- For every missing absent.var, expand the line into length(freqs)
      #   ---- lines with appropriate counts.
      thisPresentAndMissing <- thisPresent & UnassignedAbsent
      repeat{
        
        #   ---- If this is TRUE, we've plus-counted out all the necessary fish.  
        if( sum( thisPresentAndMissing ) == 0 ) break
        
          #   ---- Find the row where we need to plus-count out.
          j <- which(thisPresentAndMissing)[1]
          
          #   ---- Get the frequency dist for the absent variable on this day.  Use that to inflate plus counts.
          #   ---- Because one catch is broken out by forkLength, the freq dist for this day (trapPositionID really)
          #   ---- usually spans more than one record.  
          #   ---- Note: must compute these indicators in the loop because catch is changing length.
          thisTrapVisit <- (catch$trapVisitID[j] == catch$trapVisitID) & !is.na(catch$trapVisitID)
          RandomlySelected <- (catch$RandomSelection == "Yes") & !(is.na(catch$RandomSelection))
          UnassignedAbsent <- (catch[,absent.var]=="Unassigned") & !is.na(catch[,absent.var])
           
          #   ---- Form indicator of this trap visit, level, absent-var not "Unassigned", randomly selected.
          thisInd <- thisTrapVisit & thisPresent & !UnassignedAbsent & RandomlySelected 
                       
          if( !any( thisInd ) ){

            #   ---- If we are here, there are no fish to compute frequencies.  The if condition = TRUE.
            #   ---- Check to see if there are any non-RandomlySelected fish to use. 
            #   ---- The way they coded RandomSelection, check first to see if any randomly selected fish 
            #   ---- come from the "grab sample."  If not, they may have measured all fish, and not taken a grab sample. 
            #   ---- In this case, they set RandomSelection to "no".  This means we must check for non-random selection
            #   ---- fish after determining there are no randomly selected ones. 
            #   ---- So, relax the randomly assignment condition.  
            thisInd <- thisTrapVisit & thisPresent & !UnassignedAbsent

            if( !any(thisInd) ){

              #   ---- Still no fish. Expand out to see if there are randomly selected               
              #   ---- fish with absentvar defined from other trap visits on same day.
              thisDay <- (format(catch$SampleDate[j],"%Y-%m-%d") == format(catch$SampleDate,"%Y-%m-%d")) & !is.na(catch$SampleDate)
              thisInd <- thisDay & thisPresent & !UnassignedAbsent & RandomlySelected 
                    
              if( !any( thisInd ) ){
                
                #   ---- Still no fish.  
                #   ---- See if any non-randomly selected fish on same day.  
                thisInd <- thisDay & thisPresent & !UnassignedAbsent 

                if( !any( thisInd ) ){
                  
                  #   ---- Still no fish.
                  #   ---- Expand window out +- 24 hours (one day) around date of target trapVisit day.
                  day.low  <- catch$SampleDate[j] - 24*60*60
                  day.high <- catch$SampleDate[j] + 24*60*60
                  theseDays <- (day.low <= catch$SampleDate) & (catch$SampleDate <= day.high) & !is.na(catch$SampleDate)
                  thisInd <- theseDays & thisPresent & !UnassignedAbsent & RandomlySelected 
                            
                  if( !any( thisInd ) ){
                      
                    #   ---- Still no fish.  Try one more time.
                    #   ---- Look for non-randomly sampled fish +- 24 hours. 
                    #   ---- If still no fish, give up.
                    thisInd <- theseDays & thisPresent & !UnassignedAbsent 

                    #   ---- If no fish in thisInd at this point, we give up.  Ask user to fix manually. 
                  }
                }  
              } 
            }
          }
                    
          #   ---- Sum the freqs for this trap visit, level, absent-var not "Unassigned", randomly selected,
          #   ---- over individual levels in the absent.var.  
          freqs <- tapply( catch$Unmarked[ thisInd ], catch[ thisInd, absent.var ], sum)
          freqs <- freqs[ !is.na(freqs) ]

          #   ---- If greater than zero, there were other fish captured this day that had an "absent" 
          #   ---- designation. Use them as an estimate for the "Unassigned."
          if(length(freqs) > 0){
              
            props <- freqs / sum(freqs)

            #   ---- Multiply proportions that day by number of missings - the 'plus' count.  
            N.j <- catch$Unmarked[j]
            n.j <- c(round( props * N.j ))
                
            #   ---- Make sure the plus counts sum to the original. They won't always due to rounding.  
            #   ---- This fixes the rounding error.
            N.more <- N.j - sum(n.j)

            #   ---- Randomly allocate the rounding error to classes.
            if( N.more > 0 ){
              set.seed(884)
              n.extra <- c(rmultinom( 1, N.more, props ))
              n.j <- n.j + n.extra   
            } else if( N.more < 0 ){
              set.seed(884)
              n.extra <- c(rmultinom( 1, abs(N.more), props ))
              n.j <- n.j - n.extra                
            }
            
            #   ---- Replace line j with length(props) lines.  These new lines have $n equal to n.j, 
            #   ---- but all other variables equal to the original line.
            lines.j <- catch[rep(j,length(props)),]
            lines.j$Unmarked <- n.j
            lines.j[,absent.var] <- names(props)
    
            if( j == 1 ){
              catch <- rbind( lines.j, catch[(j+1):nrow(catch),] )
            } else if (j == nrow(catch)){
              catch <- rbind( catch[1:(j-1),], lines.j )
            } else {
              catch <- rbind( catch[1:(j-1),], lines.j, catch[(j+1):nrow(catch),] )
            }                                

            #   ---- In prep for next loop.  
            #   ---- We must recompute UnassignedAbsent and thisPresentAndMissing because we just changed the
            #   ---- number of rows in catch.  Thus, these vectors grow in length over loops.  
            UnassignedAbsent <- (catch[,absent.var]=="Unassigned") & !is.na(catch[,absent.var])
            thisPresent <- (catch[,present.var] == i) & !is.na(catch[,present.var])
            thisPresentAndMissing <- thisPresent & UnassignedAbsent
                
            #   ---- Must wipe out lines already processed.  Possible for a missing to remain.  
            #   ---- If don't do this, can have an infinite loop.
            thisPresentAndMissing[ 1:(j+length(props)-1) ] <- FALSE 

          } else {
              
            #   ---- There were no other fish caught that day with some value for the 'absent' variable.
            #   ---- Leave 'absent' blank.  NOTE: this means some missings in 'absent' may remain in the data set after this routine.
            #   ---- It might be possible to look at previous and subsequent trap visits to assign a value to 'absent', but that
            #   ----  would need to be done by hand. Write out a note to this effect. 
            cat(paste("NOTE: trapVisitID=", catch$trapVisitID[j], "has remaining Unassigned", absent.var, "on", catch$SampleDate[j],  
              "To fix, assign at least one", absent.var, "during this trapVisit.\n"))

            #   ---- I only need to update thisPresentAndMissing[j] to FALSE because nothing 
            #   ---- has happened, but just for good measure, re-compute.
            UnassignedAbsent <- (catch[,absent.var]=="Unassigned") & !is.na(catch[,absent.var])
            thisPresent <- (catch[,present.var] == i) & !is.na(catch[,present.var])
            thisPresentAndMissing <- thisPresent & UnassignedAbsent
                
            #   ---- Must wipe out lines already processed.  Possible for a missing to remain.  
            #   ---- If don't do this, can have an infinite loop.
            thisPresentAndMissing[ 1:j ] <- FALSE 
          }
        #   ---- Note no closing brace here for if above with the break. 
      }
      cat(paste("Number of fish after expanding and assigning", absent.var , " (should match count before expansion)= "))
      cat(sum(catch$Unmarked[(catch[,present.var] == i) & !is.na(catch[,present.var])]))
      cat("\n")
    } else {
        cat(paste( " No Unassigned", absent.var, "\n"))
    }
  }

  #   --- Finally, it is possible that we added some rows with n=0.  Remove them.
  catch <- catch[ (catch$Unmarked > 0) | is.na(catch$Unmarked), ]

  catch
}
