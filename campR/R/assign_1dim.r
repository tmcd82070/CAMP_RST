#' @export F.assign.1dim
#' 
#' @title F.assign.1dim
#' 
#' @description
#' 
#'    Values in one dimension of the basic run X lifstage table is present, the other is missing.
#' 
#'    Assign the missing one based on frequency dist of the other.
#' 
#' 
#'    We make sure to TOSS RANDOMSELECTION == "NO"
#' 
#' 
#' @param catch <describe argument>
#' @param  present.var <describe argument>
#' @param  absent.var  <describe argument>
#' 
#' @details <other comments found in file>
#'             print(c(j, nrow(catch)))
#'             print(catch[j,])
#'    Get the frequency distribution for the absent variable on this day.  Use that to inflate plus counts.
#'    Note: must compute these indicators in the loop because catch is changing length
#'    If we are here, there are no fish to compute frequencies.  
#'    Check to see if there are any non-RandomlySelected fish to use. 
#'    The way they coded Randomly selected, we first check to see if there are any randomly selected fish 
#'    which would come from the "grab sample".  If not, they may have measured all fish, and not taken a grab sample. 
#'    In this case, they set RandomSelection to "no".  This means we must check for non-random selection fish after 
#'    determining there are no randomly selected ones. 
#'    Still no fish                
#'    Expand out to and see if there are randomly selected fish with absentvar defined from other trap visits on same day
#'    Still no fish
#'    See if any non-randomly selected fish on same day
#'    Still no fish
#'    Expand window out +- 24 hours around date of target trapVisit
#'    Still no fish, try one more time.
#'    Look for non-randomly sampled fish +- 24 hours. 
#'    If still no fish, give up.
#'    If there are no fish in thisInd at this point, we give up and will ask the user to fix this manually. 
#'    There were other fish captured this day that had a "absent" designation. Use them.
#'                 cat(paste("Proportions of", absent.var, " (when recorded) on", catch$visitTime[j],  ":\n"))
#'                 print(props)
#'    Multiply proportions that day by number of missings - the 'plus' count
#'    Make sure the plus counts sum to the original. They won't always due to rounding.  This fixes the rounding error.
#'                 cat("-----------------------------\n")
#'                 print( N.j )
#'                 print( n.j )
#'                 print( N.more )
#'                 print( props )
#'    Replace line j with length(props) lines.  These new lines have $n equal to n.j, but all other variables equal to the original line
#'    In prep for next loop.  
#'    We must recompute UnassignedAbsent and thisPresentAndMissing because we just change the number of rows in catch.
#'    Thus, these vectors grow in length over loops
#'    There were no other fish caught that day with some value for the "absent" variable.
#'    Leave 'absent' blank.  NOTE: this means some missings in 'absent' may remain in the data set after this routine.
#'    It might be possible to look at previous and subsequent trap visits to assign a value to 'absent', but that would need to be
#'    done by hand. Write out a note to this effect. 
#'    I only need to update thisPresentAndMissing[j] to FALSE because nothing has happened, but just for good measure, I'll re-compute
#'             print(catch[j:(j+length(props)-1),])
#'             cat("hit return...")
#'             readline()
#'    Finally, it is possible that we added some rows with n=0.  Remove them.
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
F.assign.1dim <- function(catch, present.var, absent.var ){
#
#   Values in one dimension of the basic run X lifstage table is present, the other is missing.
#
#   Assign the missing one based on frequency dist of the other.
#

#   We make sure to TOSS RANDOMSELECTION == "NO"

u <- levels( catch[,present.var] )
u <- u[ u!="Unassigned" ]

nonMissTrapVisit <- !is.na(catch$trapVisitID)
nonMissAbsent <- !is.na(catch[,absent.var])
RandomlySelected <- (catch$RandomSelection == "Yes") & !(is.na(catch$RandomSelection))
UnassignedAbsent <- (catch[,absent.var]=="Unassigned") & nonMissAbsent

for( i in u ){
    thisPresent <- (catch[,present.var] == i) & !is.na(catch[,present.var])
    cat(paste("--------", present.var, "=", i))

    if(any(UnassignedAbsent & thisPresent)) {
        #   We need to assign a run based on frequencies in this lifestage
        cat("\n")

        cat(paste("Number of total fish before expanding and assigning", absent.var , "= "))
        cat(sum(catch$Unmarked[thisPresent]))
        cat("\n")

        #   For every missing absent.var, expand the line into length(freqs) lines with appropriate counts
        thisPresentAndMissing <- thisPresent & UnassignedAbsent
        repeat{
            if( sum( thisPresentAndMissing ) == 0 ) break
            j <- which(thisPresentAndMissing)[1]

#            print(c(j, nrow(catch)))
#            print(catch[j,])

            #   Get the frequency distribution for the absent variable on this day.  Use that to inflate plus counts.
            #   Note: must compute these indicators in the loop because catch is changing length
            thisTrapVisit <- (catch$trapVisitID[j] == catch$trapVisitID) & !is.na(catch$trapVisitID)
            RandomlySelected <- (catch$RandomSelection == "Yes") & !(is.na(catch$RandomSelection))
            UnassignedAbsent <- (catch[,absent.var]=="Unassigned") & !is.na(catch[,absent.var])
           
            thisInd <- thisTrapVisit & thisPresent & !UnassignedAbsent & RandomlySelected 
                       
            
            if( !any( thisInd ) ){
                #   If we are here, there are no fish to compute frequencies.  
                #   Check to see if there are any non-RandomlySelected fish to use. 
                #   The way they coded Randomly selected, we first check to see if there are any randomly selected fish 
                #   which would come from the "grab sample".  If not, they may have measured all fish, and not taken a grab sample. 
                #   In this case, they set RandomSelection to "no".  This means we must check for non-random selection fish after 
                #   determining there are no randomly selected ones. 

                thisInd <- thisTrapVisit & thisPresent & !UnassignedAbsent
                
                if( !any(thisInd) ){

                    #   Still no fish                
                    #   Expand out to and see if there are randomly selected fish with absentvar defined from other trap visits on same day
                    thisDay <- (format(catch$SampleDate[j],"%Y-%m-%d") == format(catch$SampleDate,"%Y-%m-%d")) & !is.na(catch$SampleDate)
                    thisInd <- thisDay & thisPresent & !UnassignedAbsent & RandomlySelected 
                    
                    if( !any( thisInd ) ){
                        #   Still no fish
                        #   See if any non-randomly selected fish on same day
                        thisInd <- thisDay & thisPresent & !UnassignedAbsent 
                        
                        if( !any( thisInd )){
                            #   Still no fish
                            #   Expand window out +- 24 hours around date of target trapVisit
                            day.low  <- catch$SampleDate[j] - 24*60*60
                            day.high <- catch$SampleDate[j] + 24*60*60
                            theseDays <- (day.low <= catch$SampleDate) & (catch$SampleDate <= day.high) & !is.na(catch$SampleDate)
                            thisInd <- theseDays & thisPresent & !UnassignedAbsent & RandomlySelected 
                            
                            if( !any( thisInd )){
                                #   Still no fish, try one more time.
                                #   Look for non-randomly sampled fish +- 24 hours. 
                                #   If still no fish, give up.
                                thisInd <- theseDays & thisPresent & !UnassignedAbsent 
                            }
                        }
                    }
                }
            }
            #   If there are no fish in thisInd at this point, we give up and will ask the user to fix this manually. 
                    
            
            freqs <- tapply( catch$Unmarked[ thisInd ], catch[ thisInd, absent.var ], sum)
            freqs <- freqs[ !is.na(freqs) ]

            if(length(freqs) > 0){
                #   There were other fish captured this day that had a "absent" designation. Use them.
                props <- freqs / sum(freqs)
    
#                cat(paste("Proportions of", absent.var, " (when recorded) on", catch$visitTime[j],  ":\n"))
#                print(props)

                #   Multiply proportions that day by number of missings - the 'plus' count
                N.j <- catch$Unmarked[j]
                n.j <- c(round( props * N.j ))
    
                
                #   Make sure the plus counts sum to the original. They won't always due to rounding.  This fixes the rounding error.
                N.more <- N.j - sum(n.j)

#                cat("-----------------------------\n")
#                print( N.j )
#                print( n.j )
#                print( N.more )
#                print( props )

                if( N.more > 0 ){
                    n.extra <- c(rmultinom( 1, N.more, props ))
                    n.j <- n.j + n.extra   # Randomly allocate the rounding error to classes
                }

                #   Replace line j with length(props) lines.  These new lines have $n equal to n.j, but all other variables equal to the original line
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

                #   In prep for next loop.  
                #   We must recompute UnassignedAbsent and thisPresentAndMissing because we just change the number of rows in catch.
                #   Thus, these vectors grow in length over loops
                UnassignedAbsent <- (catch[,absent.var]=="Unassigned") & !is.na(catch[,absent.var])
                thisPresent <- (catch[,present.var] == i) & !is.na(catch[,present.var])
                thisPresentAndMissing <- thisPresent & UnassignedAbsent
                thisPresentAndMissing[ 1:(j+length(props)-1) ] <- FALSE  # must wipe out lines we have already processed because its possible for a missing to remain.  If don't do this, we can have an infinite loop.

            } else {
                #   There were no other fish caught that day with some value for the "absent" variable.
                #   Leave 'absent' blank.  NOTE: this means some missings in 'absent' may remain in the data set after this routine.
                #   It might be possible to look at previous and subsequent trap visits to assign a value to 'absent', but that would need to be
                #   done by hand. Write out a note to this effect. 
                cat(paste("NOTE: trapVisitID=", catch$trapVisitID[j], "has remaining Unassigned", absent.var, "on", catch$SampleDate[j],  
                    "To fix, assign at least one", absent.var, "during this trapVisit.\n"))

                #   I only need to update thisPresentAndMissing[j] to FALSE because nothing has happened, but just for good measure, I'll re-compute
                UnassignedAbsent <- (catch[,absent.var]=="Unassigned") & !is.na(catch[,absent.var])
                thisPresent <- (catch[,present.var] == i) & !is.na(catch[,present.var])
                thisPresentAndMissing <- thisPresent & UnassignedAbsent
                thisPresentAndMissing[ 1:j ] <- FALSE  # must wipe out lines we have already processed because its possible for a missing to remain.  If don't do this, we can have an infinite loop.

            }
            
#            print(catch[j:(j+length(props)-1),])
#            cat("hit return...")
#            readline()


        }
        
        cat(paste("Number of fish after expanding and assigning", absent.var , " (should match count before expansion)= "))
        cat(sum(catch$Unmarked[(catch[,present.var] == i) & !is.na(catch[,present.var])]))
        cat("\n")
        
    } else {
        cat(paste( " No Unassigned", absent.var, "\n"))
    }
}

#   Finally, it is possible that we added some rows with n=0.  Remove them.
catch <- catch[ (catch$Unmarked > 0) | is.na(catch$Unmarked), ]


catch

}
