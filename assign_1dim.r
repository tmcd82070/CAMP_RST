F.assign.1dim <- function(catch, present.var, absent.var ){
#
#   Values in one dimension of the basic run X lifstage table is present, the other is missing.
#
#   Assign the missing one based on frequency dist of the other.
#


u <- sort(unique( catch[,present.var] ))
u <- u[ !is.na(u) ]   # NA is included in unique values,  ditch it here
for( i in u ){
    thisPresent <- (catch[,present.var] == i) & !is.na(catch[,present.var])
    cat(paste("--------", present.var, "=", i))

    if( any( is.na(catch[thisPresent,absent.var]))) {
        #   We need to assign a run based on frequencies in this lifestage
        cat("\n")

        cat(paste("Number of total fish before expanding and assigning", absent.var , "= "))
        cat(sum(catch$n[thisPresent]))
        cat("\n")

        #   For every missing absent.var, expand the line into length(freqs) lines with appropriate counts
        thisPresentAndMissing <- thisPresent & is.na(catch[,absent.var])
        repeat{
            if( sum( thisPresentAndMissing ) == 0 ) break
            j <- which(thisPresentAndMissing)[1]

#            print(c(j, nrow(catch)))
#            print(catch[j,])

            #   Get the frequency distribution for the absent variable on this day.  Use that to inflate plus counts.
            thisTrapVisit <- catch$trapVisitID[j]
#           print( catch[ catch$trapVisitID == thisTrapVisit, ] )
            freqs <- tapply( catch$n[ catch$trapVisitID == thisTrapVisit ], catch[ catch$trapVisitID == thisTrapVisit, absent.var ], sum)

            if(length(freqs) > 0){
                #   There were other fish captured this day that had a "absent" designation. Use them.
                props <- freqs / sum(freqs)
    
#                cat(paste("Proportions of", absent.var, " (when recorded) on", catch$visitTime[j],  ":\n"))
#                print(props)

                #   Multiply proportions that day by number of missings - the 'plus' count
                N.j <- catch$n[j]
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
                lines.j$n <- n.j
                lines.j[,absent.var] <- names(props)
    
                if( j == 1 ){
                    catch <- rbind( lines.j, catch[(j+1):nrow(catch),] )
                } else if (j == nrow(catch)){
                    catch <- rbind( catch[1:(j-1),], lines.j )
                } else {
                    catch <- rbind( catch[1:(j-1),], lines.j, catch[(j+1):nrow(catch),] )
                }                                

                thisPresentAndMissing <- (catch[,present.var] == i) & !is.na(catch[,present.var]) & is.na(catch[absent.var])
                thisPresentAndMissing[ 1:(j+length(props)-1) ] <- FALSE  # must wipe out lines we have already processed because its possible for a missing to remain.  If don't do this, we can have an infinite loop.

            } else {
                #   There were no other fish caught that day with some value for the "absent" variable.
                #   Leave 'absent' blank.  NOTE: this means some missings in 'absent' may remain in the data set after this routine.
                #   It might be possible to look at previous and subsequent trap visits to assign a value to 'absent', but that would need to be
                #   done by hand. Write out a note to this effect. 
                cat(paste("NOTE: trapVisitID=", catch$trapVisitID[j], "has no", absent.var, "on", catch$visitTime[j],  
                    "but no non-missing", absent.var, "found. Fix this manually.\n"))

                #   I only need to update thisPresentAndMissing[j] to FALSE because nothing has happened, but just for good measure, I'll re-compute
                thisPresentAndMissing <- (catch[,present.var] == i) & !is.na(catch[,present.var]) & is.na(catch[absent.var])
                thisPresentAndMissing[ 1:j ] <- FALSE  # must wipe out lines we have already processed because its possible for a missing to remain.  If don't do this, we can have an infinite loop.

            }
            
#            print(catch[j:(j+length(props)-1),])
#            cat("hit return...")
#            if (i == 3 ) readline()


        }
        
        cat(paste("Number of total fish after expanding and assigning", absent.var , " (should match count before expansion)= "))
        cat(sum(catch$n[(catch[,present.var] == i) & !is.na(catch[,present.var])]))
        cat("\n")
        
    } else {
        cat(paste( " No Missing", absent.var, "\n"))
    }
}

#   Finally, it is possible that we added some rows with n=0.  Remove them.
catch <- catch[ catch$n > 0, ]


catch

}
