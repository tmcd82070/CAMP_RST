F.assign.2dim <- function(catch, var1, var2 ){
#
#   Values are missing in two dimensions.  I.e., var1 and var2 are missing, but n>0.
#
#   Assign a var1 and var2 value based on joint distribution
#

    if( sum( catch$unmarked[is.na(catch[,var1]) & is.na(catch[,var2])] ) == 0 ){
        #   There is nothing to do.  Either no lines with missing var1 and var2, or n=0 for all them.
        #   n=0 should not happen if n=0 lines are taken out before here
        cat(paste("-------- No missing combinations of", var1, "and", var2, "found.\n"))
        return( catch )
    }


    #   Otherwise, we need to assign a run and lifestage based on frequencies found the same day

    cat(paste("Number of fish before expanding and assigning", var1 , "and", var2, " ="))
    cat(sum(catch$unmarked))
    cat("\n")

    #   For every line with missing var1 and var2, make sure n>0, then expand the line into length(freqs) lines with appropriate counts
    MissingCombos <- is.na(catch[,var1]) & is.na(catch[,var2])
    repeat{
        if( sum( MissingCombos ) == 0 ) break
        j <- which(MissingCombos)[1]

        if( catch$unmarked[j] > 0 ){
            #   This is a plus count that needs expanding.

#            print(catch[j,])

            #   Get the frequency distribution for the absent variable on this day.  Use that to inflate plus counts.
            thisTrapVisit <- catch$trapVisitID[j]
#            print( catch[ catch$trapVisitID == thisTrapVisit, ] )
            freqs <- tapply( catch$unmarked[ catch$trapVisitID == thisTrapVisit ], 
                list(var1=catch[ catch$trapVisitID == thisTrapVisit, var1 ], var2=catch[ catch$trapVisitID == thisTrapVisit, var2 ]), sum, na.rm=TRUE)

            if(length(freqs) > 0){
                #   There were other fish captured this day that had a "var1 X var2" designation. Use them.
                #print( cbind( catch$unmarked[ catch$trapVisitID == thisTrapVisit ], catch[ catch$trapVisitID == thisTrapVisit, var1 ], catch[ catch$trapVisitID == thisTrapVisit, var2 ]) )
                #print(freqs)

                props <- freqs / sum(freqs, na.rm=T)
                #print(props)
                
                props <- data.frame( expand.grid(dimnames(props)[[1]], dimnames(props)[[2]]), props=c(props))
                names(props) <- c(var1, var2, "props")
                props <- props[ !is.na(props$props), ]
    
    
                #   Multiply proportions that day by number of missings - the 'plus' count
                N.j <- catch$unmarked[j]
                n.j <- round( props$props * N.j )

#                cat(paste("Proportions (when recorded) on", catch$visitTime[j],  ":\n"))
#                print(cbind(props, n.j))

    
                #   Make sure the plus counts sum to the original. They won't always due to rounding.  This fixes the rounding error.
                N.more <- N.j - sum(n.j)
                if( N.more > 0 ){
                    n.extra <- c(rmultinom( 1, N.more, props$props ))
                
                    n.j <- n.j + rmultinom( 1, N.more, props$props )   # Randomly allocate the rounding error to classes
#                    cat("inside N.more....hit return...")
#                    readline()
                }
    
                #   Replace line j with length(props) lines.  These new lines have $unmarked equal to n.j, but all other variables equal to the original line
                lines.j <- catch[rep(j,nrow(props)),]
                lines.j$unmarked <- n.j
                lines.j[,var1] <- props[,var1]
                lines.j[,var2] <- props[,var2]
                
#                cat("return....")
#                readline()
                
    
                if( j == 1 ){
                    catch <- rbind( lines.j, catch[(j+1):nrow(catch),] )
                } else if (j == nrow(catch)){
                    catch <- rbind( catch[1:(j-1),], lines.j )
                } else {
                    catch <- rbind( catch[1:(j-1),], lines.j, catch[(j+1):nrow(catch),] )
                }  
                
                MissingCombos <- is.na(catch[,var1]) & is.na(catch[,var2])
                MissingCombos[ 1:(j+nrow(props)-1) ] <- FALSE  # must wipe out lines we have already processed because its possible for a missing to 
                                                                 # remain.  If don't do this, we can get into an infinite loop.

            } else {
                #   There were no other fish caught that day with values in the var1 X var2 table.
                #   Leave the line blank.  
                cat(paste("NOTE: trapVisitID=", catch$trapVisitID[j], "has no", var1, "and", var2, "on", catch$visitTime[j],  
                    "but no non-missing other values. Fix this manually.\n"))
                    
                #   I only need to update MissingCombos[j] to FALSE because nothing has happened, but just for good measure, I'll re-compute
                MissingCombos <- is.na(catch[,var1]) & is.na(catch[,var2])
                MissingCombos[ 1:j ] <- FALSE  
                    
                
            }

        }
#        print(catch[j:(j+nrow(props)-1),])
#        cat("hit return...")
#        readline()


    }

    cat(paste("Number of fish after expanding and assigning", var1 , "and", var2, " (should match count before expansion)= "))
    cat(sum(catch$unmarked))
    cat("\n")



    #   Finally, it is possible that we added some rows with n=0.  Remove them.
    catch <- catch[ catch$unmarked > 0, ]


    catch

}
