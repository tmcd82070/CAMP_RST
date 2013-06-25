F.summarize.fish.visit <- function( catch ){
#
#   Summarize fish count and fork lengths over visits.
#
#   catch = data frame containing one line per fish or fish group with identical lengths.
#       If they did not catch any member of a particular taxon, catch will have 0 rows.  This 
#       data frame will usually be the output of F.get.indiv.fish.data()
#
#   Output = a data frame with one line per visit, with catch summarized.
#

if( interactive() ) {cat("\tChecking for ad fin clips, summizing visits, computing mean fork lengths, etc ...\n")}

if( nrow(catch) > 0 ){

    #   Count number of fish, compute mean fork length, etc per visitID in catch.  Separate by adfin clipped or not.

    #clipped <- (catch$markTypeID == 2) & (catch$markPositionID == 1)
    clipped <- rep( F, nrow(catch) )
    morts   <- (catch$mortID == Yes.code)

    u.visits <- unique(catch$trapVisitID)
    null <- rep(NA, length(u.visits))
    catch.fl <- data.frame( trapVisitID=null,
                            n.tot=null, n.hatchery=null, n.wild=null,
                            mean.fl=null, mean.fl.hatchery=null, mean.fl.wild=null,
                            sd.fl=null, sd.fl.hatchery=null, sd.fl.wild=null)

    for( i in 1:length(u.visits) ){
        ind <- u.visits[i] == catch$trapVisitID

        catch.fl$trapVisitID[i] <- u.visits[i]

        catch.fl$n.tot[i]       <- sum( catch$n[ind] )
        catch.fl$n.hatchery[i]  <- ifelse( any(ind &  clipped & !morts), sum( catch$n[ind &  clipped & !morts] ), 0)
        catch.fl$n.wild[i]      <- ifelse( any(ind & !clipped & !morts), sum( catch$n[ind & !clipped & !morts] ), 0)
        catch.fl$n.morts[i]     <- ifelse( any(ind & morts), sum( catch$n[ind & morts] ), 0)

        #   Note:  n.tot should equal n.adclip + n.nonadclip + n.morts.

        #   Take weighted averages of fork lengths using 'n' as weights
        if( !is.na(catch.fl$n.tot[i]) & (catch.fl$n.tot[i] > 0) ){ # I don't actually know whether catch.fl$n.tot[i] can be missing, but just in case
            fl <- rep(catch$forkLength[ind], catch$n[ind])  
            catch.fl$mean.fl[i]           <- mean( fl, na.rm=T )   # could have missing fork length
            catch.fl$sd.fl[i]             <- sd( fl , na.rm=T )
        } else {
            catch.fl$mean.fl[i] <- NA
            catch.fl$sd.fl[i] <- NA
        }

        if( catch.fl$n.hatchery[i] > 0 ){
            fl <- rep(catch$forkLength[ind & clipped], catch$n[ind & clipped])
            catch.fl$mean.fl.hatchery[i]    <- mean( fl, na.rm=T )
            catch.fl$sd.fl.hatchery[i]      <- sd( fl, na.rm=T )
        } else {
            catch.fl$mean.fl.hatchery[i] <- NA
            catch.fl$sd.fl.hatchery[i]   <- NA
        }


        if( catch.fl$n.wild[i] > 0 ){
            fl <- rep(catch$forkLength[ind & !clipped], catch$n[ind & !clipped])
            catch.fl$mean.fl.wild[i]    <- mean( fl, na.rm=T )
            catch.fl$sd.fl.wild[i]      <- sd( fl, na.rm=T )
        } else {
            catch.fl$mean.fl.wild[i] <- NA
            catch.fl$sd.fl.wild[i]   <- NA
        }


    }
} 

catch.fl

}
