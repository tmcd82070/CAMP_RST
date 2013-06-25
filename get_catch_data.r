F.get.catch.data <- function( site, taxon, run, min.date, max.date ){
#
#   Fetch the catch data for a SINGLE TAXON from an Access data base. Do some initial
#   computations, like dates.
#
#   input:
#   db = full path and name of the Access data base to retrieve data from
#   tables = vector with named components containing names
#           of the table in db to pull values from
#   site = site ID of place we want to do estimates for. 
#   taxon = the taxon number(s) (from luTaxon) to retrieve.  If a scalar, only 
#       one taxon is retrieved.  If vector of taxon id's, the sum of all 
#       taxons is retrieved.
#   run = the single run ID of the fish we want.  If run = NA, all records for the fish
#       will be pulled. 
#   min.date = minimum date for data to include. This is a text string in the format %Y-%m-%d, or YYYY-MM-DD
#   max.date = maximum date for data to include.  Same format as min.date
#
#   To be included in the catch data, a record has to be from the site, 
#   of the correct taxon, of the correct run, and between min and max dates. 
#

f.banner <- function( x ){

    cat("\n")
    cat(paste(rep("=",50), collapse="")); 
    cat(x); 
    cat(paste(rep("=",50), collapse="")); 
    cat("\n")
}


#   ================================================================================================
#   Fetch the visit table. 
f.banner("F.get.catch.data - Retrieving Visits")

visit <- F.get.indiv.visit.data( site, run, min.date, max.date )


#write.table(visit, file="Visits_test.csv", sep=",", row.names=F )

#print(attributes(visit))

#   Save some attributes for later
site.name <- attr(visit, "site.name") 
site.abbr <- attr(visit, "site.abbr")
run.name <- attr(visit, "run.name")
run.season <- attr(visit, "run.season")


   
#   ================================================================================================
#   Fetch raw catch data, one line per fish or group of fish of same length
f.banner("Retrieving Fish")

catch <- F.get.indiv.fish.data( site, taxon, run, min.date, max.date, keep="unmarked" )

#   If 0 of the target taxon were caught, catch has 0 rows

if( nrow(catch) >= 20 ) print(catch[1:20,]) else print(catch)

#write.table(catch, file="catch_test.csv", sep=",", row.names=F )

site.stream <- attr(catch, "site.stream") 
subsite.string <- attr(catch, "subsites")
taxon.string <- attr(catch, "taxonID" )
sp.commonName <- attr(catch, "species.name")

tmp.catch <<- catch


#   ================================================================================================
#   Now, summarize fish by visit.  I.e., add up and compute mean fork lengths, etc over fish and subsites
f.banner("Summarizing fish by visit")

if( nrow(catch) > 0 ){
    catch.fl <- F.summarize.fish.visit( catch )
} else {
    u.visits <- unique(visit$trapVisitID)
    null <- rep(NA, length(u.visits))
    catch.fl <- data.frame( trapVisitID=u.visits,
                            n.tot=null, n.hatchery=null, n.wild=null, n.morts=null,
                            mean.fl=null, mean.fl.hatchery=null, mean.fl.wild=null,
                            sd.fl=null, sd.fl.hatchery=null, sd.fl.wild=null )
}


#   ================================================================================================
#   merge summed fish into visit data
f.banner("Merging and cleaning up")

visit <- merge( visit, catch.fl, by="trapVisitID", all.x=T ) # keeps all records in visit, but could be missing in catch


#   Visits where they did not catch any fish are just missing from catch.  After merge, these 
#   visits have NA for n.  Change to 0.
visit$n.tot[ is.na(visit$n.tot) ] <- 0
visit$n.hatchery[ is.na(visit$n.hatchery) ] <- 0
visit$n.wild[ is.na(visit$n.wild) ] <- 0
visit$n.morts[ is.na(visit$n.morts) ] <- 0


#   TaxonID for visits where they did not catch fish is missing.  Assign it. 
taxon.composite <- paste(taxon, collapse="+")
visit$taxonID <- rep(taxon.composite, nrow(visit))

#warning("need to implement plus count methods, need to implement subsampling methods, add counts as other columns - n.parr, n.smolt, etc.")

#   Sort
visit <- visit[ order(visit$trapPositionID, visit$sampleStart), ]

#   Assign attributes
attr(visit, "siteID" ) <- site
attr(visit, "site.name") <- site.name
attr(visit, "site.abbr") <- site.abbr
attr(visit, "runID") <- run
attr(visit, "run.name") <- run.name
attr(visit, "run.season") <- run.season
attr(visit, "site.stream") <- site.stream 
attr(visit, "subsites") <- subsite.string
attr(visit, "taxonID" ) <- taxon.string 
attr(visit, "species.name") <- sp.commonName

cat("First 20 records of final catch data frame...\n")
if( nrow(visit) >= 20 ) print( visit[1:20,] ) else print( visit )

f.banner("F.get.catch.data - Complete")

visit

}
