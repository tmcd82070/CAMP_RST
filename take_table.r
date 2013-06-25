F.take.table <- function( site, min.date, max.date, output.file ){
#
#   List all catches, regardless of species, etc. 
#
#   Input:
#   site = site ID of the place we want, trap locaton 
#
#   Output:
#   The take table is output to a CSV file
#


#   ---- Retrieve all catch records between two dates, regardless of species, taxon, etc.
df <- F.get.all.fish.data( site, min.date, max.date )


df$finalRunID[is.na(df$finalRunID)]  <- 255   # do this so that species without runs get through tapply



#   ---- Do some summaries.
by.list <- list( taxonID=df$taxonID, runID=df$finalRunID, fishOriginID=df$fishOriginID, lifeStageID=df$lifeStageID, date=df$visitTime )


#   Live and dead counts
tmp <- df$n * (df$mortID == 2)
n.live <- tapply( tmp, by.list, sum )
tmp <- df$n * (df$mortID == 1)
n.dead <- tapply( tmp, by.list, sum )

#   Now labels
sp.name <- tapply( df$commonName, by.list, function(x){x[1]} )
run.name <- tapply( df$run, by.list, function(x){x[1]} )
fishOrigin.name <- tapply( df$fishOrigin, by.list, function(x){x[1]} )
lifeStage.name <- tapply( df$lifeStageID, by.list, function(x){x[1]} )
date.name <- tapply( df$visitTime, by.list, function(x){x[1]} )


tmp <- data.frame( species=c(sp.name), run=c(run.name), origin=c(fishOrigin.name), 
        lifeStage=c(lifeStage.name), date=c(date.name), n.live=c(n.live), n.dead=c(n.dead), stringsAsFactors=FALSE )
tmp <- tmp[ !(is.na(tmp$n.live) & is.na(tmp$n.dead)), ]
class(tmp$date) <- class(df$visitTime)
print(tmp[1:30,])

print(table(tmp$species))

#   ---- Sort
ord <- order( tmp$species, tmp$run, tmp$origin, tmp$lifeStage )
tmp <- tmp[ord,]

#   ---- Write out the data set
out.fn <- paste(output.file, "_daily_catch.csv", sep="")
write.table( tmp, file=out.fn, sep=",", col.names=T, row.names=F )  # careful, this overwrites any old file


#   ---- Send messages back to the interface
cat("SUCCESS - F.take.table\n\n")
cat(paste("Working directory:", getwd(), "\n"))
cat(paste("R data frames saved in file:", "<no RData saved>", "\n\n"))
cat("Number of files created in working directory = 1\n")
cat(paste(out.fn, "\n"))
cat("\n")
 
invisible(tmp)


}
