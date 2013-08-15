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
df <- F.get.all.fish.data( site, min.date, max.date )  # this does the cross-walk to CAMP lifestages


#   ---- Fix up the missing values
df$finalRunID[is.na(df$finalRunID)]  <- 251   # do this so that species without runs get through tapply
df$lifeStageID[is.na(df$lifeStageID)]  <- 251   # do this so that species without runs get through tapply
df$fishOriginID[is.na(df$fishOriginID)]  <- 251   # do this so that species without runs get through tapply


df$finalRunID[ df$finalRunID %in% c(252,253,254,255)] <- 251
df$lifeStageID[ df$lifeStageID %in% c(252,253)] <- 251
df$fishOriginID[ df$fishOriginID %in% c(252,253,254,255)] <- 251



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
lifeStage.name <- tapply( df$lifeStage, by.list, function(x){x[1]} )
date.name <- tapply( df$visitTime, by.list, function(x){x[1]} )


tmp <- data.frame( species=c(sp.name), run=c(run.name), origin=c(fishOrigin.name), 
        lifeStage=c(lifeStage.name), date=c(date.name), n.live=c(n.live), n.dead=c(n.dead), stringsAsFactors=FALSE )
tmp <- tmp[ !(is.na(tmp$n.live) & is.na(tmp$n.dead)), ]
class(tmp$date) <- class(df$visitTime)

cat("Life Stage X species table:\n")
print(table(tmp$lifeStage,tmp$species, useNA="always"))


tmp <- tmp[order(tmp$date),]

cat("\nFirst 30 records of output sorted by date:\n")
print(tmp[1:30,])
cat("Table of counts of all species found:\n")
print(table(tmp$species))


#   ---- Sort
ord <- order( tmp$species, tmp$run, tmp$origin, tmp$lifeStage, tmp$date )
tmp <- tmp[ord,]



#   ---- Write out the data set, after fixing it up a bit
tmp <- tmp[,c("species","run","origin","lifeStage","date","n.live","n.dead")]
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
