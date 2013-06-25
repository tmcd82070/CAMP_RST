F.biweekly.report <- function(  site, min.date, max.date ){
#F.biweekly.report <- function(  site, min.date, max.date, output.type, from, to, return.addr ){  Reinistate this line when karen fixed the interface
#
#   Generate a bi-weekly report
#
#   output.type = "pdf" or "odt"
#

#   add these 4 variables back to the parameters when Karen gets the interface fixed 
    output.type <- "odt"   # or "pdf"
    from      <- "Trent McDonald, Ph.D., WEST Incorporated"
    to        <- "Doug Threloff, USFWS CAMP Coordinator"
    return.addr <- "FISH AND WILDLIFE SERVICE!USFWS Caswell State Park Office!1234 Abbey Rd.!Caswell, California  96080!(530) 527-3043, FAX (530) 529-0292"
    run <- 3


input.name <- "SampleBiWeeklyReport"
outfn.name <- paste(input.name, "_", format(Sys.time(),"%d%b%y"), sep="")

if( output.type == "pdf" ){
#    library(RODBC)
    library(tools)
    
    intex.fn <- paste(input.name, ".Rnw", sep="")
    outtex.fn <- paste(outfn.name, ".tex", sep="")
    
    Sweave( intex.fn, output=outtex.fn )
    system( paste("texify.exe --pdf", outtex.fn ))

} else if( output.type == "odt" ){
    library(odfWeave)

    inodt.fn <- paste(input.name, ".odt", sep="")
    outodt.fn <- paste(outfn.name, ".odt", sep="")
    
    odfWeave( inodt.fn, outodt.fn )

} else stop("Unknown output type.")

1
}
