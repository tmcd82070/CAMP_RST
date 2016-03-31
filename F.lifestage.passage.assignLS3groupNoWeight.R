F.lifestage.passage.assignLS3groupNoWeight <- function(site, taxon, min.date, max.date, output.file, ci=TRUE){
###
###   ANNUAL PRODUCTION ESTIMATES BY LIFE STAGE AND RUN – TABULAR SUMMARY
###   A table of passage estimates, with lifestages down the rows, and runs across the columns.
###
###   Input:
###   site = site ID of the place we want, trap locaton
###   taxon = taxon number (from luTaxon) to retrieve
###


    ## 3 life stage groups are fit and weight will not be used to assign lifestage
    passageWithLifeStageAssign(site=site,taxon=taxon,min.date=min.date,max.date=max.date,output.file=output.file,ci=ci,nLS=3,weightUse=FALSE)

}
