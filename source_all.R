#
#   Thim mimics attaching a package.  Eventually this will be replaced when the real package is done.
#

#remove(list=ls())   # erase everything; CAREFUL

.onAttach <- function(){
    #   Attach the libraries we need for all or nearly all routines.
    library(RODBC)
    
    #   These are other libraries needed by certain routines.  These need to be installed, 
    #   and will be attached by the routines that need them. 
    #   library(quantreg)
    #   library(splines)
    #   library(MASS)
    #   library(mvtnorm)  # this is needed in F.bootstrap.passage

    

    #   =================== Global variables 
    
    #   Parameter db.file is a string containing the full or relative path to the data base
    #db.file <<- "..\\Data\\WorkingVersionCAMP_LAR_16July2013.mdb"  # For trent's testing in 'code' directory
    #db.file <<- "..\\Data\\CAMPFeather_NewVersion1July2013.mdb"  # For trent's testing in 'code' directory
    #db.file <<- "..\\Data\\connie's caswell stanislaus file after doug adds finaRunIDs  CAMP.mdb"


    db.file  <<- "..\\Data\\CAMP.mdb"    #   THE OFFICIAL DATA BASE
    
    cat(paste("DB file:", db.file ,"\n"))

    #   Parameter table.names is a list containing the mapping of table names in Access to table names in R. 
    #   This was used to facility painless table name changes in Access.  This should not change unless tables or table names in Access change.
    table.names <<- c(trap.visit="TrapVisit",
                    sites = "Site",
                    project = "ProjectDescription",
                    subsites = "SubSite",
                    catch = "CatchRaw",
                    release="Release",
                    mark.applied="MarkApplied",
                    catch = "CatchRaw",
                    mark.found="MarkExisting",
                    trap.visit="TrapVisit",
                    species.codes="luTaxon",
                    run.codes ="luRun",
                    rel.x.target="ReleaseXTargetSite",
                    LAD = "LengthAtDate",
                    yes.no.codes="luNoYes",
                    CAMP.life.stages="luLifeStageCAMP",
                    life.stages="luLifeStage",
                    fish.origin="luFishOrigin" )
                    
    #   Retreive the YES/NO codes from the luNoYes table.  Just in case they should ever change in the data base
#     setwd('C:/Users/jmitchell/Desktop/CAMP_RST-master/Jason')
#     ch <- odbcConnectAccess('CAMPStanislaus_16Sept2013.mdb')
    ch <- odbcConnectAccess(db.file)
    luNoYes <- sqlFetch(ch, table.names["yes.no.codes"])
    No.code <<- luNoYes$noYesID[ casefold(luNoYes$noYes) == "no" ]
    Yes.code <<- luNoYes$noYesID[ casefold(luNoYes$noYes) == "yes" ]
    close(ch)      
    
    #   Assign sample cut time for batch dates that are missing. 
    #   If a sample period ends before this time, batch date is the day the period ends. 
    #   If a sample period ends after this time, batch date is the next day following the end of the sampling period.
    samplePeriodCutTime <<- "04:00:00"              # In military time
    
    #   Maximum gap, in hours, that is "okay".  Gaps in trapping smaller than this are ignored.  No catch is imputed for them. 
    #   Because gam model for imputation predicts an hourly rate, this max gap cannot be < 1 hour
    max.ok.gap <<- 2
    
    #   Write out the memory limit on this machine
    cat(paste("Memory limit:", memory.limit(), "Mb \n"))    
    
    #   Set time zone. NOTE: all times are assumed to be in this time zone.  
    #   If not, they may be incorrect.  In any event, all times are forced to this time zone.
    time.zone <<- "America/Los_Angeles"
    
    #  *************** NOTE: To do - read the data base and figure out which water shed is being analyzed.  Then, 
    #  *************** Set the efficiency model to use. 
    #
    #   Specify the capture efficiency model
    #eff.model.method <- 3
                    
}
.onAttach()

#   --------------------------------------------------------
#   Source code

source(	"capwords.r"	)
source(	"catch_model.r"	)
source(	"eff_model.r"	)
source(	"est_catch.r"	)
source(	"est_efficiency.r"	)
source(	"est_passage.r"	)
#source(	"find_recaps.r"	)
source(	"get_catch_data.r"	)
source(	"get_all_catch_data.r"	)
source(	"get_release_data.r"	)
#source(	"latex_biweekly_table.r"	)
#source(	"odt_biweekly_table.r"	)
#source(	"latex_passage.r"	)
#source(	"latex_recapSummary.r"	)
source(	"passage.r"	)
source( "annual_passage.r" )
source(	"plot_catch_model.r"	)
source(	"plot_eff_model.r"	)
source(	"plot_passage.r"	)
source(	"release_summary.r"	)
source( "run_passage.r" )
source(	"summarize_releases.r"	)
source( "summarize_fish_visit.r" )
source( "all_catch_table.r" )
#source(	"vec2sqlstr.r"	)
source(	"size_by_date.r"	)
source(	"get_indiv_fish_data.r"	)
source(	"get_indiv_visit_data.r"	)
source(	"length_freq.r"	)
source( "sql_error_check.r" )
#source( "assign_sample_period.r" )
source( "assign_batch_date.r" )
source( "assign_gaplen.r" )
#source( "check_for_missing.r" )
#source( "biweekly_report.r" )
source( "weekly_effort.r" )
#source( "weekly_passage.r" )  # exclude. Same as F.passage with by="week"
source( "lifestage_passage.r" )
source( "bootstrap_passage.r" )
source( "summarize_passage.r" )
source( "summarize_index.r" )
source( "expand_plus_counts.r" )
source( "assign_1dim.r" )
source( "assign_2dim.r" )                             # stuck in an endless loop?  maybe doesnt matter.
source( "get_all_fish_data.r" )
source( "plot_lifestages.r" )
#source( "get_life_stages.r" )
source( "build_report_criteria.r" )
source( "chinook_by_date.r" )
source( "get_by_catch.r" )
source( "by_catch_table.r" )
source( "run_sqlFile.r" )
source( "build_report_criteria_release.r" )


