#' @export .onAttach
#' 
#' @title onAttach
#' 
#' @description
#' 
#'    Function to run "on attachment", or before all CAMP R analyses.  Contains mostly global variable declarations. 
#' 
#' 
#' @details 
#' Global constants defined here:
#' \list{
#' 	\item \code{db.file}: character string naming the CAMP Access data base fiel.  Full or relative path must be inluded. 
#' 	
#' 	\item \code{table.names}: A list containing the mapping of table names in Access to table names in R. This was used 
#' 	to facilitate painless table name changes in Access.  This should not change unless tables or table names in Access change.
#' 	
#' 	\item \code{Yes.code}: Integer defining the code for "Yes".  Usually 2. This code is given in an Access look-up 
#' 	table named \code{table.names["yes.no.codes"]}. Unless it has changed, this table was named \code{luNoYes} in Access. 
#' 	To read this code from Access, an ODBC connection is established and disconnected during execution of this 
#' 	routine.
#' 	
#' 	\item \code{No.code}: Integer defining the code for "No".  Usually 1. This code is given in an Access look-up 
#' 	table named \code{table.names["yes.no.codes"]}. Unless it has changed, this table was named \code{luNoYes} in Access.
#' 	To read this code from Access, an ODBC connection is established and disconnected during execution of this 
#' 	routine.
#' 	
#' 	\item \code{samplePeriodCutTime}: String giving the sample period cut off for batch date assignment, in military time.
#' 	Current value is "04:00:00". This is used to assign batch dates that are missing. 
#' 	If a sample period ends before this time on a particular day, 
#' 	the batch date assigned is the previous day. If a sample period ends after 
#' 	this time, batch date assigned is the day that the sampling period ends (i.e., current day). For example, assuming 
#' 	\code{samplePeriodCutTime} = "04:00:00", a sample period ending at 01:00:00 (1 am) would assign 
#' 	catch to the previous day.  A sample period ending at 07:00:00 (7 am)
#' 	would assign catch to the current day.  See routine \code{\link{assign.batchdates}}.
#' 	
#' 	\item \code{max.ok.gap}: Maximum gap, in hours, that is "okay". Current value is 2. Gaps in trapping smaller than 
#' 	this are ignored.  No catch is imputed for them. Gaps in trapping bigger than this get assigned an imputed catch 
#' 	from the GAM model.  Because the GAM model for imputation predicts an 
#' 	hourly rate, this max gap cannot be < 1 hour.
#' 	
#' 	\item \code{fishingGapMinutes} Maximum gap, in minutes, that is NOT okay.  Current value is 10,080 minutes or 7 days. 
#' 	Gaps in fishing greater than 
#' 	this constitute a "big" gap, and no imputation is performed.  The GAM models are unstable for "big" gaps.  
#' 	In these cases, the season is broken into two periods (before and after the gap) and treated as two separate traps.
#' 	
#' 	\item \code{knotMesh}: The number of data points required per smoothing spline knot. Current value is 15.
#' 	For example, \code{knotMesh == 15} means each additional knot in a smoothing spline requires an additional
#'  15 data points.  A linear fit requires at least 15 data points,
#'  a quadratic requires at least 30 data points, a spline with 3 knots required 45, 4 knots requires 60, etc. 
#'  This restriction is put in place to help assure smoothing splines have adequate support and are stable. 
#'  
#'  \item \code{halfConeMulti}: The multiplication factor to use for expanding fish caught during halfCone operations.
#'  Current value is 2. For example, if 15 fish were caught during half cone operation, the analysis considers this 
#'  \code{halfConeMulti*15} fish.
#'  
#'  \item \code{sample.size.forkLength}: Number of fish with measured forklength required to assign life stage. 
#'  Current value is 100. 
#'  
#'  \item \code{sample.size.forkLengthAndWeight}: Number of fish with measured forklength 
#'  \emph{and} weight required to use weight in the assignment of life stage.  Current value is 100. 
#'  
#'  \item \code{weight.prop.forkLength}: The minimum proportion of fish with weight to fish with forklength 
#'  in order to use weight in assignment of life stage. Current value is 0.5. 
#'  
#'  \item \code{forkLength.mean.diff}: When the number of life stage groups (2 or 3) is not specified the clustering 
#'  algorithm starts with 3 groups and reduces groups if the mean difference in forklength between groups is less 
#'  than this number. Units are same as forklengths (mm). Current value is 10 mm.
#'  
#'  \item \code{time.zone}: Time zone to assign to all times.  Current value is "America/Los_Angeles", or Pacific time.
#'  It would likely never happen, but these routines cannot correctly account for times in two different zones. 
#'  For example, one trap in one time zone, and another trap in a second zone.  This might only happen if a 
#'  future analysis combined sites over large areas (e.g., states). 
#'  In the current routines, all times are assigned (or forced) to be in this time zone. 
#'  
#'  }
#'  
#'  In addition to defining global constants, \code{.onAttach} writes the data base name and memory limit 
#'  of the machine to the R log file. 
#' 
#' @return No return value.  
#' 
#' @author WEST Inc.
#' 
#' @seealso \code{\link{library}}
#' 
#' @examples
#' library(campR)
#' 
#' .onAttach()
.onAttach <- function(){
	#.libPaths(.Library)   # check out libpaths -- 01/04/2016.  Jason's machine may need this

	#   Attach the libraries we need for all or nearly all routines.
  #library(RODBC)

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

    ch <- odbcConnectAccess(db.file)
    luNoYes <- sqlFetch(ch, table.names["yes.no.codes"])
    No.code <<- luNoYes$noYesID[ casefold(luNoYes$noYes) == "no" ]
    Yes.code <<- luNoYes$noYesID[ casefold(luNoYes$noYes) == "yes" ]
    close(ch)

    samplePeriodCutTime <<- "04:00:00"              # In military time

    max.ok.gap <<- 2

    fishingGapMinutes <<- 10080

    knotMesh <<- 15

    halfConeMulti <<- 2

    sample.size.forkLength <<- 100

    sample.size.forkLengthAndWeight <<- 100

    weight.prop.forkLength <<- .5

    forkLength.mean.diff <<- 10

    time.zone <<- "America/Los_Angeles"

    cat(paste("Memory limit:", memory.limit(), "Mb \n"))
    
    #  *************** NOTE: To do - read the data base and figure out which water shed is being analyzed.  Then,
    #  *************** Set the efficiency model to use.
    #
    #   Specify the capture efficiency model
    #eff.model.method <- 3

}

