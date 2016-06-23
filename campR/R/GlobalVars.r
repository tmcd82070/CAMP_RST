#' @export GlobalVars
#' 
#' @title CAMP RST Global Variables
#' 
#' @description
#' 
#'  Function to run at the beginning to define global variables. 
#' 
#' 
#' @param db.file Character string giving the full path and name of the CAMP Access data base file.  
#' Path relative to \code{getwd()} can be used. 
#' 
#' @param sql.code.dir Directory containing all the SQL code required by \code{campR}. 
#' Defaults to the \code{sql} directory of the \emph{installed} \code{campR} package.
#' By default, all SQL code should be placed in the \code{inst/sql} directory in the 
#' \code{campR} code repository.
#' 
#' @param output.dir Directory for all output. 
#' 	
#' @param Yes.code Integer defining the code for "Yes".  Usually 2. This code is given in an Access look-up 
#' 	table named \code{table.names["yes.no.codes"]}. Unless it has changed, this table was named \code{luNoYes} in Access. 
#' 	To read this code from Access, an ODBC connection is established and disconnected during execution of this 
#' 	routine.
#' 	
#' @param No.code Integer defining the code for "No".  Usually 1. This code is given in an Access look-up 
#' 	table named \code{table.names["yes.no.codes"]}. Unless it has changed, this table was named \code{luNoYes} in Access.
#' 	To read this code from Access, an ODBC connection is established and disconnected during execution of this 
#' 	routine.
#' 	
#' @param samplePeriodCutTime String giving the sample period cut off for batch date assignment, in military time.
#' 	Default value is "04:00:00". This is used to assign batch dates that are missing. 
#' 	If a sample period ends before this time on a particular day, 
#' 	the batch date assigned is the previous day. If a sample period ends after 
#' 	this time, batch date assigned is the day that the sampling period ends (i.e., current day). For example, assuming 
#' 	\code{samplePeriodCutTime} = "04:00:00", a sample period ending at 01:00:00 (1 am) would assign 
#' 	catch to the previous day.  A sample period ending at 07:00:00 (7 am)
#' 	would assign catch to the current day.  See routine \code{\link{assign.batchdates}}.
#' 	
#' @param max.ok.gap Maximum gap, in hours, that is "okay". Default value is 2. Gaps in trapping smaller than 
#' 	this are ignored.  No catch is imputed for them. Gaps in trapping bigger than this get assigned an imputed catch 
#' 	from the GAM model.  Because the GAM model for imputation predicts an 
#' 	hourly rate, this max gap cannot be < 1 hour.
#' 	
#' @param fishingGapMinutes Maximum gap, in minutes, that is NOT okay.  Default value is 10,080 minutes or 7 days. 
#' 	Gaps in fishing greater than 
#' 	this constitute a "big" gap, and no imputation is performed.  The GAM models are unstable for "big" gaps.  
#' 	In these cases, the season is broken into two periods (before and after the gap) and treated as two separate traps.
#' 	
#' @param knotMesh The number of data points required per smoothing spline knot. Default value is 15.
#' 	For example, \code{knotMesh == 15} means each additional knot in a smoothing spline requires an additional
#'  15 data points.  A linear fit requires at least 15 data points,
#'  a quadratic requires at least 30 data points, a spline with 3 knots required 45, 4 knots requires 60, etc. 
#'  This restriction is put in place to help assure smoothing splines have adequate support and are stable. 
#'  
#' @param halfConeMulti The multiplication factor to use for expanding fish caught during halfCone operations.
#'  Default value is 2. For example, if 15 fish were caught during half cone operation, the analysis considers this 
#'  \code{halfConeMulti*15} fish.
#'  
#' @param sample.size.forkLength Number of fish with measured forklength required to assign life stage. 
#'  Default value is 100. 
#'  
#' @param sample.size.forkLengthAndWeight Number of fish with measured forklength 
#'  \emph{and} weight required to use weight in the assignment of life stage.  Default value is 100. 
#'  
#' @param weight.prop.forkLength The minimum proportion of fish with weight to fish with forklength 
#'  in order to use weight in assignment of life stage. Default value is 0.5. 
#'  
#' @param forkLength.mean.diff When the number of life stage groups (2 or 3) is not specified the clustering 
#'  algorithm starts with 3 groups and reduces groups if the mean difference in forklength between groups is less 
#'  than this number. Units are same as forklengths (mm). Default value is 10 mm.
#'  
#' @param time.zone Time zone to assign to all times.  Default value is "America/Los_Angeles", or Pacific time.
#'  It would likely never happen, but these routines cannot correctly account for times in two different zones. 
#'  For example, one trap in one time zone, and another trap in a second zone.  This might only happen if a 
#'  future analysis combined sites over large areas (e.g., states). 
#'  In the current routines, all times are assigned (or forced) to be in this time zone. 
#'  
#'
#' @details   
#'  One additional global variable is defined. \code{table.names} is a list containing the mapping 
#'  of table names in Access to table names in R. This was set up 
#' 	to facilitate painless table name changes in Access.  This should not change unless tables 
#' 	or table names in Access change.  Because these rarely if ever change, we purposefully left this 
#' 	variable out of the arguments to \code{GlobalVars}.  To change table names, you must edit the code 
#' 	of \code{GlobalVars} and recompile the package.

#'  The data base name in use is written to the R log file. 
#' 
#' @return No return value.  
#' 
#' @author WEST Inc.
#' 
#' 
#' @examples 
#' \dontrun{
#' #Change data base file 
#' GlobalVars(db.file="../../Platform/data/StanislawCAMP.mdb" ) 
#' 
#' #Change where output goes 
#' GlobalVars(ouput.dir="~/Camp_output" ) 
#' 
#' }
GlobalVars <- function(
	db.file="..\\Data\\CAMP.mdb",
	output.dir="..\\outputs",
	sql.code.dir=file.path(find.package("campR"),"inst","sql"),
	samplePeriodCutTime = "04:00:00", 
	max.ok.gap = 2,
	fishingGapMinutes = 10080,
	knotMesh = 15,
	halfConeMulti = 2,
	sample.size.forkLength = 100,
	sample.size.forkLengthAndWeight = 100,
	weight.prop.forkLength = 0.5,
	forkLength.mean.diff = 10,
	time.zone = "America/Los_Angeles", 
	Yes.code = 1,
	No.code = 2,
	seed = 884
	){

	  # DB file -------
		assign("db.file", db.file, pos=.GlobalEnv)
	  cat(paste("DB file:", db.file ,"\n"))

	  # Output dir ------	  
	  assign("output.dir", output.dir, pos=.GlobalEnv)
	  
		# SQL code dir ------	  
	  assign("sql.code.dir", sql.code.dir, pos=.GlobalEnv)
	  
	  # Sample Period Cut time ------
    assign("samplePeriodCutTime", samplePeriodCutTime, pos=.GlobalEnv)

	  # Max ok gap ------
    assign("max.ok.gap", max.ok.gap, pos=.GlobalEnv)

	  # Fishing Gap Minutes ------
    assign("fishingGapMinutes", fishingGapMinutes, pos=.GlobalEnv)

	  # Knot mesh -----
    assign("knotMesh", knotMesh, pos=.GlobalEnv)

	  # half cone multiplier -------
    assign("halfConeMulti", halfConeMulti, pos=.GlobalEnv)

	  # sample size to use fork length --------
    assign("sample.size.forkLength", sample.size.forkLength, pos=.GlobalEnv)

	  # sample size to use fork length and weight -------
    assign("sample.size.forkLengthAndWeight", sample.size.forkLengthAndWeight, pos=.GlobalEnv)

	  # weight proportion forklength ------
    assign("weight.prop.forkLength", weight.prop.forkLength, pos=.GlobalEnv)

	  # Mean fork length difference for clusters ------
    assign("forkLength.mean.diff", forkLength.mean.diff, pos=.GlobalEnv)

	  # time zone ------
    assign("time.zone", time.zone, pos=.GlobalEnv)

	  # Yes code (must match data lu.YesNo table in data base) ------
    assign("Yes.code", Yes.code, pos=.GlobalEnv)

	  # No code (must match data lu.YesNo table in data base) ------
	  assign("No.code", No.code, pos=.GlobalEnv)
	  
	  # Set the seed ------
	  set.seed(884)
	  assign("seed", seed, pos=.GlobalEnv)

    
    # Table names is a special global variable that we intentionally make harder ----
    # to change.  I.e., must change code here and re-compile. 
    table.names <- c(trap.visit="TrapVisit",
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
    assign("table.names", table.names, pos=.GlobalEnv)
    
    #  *************** NOTE: To do - read the data base and figure out which water shed is being analyzed.  Then,
    #  *************** Set the efficiency model to use.
    #
    #   Specify the capture efficiency model
    #eff.model.method <- 3

}

