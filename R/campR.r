#' campR: R routines for the CAMP R platform.
#'
#' @section Description: Allow annual estimation of both catch and efficiency of
#'   juvenile Chinook salmon in the Central Valley of California.
#'
#' @docType package
#' @name campR
NULL

#' Beta coefficients resulting from the fitting of trap- (\code{trapPositionID}
#' or \code{subSiteID}) and river-specific (\code{site}) enhanced-efficiency
#' models.
#'
#' @name betas
#' @docType data
#' @usage data(betas)
#' @format A data frame with 40 rows and 24 variables.
#' 
#' \describe{ A dataset containing beta estimates resulting from the fitting of 
#' trap-specific generalized additive logistic models enhanced efficiency models
#' in the Central Valley of California, USA.  All coefficients are on the logit 
#' scale, and interpret as the average positive increase (negative decrease) in 
#' log-odds of efficiency for every one-unit increase (decrease) in the measured
#' variable.  Positive values communicated average incraese, while negative 
#' values communicate average decrease.  For example, a value of +1.50 for
#' \code{turbidity_ntu} communicates that for every one-ntu increase in
#' turbidity is associated with a +1.50 increase in the log-odds of efficiency. 
#' The variables are as follows:
#' 
#' \itemize{
#'   \item \code{subsiteID}. Trap identifier.
#'   \item \code{(Intercept)}. Estimated intercept.  
#'   \item \code{bdMeanNightProp}. Estimated effect due to proportion of fishing at night.  
#'   \item \code{bdMeanMoonProp}. Estimated effect due to proportion of fishing with moon in the sky.
#'   \item \code{bdMeanForkLength}. Estimated effect due to mean fork length.  
#'   \item \code{flow_cfs}. Estimated effect due to flow, in units of cfs, as recorded in the Environmental Covariate database.
#'   \item \code{temp_c}. Estimated effect due to water temperature, in units of C, as recorded in the Environmental Covariate database.  
#'   \item \code{discharge_cfs}.Estimated effect due to flow, in units of cfs, as recorded in the CAMP \code{mdb}.
#'   \item \code{waterDepth_cm}.Estimated effect due to water depth, in units of cm, as recorded in the CAMP \code{mdb}.  
#'   \item \code{waterDept_ft}. Estimated effect due to water dept, in units of ft, as recorded in the CAMP \code{mdb}.  
#'   \item \code{airTemp_C}. Estimated effect due to air temperature, in units of C, as recorded in the CAMP \code{mdb}.  
#'   \item \code{airTemp_F}. Estimated effect due to air temperature, in units of F, as recorded in the CAMP \code{mdb}.  
#'   \item \code{turbidity_ntu}. Estimated effect due to turbidity, in units of ntu, as recorded in the CAMP \code{mdb}.  
#'   \item \code{waterVel_fts}. Estimated effect due to water velocity, in units of ft/s, as recorded in the CAMP \code{mdb}.  
#'   \item \code{waterTemp_C}. Estimated effect due to water temperature, in units of C, as recorded in the CAMP \code{mdb}.  
#'   \item \code{waterTemp_F}. Estimated effect due to water temperature, in units of F, as recorded in the CAMP \code{mdb}.  
#'   \item \code{lightPenetration_ntu}. Estimated effect due to light penetration, in units of ntu, as recorded in the CAMP \code{mdb}.  
#'   \item \code{dissolvedOxygen_mgL}. Estimated effect due to dissolved oxygen, in units of mg/L, as recorded in the CAMP \code{mdb}.  
#'   \item \code{conductivity_mgL}. Estimated effect due to conductivity, in units of mg/L, as recorded in the CAMP \code{mdb}.  
#'   \item \code{barometer_inHg}. Estimated effect due to barometer, in units of inches Hg, as recorded in the CAMP \code{mdb}.  
#'   \item \code{precipLevel_qual}. Estimated effect due to precipitation level, on a 4-unit quantitative scale, as recorded in the CAMP \code{mdb}.  
#'   \item \code{threshold}. The number of distinct efficiency trials with no missing covariates utilized to estimate the betas for the given \code{subsiteID}.
#'   \item \code{available}. The number of distinct efficiency trials considered for beta estimation for the given \code{subsiteID}.
#'   \item \code{Stage}. Ubiquitously set to value of 'Final Model Betas'.  
#' }
#' }
#' @keywords datasets
#' 
NULL

#' Average cubic feet per second recorded from the total dishcarge diverted to
#' the two side canals immediately prior to the flow of the Scaramento River
#' through the Red Bluff Diversion Dam.  
#'
#' @name canal
#' @docType data
#' @usage data(canal)
#' @format A data frame with 366 rows and 2 variables.
#' 
#' \describe{A dataset containing a row for each day of the leap-year in the format of \code{mm-dd}, and another column holding the average 
#' cubic feet per second through the two diversion canals on the Sacramento River immediately before the Red Bluff Diversion Dam.  Daily 
#' values obtained from the average of 11 years' worth of recorded data, 2002-2013.  Data obtained from Felipe Carrillo.  
#' The variables are as follows:
#' 
#' \itemize{
#'   \item \code{monthDay}. Month-day identifier in the form \code{mm-dd}.  Note entry for \code{2-29}.  
#'   \item \code{average_cfs}. Estimated diversion daily flow in cubic feet per second.  
#' }
#' }
#' @keywords datasets
#' 
NULL

#' Annual estimates of available covariates, per site and estimation year.  Used in lieu of 
#' daily covariate values in the estimation of enhanced efficiency models, when daily data 
#' do not exist, or are otherwise not available.  
#'
#' @name annual_records
#' @docType data
#' @usage data(annual_records)
#' @format A data frame with 81 rows and 18 variables.
#' 
#' \describe{Simple annual mean estimates per \code{site} and \code{Season}
#' (typically year).  Values of \code{-9999} in variable \code{Season} indicate
#' overall grand-mean values over all years for the given \code{site}.
#' The variables are as follows:
#' 
#' \itemize{
#'   \item \code{site}. Site identifier.  Usually tied to a particular stream.  
#'   \item \code{Season}. Typically a year, or in the case of \code{-9999}, the mean over all available values for that \code{site}.
#'   \item \code{discharge_cfs}.Estimated annualized mean effect due to flow, in units of cfs, as recorded in the CAMP \code{mdb}. 
#'   \item \code{waterDepth_cm}.Estimated annualized mean effect due to water depth, in units of cm, as recorded in the CAMP \code{mdb}.  
#'   \item \code{waterVel_fts}. Estimated annualized mean effect due to water velocity, in units of ft/s, as recorded in the CAMP \code{mdb}. 
#'   \item \code{airTemp_F}. Estimated annualized mean effect due to air temperature, in units of F, as recorded in the CAMP \code{mdb}.  
#'   \item \code{waterTemp_C}. Estimated annualized mean effect due to water temperature, in units of C, as recorded in the CAMP \code{mdb}.  
#'   \item \code{lightPenetration_ntu}. Estimated annualized mean effect due to light penetration, in units of ntu, as recorded in the CAMP \code{mdb}.  
#'   \item \code{turbidity_ntu}. Estimated annualized mean effect due to turbidity, in units of ntu, as recorded in the CAMP \code{mdb}.  
#'   \item \code{dissolvedOxygen_mgL}. Estimated annualized mean effect due to dissolved oxygen, in units of mg/L, as recorded in the CAMP \code{mdb}.  
#'   \item \code{conductivity_mgL}. Estimated annualized mean effect due to conductivity, in units of mg/L, as recorded in the CAMP \code{mdb}.  
#'   \item \code{barometer_inHg}. Estimated annualized mean effect due to barometer, in units of inches Hg, as recorded in the CAMP \code{mdb}.  
#'   \item \code{flow_cfs}. Estimated annualized mean effect due to flow, in units of cfs, as recorded in the Environmental Covariate database.
#'   \item \code{temp_c}. Estimated annualized mean effect due to water temperature, in units of C, as recorded in the Environmental Covariate database.  
#'   \item \code{percQ}. Estimated annualized mean effect due to percent \eqn{Q}.     
#'   \item \code{bdMeanNightProp}. Estimated annualized mean effect due to proportion of fishing at night.  
#'   \item \code{bdMeanMoonProp}. Estimated annualized mean effect due to proportion of fishing with moon in the sky.
#'   \item \code{bdMeanForkLength}. Estimated annualized mean effect due to mean fork length.  
#' }
#' }
#' @keywords datasets
#' 
NULL