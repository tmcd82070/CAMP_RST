% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/checkValidCovars.r
\name{checkValidCovars}
\alias{checkValidCovars}
\title{checkValidCovars}
\usage{
checkValidCovars(df, tmp.df, min.date, max.date, covarB, site, strt.dt,
  end.dt)
}
\arguments{
\item{df}{The data frame for a specific \code{TrapPositionID} containing 
efficiency-trial information and covariates, if available, at the time of 
fitting enhanced efficiency trials in \code{eff_model.r} (or
\code{F.efficiency.model }).}

\item{tmp.df}{The reduced data frame originating from \code{df} containing 
only efficiency-trial dates; i.e., those with non-\code{NA}
\code{nReleased} values.}

\item{min.date}{The start date for data to include. This is a text string in 
the format \code{\%Y-\%m-\%d}, or \code{YYYY-MM-DD}.}

\item{max.date}{The end date for data to include.  Same format as 
\code{min.date}.}

\item{covarB}{A character vector containing the covariates deemed important 
in the enhanced efficiency model tied to the \code{TrapPositionID} returned
from \code{df}.}

\item{site}{The identification number of the site for which estimates are 
required.}

\item{strt.dt}{The remapped start date associated with the current trap's 
minimum (earliest) spline date.}

\item{end.dt}{The remapped end date associated with the current trap's 
maximum (latest) spline date.}
}
\value{
A modified dataframe input of \code{df}, containing possibly 
  filled-in values for \code{NA} observed outside the temporal range of 
  efficiency trials returned from \code{min.date} and \code{max.date}.
}
\description{
Check a data frame of efficiency trials and covariates for a 
  trap location for complete data.
}
\details{
The values of \code{min.date} and \code{max.date} are the same as 
  those provided by the user in the initial passage estimation run.
  
  Function \code{checkValidCovars} first assesses the degree of presence of 
  each covariate (other than temporal spline basis vectors) required to run 
  that particular \code{TrapPositionID}'s enhanced efficiency model via 
  \code{df}.
  
  It performs three checks.  
  
  The first ensures that all covariates listed in \code{covarB} are actually 
  present in data frame \code{df}.  A covariate could have been used for 
  enhanced efficiency fitting, but yet be missing in a particular year
  requested for passage, if over ALL years on which the enhanced efficiency
  estimation was fit, more than 90% of the e-trials contained data on the
  covariate in question.  This implies that, inevitably, one year could "take
  the blame," and thus, prevent enhanced-efficiency estimation.  This
  behavior occurs on the Stanislaw in 1998 on \code{subsiteID=1004}, where
  covariate waterDepth_cm is missing for the majority of the e-trial year. 
  Thus, passage involving this year and trap utilized original spline-style
  efficiency modeling.
  
  The second assesses for presence, for each 
  \code{batchDate}, with respect to the user-provided \code{min.date} and 
  \code{max.date}, which could be arbitrary, while the third assesses for 
  covariate presence within the \code{batchDate} date range over which 
  efficiency trials took place within the provided \code{min.date} and 
  \code{max.date}.
  
  All covariates tied to an enhanced-efficiency fit must at least be present 
  for all inclusive efficiency-trial \code{batchDates} to fit the
  \code{TrapPositionID}'s enhanced efficiency model.
}
\examples{
\dontrun{
checkValidCovars(df,tmp.df,min.date,max.date,covarB,site)
}
}
