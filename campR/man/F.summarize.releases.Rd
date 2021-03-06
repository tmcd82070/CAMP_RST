% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/summarize_releases.r
\name{F.summarize.releases}
\alias{F.summarize.releases}
\title{F.summarize.releases}
\usage{
F.summarize.releases(release.df)
}
\arguments{
\item{release.df}{A data frame read by function \code{F.get.release.data}
containing information on releases.  Run season is a vector of
dates for the start and end of a run, and is stored as an attribute of the
data frame.}
}
\value{
A data frame containing a summary of unique combinations of traps
  (via \code{trapPosition}) and releases (via \code{releaseID}).
}
\description{
Summarize trap efficiency for every release.
}
\details{
The multiplicative expansion of caught fish due to half-cone 
  operations utilizes the value of the global variable \code{halfConeMulti}, 
  set in function \code{GlobalVars}. This value is currently set to \code{2}.
}
\examples{
\dontrun{
#   ---- Summarize releases for provided data frame release.df. 
df <- F.summarize.releases(release.df)
}
}
\seealso{
\code{F.get.release.data}
}
\author{
WEST Inc.
}
