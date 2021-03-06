% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/annualizeCovars.R
\name{annualizeCovars}
\alias{annualizeCovars}
\title{annualizeCovars}
\usage{
annualizeCovars(site, min.date, max.date, season, taxon)
}
\arguments{
\item{site}{The identification number of the site for which estimates are 
required.}

\item{min.date}{The start date for data to include. This is a text string in 
the format \code{\%Y-\%m-\%d}, or \code{YYYY-MM-DD}.}

\item{max.date}{The end date for data to include.  Same format as 
\code{min.date}.}

\item{season}{Typically a year related to the timeframe specified by 
\code{min.date} and \code{max.date}.}

\item{taxon}{The species identifier indicating the type of fish of interest. 
This is always \code{161980}; i.e., Chinook Salmon.}
}
\value{
If successful, an updated data file in the thing.
}
\description{
Estimate available covariates annually per site.
}
\details{
Sites and "annual" are defined via the so-called 
  \code{"TheExcel.csv"}, the listing of which forms the loops of the Big 
  Looper.  Annual here corresponds to \code{"Season"}, although one year (or 
  "annual" time period) only ever contains one Season.
}
