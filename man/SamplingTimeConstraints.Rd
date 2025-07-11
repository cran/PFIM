% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/SamplingTimeConstraints.R
\name{SamplingTimeConstraints}
\alias{SamplingTimeConstraints}
\title{SamplingTimeConstraints}
\usage{
SamplingTimeConstraints(
  outcome = character(0),
  initialSamplings = 0,
  fixedTimes = 0,
  numberOfsamplingsOptimisable = 0,
  samplingsWindows = list(),
  numberOfTimesByWindows = 0,
  minSampling = 0
)
}
\arguments{
\item{outcome}{A string giving the outcome.}

\item{initialSamplings}{A vector of numeric giving the initialSamplings.}

\item{fixedTimes}{A vector of numeric giving the fixedTimes.}

\item{numberOfsamplingsOptimisable}{A vector of numeric giving the numberOfsamplingsOptimisable.}

\item{samplingsWindows}{A vector of numeric giving the samplingsWindows.}

\item{numberOfTimesByWindows}{A vector of numeric giving the numberOfTimesByWindows.}

\item{minSampling}{A vector of numeric giving the minSampling.}
}
\description{
The class "SamplingTimeConstraints" implements the constraints for the sampling times.
}
