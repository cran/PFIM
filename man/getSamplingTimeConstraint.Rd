% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Arm.R
\name{getSamplingTimeConstraint}
\alias{getSamplingTimeConstraint}
\alias{getSamplingTimeConstraint,Arm-method}
\title{getSamplingTimeConstraint}
\usage{
getSamplingTimeConstraint(object, outcome)

\S4method{getSamplingTimeConstraint}{Arm}(object, outcome)
}
\arguments{
\item{object}{An object \code{Arm} from the class \linkS4class{Arm}.}

\item{outcome}{A string giving the name of the outcome.}
}
\value{
The element of the list \code{samplingTimesConstraints} containing the sampling times constraints of the outcome \code{outcome}
}
\description{
Get the sampling times constraints by outcome.
}
