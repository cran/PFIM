% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Arm.R
\name{getSamplingTime}
\alias{getSamplingTime}
\alias{getSamplingTime,Arm-method}
\title{getSamplingTime}
\usage{
getSamplingTime(object, outcome)

\S4method{getSamplingTime}{Arm}(object, outcome)
}
\arguments{
\item{object}{An object \code{Arm} from the class \linkS4class{Arm}.}

\item{outcome}{A string giving the name of the outcome.}
}
\value{
The element of the list \code{samplingTimes} containing the sampling times of the outcome \code{outcome}
}
\description{
Get the sampling times by outcome.
}
