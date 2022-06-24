% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ModelParameter.R
\docType{class}
\name{ModelParameter-class}
\alias{ModelParameter-class}
\alias{ModelParameter}
\title{Class "ModelParameter"}
\description{
Class \code{ModelParameter} represents a parameters theta
included in f(x,theta)
theta = {mu, covariance_matrix}
mu - parameter that acts in the individual model
covariance_matrix - additional parameter for the population model
theta_distribution - Distribution.
}
\section{Objects from the class}{

Objects form the class \code{ModelParameter} can be created by calls of the form \code{ModelParameter(...)} where
(...) are the parameters for the \code{ModelParameter} objects.
}

\section{Slots for ModelParameter objects}{

\describe{
\item{\code{name}:}{A character string giving the name of the parameter.}
\item{\code{mu}:}{A numeric giving the value of the mean mu.}
\item{\code{omega}:}{A numeric giving the value of the variance.}
\item{\code{distribution}:}{An object of the class \code{Distribution}.}
\item{\code{fixed}:}{A boolean giving if the parameter is fixed or remain to be estimated.}
\item{\code{fixedMu}:}{A boolean giving if the mean mu is fixed or remain to be estimaed.}
}
}
