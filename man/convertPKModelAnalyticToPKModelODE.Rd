% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Model.R, R/ModelAnalytic.R,
%   R/ModelAnalyticSteadyState.R, R/ModelAnalyticInfusion.R
\name{convertPKModelAnalyticToPKModelODE}
\alias{convertPKModelAnalyticToPKModelODE}
\alias{convertPKModelAnalyticToPKModelODE,ModelAnalytic-method}
\alias{convertPKModelAnalyticToPKModelODE,ModelAnalyticSteadyState-method}
\alias{convertPKModelAnalyticToPKModelODE,ModelAnalyticInfusion-method}
\title{Convert an analytic model to a ode model.}
\usage{
convertPKModelAnalyticToPKModelODE(object)

\S4method{convertPKModelAnalyticToPKModelODE}{ModelAnalytic}(object)

\S4method{convertPKModelAnalyticToPKModelODE}{ModelAnalyticSteadyState}(object)

\S4method{convertPKModelAnalyticToPKModelODE}{ModelAnalyticInfusion}(object)
}
\arguments{
\item{object}{An object from the class \linkS4class{Model}.}
}
\value{
A ode model.
}
\description{
Convert an analytic model to a ode model.
}
