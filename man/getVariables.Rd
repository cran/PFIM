% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Model.R, R/ModelODE.R, R/ModelODEBolus.R,
%   R/ModelODEInfusion.R
\docType{class}
\name{getVariables}
\alias{getVariables}
\alias{getVariables,ModelODE-method}
\alias{ModelODEBolus-class}
\alias{ModelODEBolus}
\alias{getVariables,ModelODEBolus-method}
\alias{getVariables,ModelInfusion-method}
\title{Return the variable of an ode model}
\usage{
getVariables(object)

\S4method{getVariables}{ModelODE}(object)

\S4method{getVariables}{ModelODEBolus}(object)

\S4method{getVariables}{ModelInfusion}(object)
}
\arguments{
\item{object}{An object from the class \linkS4class{Model}.}
}
\value{
Return the variable of an ode model
}
\description{
The class \code{ModelODEBolus} defines information concerning the construction of an ode model bolus.
The class \code{ModelODEBolus} inherits from the class \code{ModelBolus}.
}
