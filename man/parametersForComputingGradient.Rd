% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Model.R
\name{parametersForComputingGradient}
\alias{parametersForComputingGradient}
\alias{parametersForComputingGradient,Model-method}
\title{Define the parameters for computing the gradients of a model.}
\usage{
parametersForComputingGradient(object, valuePars)

\S4method{parametersForComputingGradient}{Model}(object, valuePars)
}
\arguments{
\item{object}{An object from the class \linkS4class{Model}.}

\item{valuePars}{Vector of parameter values}
}
\value{
A list giving the parameters for computing the gradients of a model.
}
\description{
Define the parameters for computing the gradients of a model.
}
