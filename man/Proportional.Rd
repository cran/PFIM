% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Proportional.R
\name{Proportional}
\alias{Proportional}
\title{Proportional}
\usage{
Proportional(
  output = character(0),
  equation = expression(sigmaSlope),
  derivatives = list(),
  sigmaInter = 0,
  sigmaSlope = 0,
  sigmaInterFixed = FALSE,
  sigmaSlopeFixed = FALSE,
  cError = 1
)
}
\arguments{
\item{output}{A string giving the model error output.}

\item{equation}{A expression giving the model error equation.}

\item{derivatives}{A list giving the derivatives of the model error equation.}

\item{sigmaInter}{A double giving the sigma inter.}

\item{sigmaSlope}{A double giving the sigma slope}

\item{sigmaInterFixed}{A Boolean giving if the  sigma inter is fixed or not. - not in the v7.0}

\item{sigmaSlopeFixed}{A Boolean giving if the  sigma slope is fixed or not. - not in the v7.0}

\item{cError}{A integer giving the power parameter.}
}
\description{
The class \code{Proportional} is used to defined a model error.
}
