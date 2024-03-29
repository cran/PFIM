% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/MultiplicativeAlgorithm.R
\docType{class}
\name{MultiplicativeAlgorithm-class}
\alias{MultiplicativeAlgorithm-class}
\alias{MultiplicativeAlgorithm}
\title{Class "MultiplicativeAlgorithm"}
\description{
The class \code{MultiplicativeAlgorithm} implements the multiplicative algorithm.
}
\section{Objects from the class}{

Objects form the class \code{MultiplicativeAlgorithm} can be created by calls of the form \code{MultiplicativeAlgorithm(...)} where
(...) are the parameters for the \code{MultiplicativeAlgorithm} objects.
}

\section{Slots for \code{MultiplicativeAlgorithm} objects}{

\describe{
\item{\code{arms}:}{A list giving the arms.}
\item{\code{lambda}:}{A numeric giving the lambda parameter of the multiplicative algorithm.}
\item{\code{delta}:}{A numeric giving the delta parameter of the multiplicative algorithm.}
\item{\code{numberOfIterations}:}{A numeric giving the maximal number iteration of the optimization process.}
\item{\code{optimalWeights}:}{A vector giving the optimal weights.}
\item{\code{optimalDesign}:}{An object of the class \code{Design} giving the optimal design.}
\item{\code{showProcess}:}{A boolean for showing or not the process of optimization.}
}
}

