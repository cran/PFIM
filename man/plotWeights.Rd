% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/OptimizationAlgorithm.R,
%   R/MultiplicativeAlgorithm.R, R/Optimization.R
\name{plotWeights}
\alias{plotWeights}
\alias{plotWeights,MultiplicativeAlgorithm-method}
\alias{plotWeights,Optimization-method}
\title{Graph of the weights for the multiplicative algorithm.}
\usage{
plotWeights(object, threshold)

\S4method{plotWeights}{MultiplicativeAlgorithm}(object, threshold)

\S4method{plotWeights}{Optimization}(object, threshold)
}
\arguments{
\item{object}{An object from the class \linkS4class{OptimizationAlgorithm}.}

\item{threshold}{A numeric giving the threshold for the optimal weights in the multiplicative algorithm.}
}
\value{
The graphs of the  weights for the multiplicative algorithm.
}
\description{
Graph of the weights for the multiplicative algorithm.
}
