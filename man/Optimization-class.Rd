% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Optimization.R
\docType{class}
\name{Optimization-class}
\alias{Optimization-class}
\alias{Optimization}
\title{Class "Optimization"}
\description{
A class storing information concerning the design optimization.
}
\section{Objects from the class}{

Objects form the class \code{Optimization} can be created by calls of the form \code{Optimization(...)} where
(...) are the parameters for the \code{Optimization} objects.
}

\section{Slots for \code{Administration} objects}{

 \describe{
   \item{\code{name}:}{A character string giving the name of the optimization process.}
   \item{\code{model}:}{A object of class \code{Model} giving the model.}
   \item{\code{modelEquations}:}{A list giving the model equations.}
   \item{\code{modelParameters}:}{A list giving the model parameters.}
   \item{\code{modelError}:}{A list giving the model error.}
   \item{\code{optimizer}:}{A object of class \code{OptimizationAlgorithm} giving the optimization algorithm.}
   \item{\code{optimizerParameters}:}{A list giving the parameters of the optimization algorithm.}
   \item{\code{outcomes}:}{A list giving the outcomes of the model.}
   \item{\code{designs}:}{A list giving the designs to be optimized.}
   \item{\code{fim}:}{A object of class \code{FIM} giving the Fisher information matrix.}
   \item{\code{odeSolverParameters}:}{A list giving the parameters for the ode solver.}
   \item{\code{optimizationResults}:}{A object of class \code{OptimizationAlgorithm} giving the results of the optimization.}
   \item{\code{evaluationFIMResults}:}{A object of class \code{Evaluation} giving the results of the evaluation of the optimal design.}
   \item{\code{evaluationInitialDesignResults}:}{A object of class \code{Evaluation} giving the results of the evaluation of the initial design.}
 }
}

