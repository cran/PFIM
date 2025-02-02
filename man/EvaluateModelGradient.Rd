% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Model.R, R/ModelAnalytic.R,
%   R/ModelAnalyticSteadyState.R, R/ModelAnalyticInfusion.R,
%   R/ModelAnalyticInfusionSteadyState.R, R/ModelODEBolus.R,
%   R/ModelODEDoseInEquations.R, R/ModelODEDoseNotInEquations.R,
%   R/ModelODEInfusionDoseInEquations.R
\name{EvaluateModelGradient}
\alias{EvaluateModelGradient}
\alias{EvaluateModelGradient,ModelAnalytic-method}
\alias{EvaluateModelGradient,ModelAnalyticSteadyState-method}
\alias{EvaluateModelGradient,ModelAnalyticInfusion-method}
\alias{EvaluateModelGradient,ModelAnalyticInfusionSteadyState-method}
\alias{EvaluateModelGradient,ModelODEBolus-method}
\alias{EvaluateModelGradient,ModelODEDoseInEquations-method}
\alias{EvaluateModelGradient,ModelODEDoseNotInEquations-method}
\alias{EvaluateModelGradient,ModelODEInfusionDoseInEquations-method}
\title{Evaluate model gradient.}
\usage{
EvaluateModelGradient(object, dataForModelEvaluation, arm)

\S4method{EvaluateModelGradient}{ModelAnalytic}(object, dataForModelEvaluation, arm)

\S4method{EvaluateModelGradient}{ModelAnalyticSteadyState}(object, dataForModelEvaluation, arm)

\S4method{EvaluateModelGradient}{ModelAnalyticInfusion}(object, dataForModelEvaluation, arm)

\S4method{EvaluateModelGradient}{ModelAnalyticInfusionSteadyState}(object, dataForModelEvaluation, arm)

\S4method{EvaluateModelGradient}{ModelODEBolus}(object, dataForModelEvaluation, arm)

\S4method{EvaluateModelGradient}{ModelODEDoseInEquations}(object, dataForModelEvaluation, arm)

\S4method{EvaluateModelGradient}{ModelODEDoseNotInEquations}(object, dataForModelEvaluation, arm)

\S4method{EvaluateModelGradient}{ModelODEInfusionDoseInEquations}(object, dataForModelEvaluation, arm)
}
\arguments{
\item{object}{An object from the class \linkS4class{Model}.}

\item{dataForModelEvaluation}{...}

\item{arm}{An object from the class \linkS4class{Arm}.}
}
\value{
A list giving the results of the model evaluation.
}
\description{
Evaluate model gradient.
}
