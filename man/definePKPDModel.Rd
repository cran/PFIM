% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Model.R, R/ModelAnalytic.R,
%   R/ModelAnalyticSteadyState.R, R/ModelAnalyticInfusion.R, R/ModelODEBolus.R,
%   R/ModelODEDoseInEquations.R, R/ModelODEDoseNotInEquations.R,
%   R/ModelODEInfusionDoseInEquations.R
\name{definePKPDModel}
\alias{definePKPDModel}
\alias{definePKPDModel,ModelAnalytic,ModelAnalytic-method}
\alias{definePKPDModel,ModelAnalytic,ModelODE-method}
\alias{definePKPDModel,ModelAnalyticSteadyState,ModelAnalyticSteadyState-method}
\alias{definePKPDModel,ModelAnalyticSteadyState,ModelODE-method}
\alias{definePKPDModel,ModelAnalyticInfusion,ModelAnalytic-method}
\alias{definePKPDModel,ModelAnalyticInfusion,ModelODE-method}
\alias{definePKPDModel,ModelODEBolus,ModelODE-method}
\alias{definePKPDModel,ModelODEDoseInEquations,ModelODE-method}
\alias{definePKPDModel,ModelODEDoseNotInEquations,ModelODE-method}
\alias{definePKPDModel,ModelODEInfusion,ModelODE-method}
\alias{definePKPDModel,ModelODEInfusionDoseInEquations,ModelODE-method}
\title{Define a PKPD model.}
\usage{
definePKPDModel(PKModel, PDModel, outcomes)

\S4method{definePKPDModel}{ModelAnalytic,ModelAnalytic}(PKModel, PDModel, outcomes)

\S4method{definePKPDModel}{ModelAnalytic,ModelODE}(PKModel, PDModel, outcomes)

\S4method{definePKPDModel}{ModelAnalyticSteadyState,ModelAnalyticSteadyState}(PKModel, PDModel, outcomes)

\S4method{definePKPDModel}{ModelAnalyticSteadyState,ModelODE}(PKModel, PDModel, outcomes)

\S4method{definePKPDModel}{ModelAnalyticInfusion,ModelAnalytic}(PKModel, PDModel, outcomes)

\S4method{definePKPDModel}{ModelAnalyticInfusion,ModelODE}(PKModel, PDModel, outcomes)

\S4method{definePKPDModel}{ModelODEBolus,ModelODE}(PKModel, PDModel, outcomes)

\S4method{definePKPDModel}{ModelODEDoseInEquations,ModelODE}(PKModel, PDModel, outcomes)

\S4method{definePKPDModel}{ModelODEDoseNotInEquations,ModelODE}(PKModel, PDModel, outcomes)

\S4method{definePKPDModel}{ModelODEInfusion,ModelODE}(PKModel, PDModel, outcomes)

\S4method{definePKPDModel}{ModelODEInfusionDoseInEquations,ModelODE}(PKModel, PDModel, outcomes)
}
\arguments{
\item{PKModel}{An object from the class \linkS4class{Model}.}

\item{PDModel}{An object from the class \linkS4class{Model}.}

\item{outcomes}{A list giving the outcomes of the  PKPD model.}
}
\value{
A model giving a PKPD model.
}
\description{
Define a PKPD model.
}
