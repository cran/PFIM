% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Fim.R, R/BayesianFim.R, R/IndividualFim.R,
%   R/PopulationFim.R
\name{reportTablesFIM}
\alias{reportTablesFIM}
\alias{reportTablesFIM,BayesianFim-method}
\alias{reportTablesFIM,IndividualFim-method}
\alias{reportTablesFIM,PopulationFim-method}
\title{Generate the tables for the report.}
\usage{
reportTablesFIM(object, evaluationObject)

\S4method{reportTablesFIM}{BayesianFim}(object, evaluationObject)

\S4method{reportTablesFIM}{IndividualFim}(object, evaluationObject)

\S4method{reportTablesFIM}{PopulationFim}(object, evaluationObject)
}
\arguments{
\item{object}{An object from the class \linkS4class{Fim}.}

\item{evaluationObject}{A list giving the results of the evaluation of the model.}
}
\value{
A list giving the table in kable format for the report.
}
\description{
Generate the tables for the report.
}
