% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Model.R
\name{isModelBolus}
\alias{isModelBolus}
\alias{isModelBolus,Model-method}
\title{Test if a mode is bolus.}
\usage{
isModelBolus(object, designs)

\S4method{isModelBolus}{Model}(object, designs)
}
\arguments{
\item{object}{An object from the class \linkS4class{Model}.}

\item{designs}{A list of objects from the class \linkS4class{Design}.}
}
\value{
Return a Boolean giving if the mode is bolus or not.
}
\description{
Test if a mode is bolus.
}
