#' Class "ModelAnalyticBolus"
#'
#' @description The class \code{Model} defines information concerning the construction of an analytical bolus model.
#' The class \code{ModelAnalyticBolus} inherits from the class \code{ModelAnalytic}.
#'
#' @name ModelAnalyticBolus-class
#' @aliases ModelAnalyticBolus
#' @docType class
#' @include ModelAnalytic.R
#' @export

ModelAnalyticBolus = setClass( Class = "ModelAnalyticBolus",
                               contains = "ModelAnalytic",
                               prototype = prototype(
                                 initialConditions = list(NULL),
                                 odeSolverParameters = list(NULL)))

setMethod(
  f="initialize",
  signature="ModelAnalyticBolus",
  definition= function (.Object, name, description, equations, outcomes, parameters, modelError)
  {
    if(!missing(name))
    {
      .Object@name = name
    }
    if(!missing(description))
    {
      .Object@description = description
    }
    if(!missing(equations))
    {
      .Object@equations = equations
    }
    if(!missing(outcomes))
    {
      .Object@outcomes = outcomes
    }
    if(!missing(parameters))
    {
      .Object@parameters = parameters
    }
    if(!missing(modelError))
    {
      .Object@modelError = modelError
    }
    validObject(.Object)
    return (.Object )
  }
)

##########################################################################################################
# END Class "ModelAnalyticBolus"
##########################################################################################################

