#' Class "ModelAnalyticBolusSteadyState"
#'
#' @description The class \code{Model} defines information concerning the construction of an analytical model in steady state.
#' The class \code{ModelAnalyticBolusSteadyState} inherits from the class \code{ModelAnalyticSteadyState}.
#'
#' @name ModelAnalyticBolusSteadyState-class
#' @aliases ModelAnalyticBolusSteadyState
#' @docType class
#' @include ModelAnalyticSteadyState.R
#' @export

ModelAnalyticBolusSteadyState = setClass( "ModelAnalyticBolusSteadyState",
                                          contains = "ModelAnalyticSteadyState",
                                          prototype = prototype(
                                            initialConditions = list(NULL),
                                            odeSolverParameters = list(NULL) ) )
#' initialize
#' @param .Object .Object
#' @param name name
#' @param description description
#' @param equations equations
#' @param outcomes outcomes
#' @param parameters parameters
#' @param modelError modelError
#' @return ModelAnalyticBolusSteadyState
#' @export

setMethod( f="initialize",
           signature="ModelAnalyticBolusSteadyState",
           definition= function (.Object, name, description, equations, outcomes, parameters, modelError )
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
# END Class "ModelAnalyticBolusSteadyState"
##########################################################################################################






