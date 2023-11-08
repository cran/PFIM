#' Class "ModelInfusion"
#' @description ...
#' @name ModelInfusion-class
#' @aliases ModelInfusion
#' @docType class
#' @include Model.R
#' @export

ModelInfusion = setClass("ModelInfusion",
                         contains = "Model",
                         prototype = prototype(
                           equations = list(
                             duringInfusion = list(),
                             afterInfusion = list())))
setMethod(
  f="initialize",
  signature="ModelInfusion",
  definition= function (.Object, name, description, equations, outcomes, parameters,
                        modelError, initialConditions, odeSolverParameters )
  {
    if(!missing(name))
    {
      .Object@name = name
    }
    if(!missing( description ) )
    {
      .Object@description = description
    }
    if(!missing( equations ) )
    {
      .Object@equations = equations
    }
    if(!missing( outcomes ) )
    {
      .Object@outcomes = outcomes
    }
    if(!missing( parameters ) )
    {
      .Object@parameters = parameters
    }
    if(!missing( initialConditions ) )
    {
      .Object@initialConditions = initialConditions
    }
    if(!missing( modelError ) )
    {
      .Object@modelError = modelError
    }
    if(!missing( odeSolverParameters ) )
    {
      .Object@odeSolverParameters = odeSolverParameters
    }
    validObject(.Object)
    return (.Object )
  }
)

# ======================================================================================================
#' Get the equations during infusion.
#'
#' @name getEquationsDuringInfusion
#' @param object An object from the class \linkS4class{Model}.
#' @return A list giving the equations during the infusion.
# ======================================================================================================

setGeneric(
  "getEquationsDuringInfusion",
  function(object) {
    standardGeneric("getEquationsDuringInfusion")
  })

setMethod("getEquationsDuringInfusion",
          "Model",
          function(object) {
            equations = getEquations( object )
            equations = equations$duringInfusion
            return( equations )
          })

# ======================================================================================================
#' Get the equations after infusion.
#'
#' @name getEquationsAfterInfusion
#' @param object An object from the class \linkS4class{Model}.
#' @return A list giving the equations after the infusion.
# ======================================================================================================

setGeneric(
  "getEquationsAfterInfusion",
  function(object) {
    standardGeneric("getEquationsAfterInfusion")
  })

setMethod("getEquationsAfterInfusion",
          "Model",
          function(object) {
            equations = getEquations( object )
            equations = equations$afterInfusion
            return( equations )
          })

# ======================================================================================================
#' Set the equations after infusion.
#'
#' @name setEquationsAfterInfusion
#' @param object An object from the class \linkS4class{Model}.
#' @param equations A list giving the equations after the infusion.
#' @return The model with the updated equations after the infusion.
# ======================================================================================================

setGeneric(
  "setEquationsAfterInfusion",
  function(object, equations) {
    standardGeneric("setEquationsAfterInfusion")
  })

setMethod("setEquationsAfterInfusion",
          "Model",
          function(object,equations) {
            object@equations$afterInfusion = equations
            return( object )
          })

# ======================================================================================================
#' Set the equations during infusion.
#'
#' @name setEquationsDuringInfusion
#' @param object An object from the class \linkS4class{Model}.
#' @param equations A list giving the equations during the infusion.
#' @return The model with the updated equations during the infusion.
# ======================================================================================================

setGeneric(
  "setEquationsDuringInfusion",
  function(object, equations) {
    standardGeneric("setEquationsDuringInfusion")
  })

setMethod("setEquationsDuringInfusion",
          "Model",
          function(object,equations) {
            object@equations$duringInfusion = equations
            return( object )
          })

###########################################################################################
# End class ModelInfusion
###########################################################################################













