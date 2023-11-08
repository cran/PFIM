#' Class "ModelParameter"
#'
#' @description
#' The class \code{ModelParameter} defines information concerning the model parameters.
#'
#' @name ModelParameter-class
#' @aliases ModelParameter
#' @docType class
#' @include Distribution.R
#' @include GenericMethods.R
#' @export
#'
#' @section Objects from the class:
#' Objects form the class \code{ModelParameter} can be created by calls of the form \code{ModelParameter(...)} where
#' (...) are the parameters for the \code{ModelParameter} objects.
#'
#' @section Slots for \code{ModelParameter} objects:
#'  \describe{
#'    \item{\code{name}:}{A string giving the name of the parameter.}
#'    \item{\code{distribution}:}{An object from the class \code{Distribution} giving the distribution of the parameter.}
#'    \item{\code{fixedMu}:}{A boolean giving if mu is fixed or not.}
#'    \item{\code{fixedOmega}:}{A boolean giving if omega is fixed or not.}
#'  }

ModelParameter = setClass(
  Class = "ModelParameter",
  representation = representation(
    name = "character",
    distribution = "Distribution",
    fixedMu = "logical",
    fixedOmega = "logical"
  ),
  prototype = prototype(
    fixedMu = FALSE,
    fixedOmega = FALSE
  )
)

# Initialize method
setMethod(
  f = "initialize",
  signature = "ModelParameter",
  definition = function( .Object, name, distribution, fixedMu, fixedOmega )
  {
    if(!missing(name))
    {
      .Object@name = name
    }
    if(!missing(distribution))
    {
      .Object@distribution = distribution
    }
    if(!missing(fixedMu))
    {
      .Object@fixedMu = fixedMu
    }
    if(!missing(fixedOmega))
    {
      .Object@fixedOmega = fixedOmega
    }
    validObject(.Object)
    return(.Object)
  }
)

# ======================================================================================================
# getName
# ======================================================================================================

setMethod("getName",
          "ModelParameter",
          function(object)
          {
            name = object@name
            return(name)
          })

# ======================================================================================================
#' Get the distribution.
#'
#' @name getDistribution
#' @param object An object from the class \linkS4class{ModelParameter}.
#' @return The parameter distribution.
# ======================================================================================================

setGeneric(
  "getDistribution",
  function(object) {
    standardGeneric("getDistribution")
  })

setMethod("getDistribution",
          "ModelParameter",
          function(object)
          {
            distribution = object@distribution
            return(distribution)
          })

# ======================================================================================================
#' Set the distribution.
#'
#' @name setDistribution
#' @param object An object from the class \linkS4class{ModelParameter}.
#' @param distribution An object from the class \linkS4class{Distribution}.
#' @return The model parameter with the updated distribution.
# ======================================================================================================

setGeneric(
  "setDistribution",
  function(object,distribution) {
    standardGeneric("setDistribution")
  })

setMethod("setDistribution",
          "ModelParameter",
          function(object,distribution)
          {
            object@distribution = distribution
            return(object)
          })

# ======================================================================================================
#' Get the fixed effect.
#'
#' @name getFixedMu
#' @param object An object from the class \linkS4class{ModelParameter}.
#' @return A boolean giving the fixed mu.
# ======================================================================================================

setGeneric(
  "getFixedMu",
  function(object) {
    standardGeneric("getFixedMu")
  })

setMethod("getFixedMu",
          signature("ModelParameter"),
          function(object)
          {
            return(object@fixedMu)
          })

# ======================================================================================================
#' Set the mu as fixed or not.
#'
#' @name setFixedMu
#' @param object An object from the class \linkS4class{ModelParameter}.
#' @param value A Boolean if fixed or not.
#' @return The mode parameter with the the mu updated as fixed or not.
# ======================================================================================================

setGeneric(
  "setFixedMu",
  function(object,value) {
    standardGeneric("setFixedMu")
  })

setMethod("setFixedMu",
          signature("ModelParameter"),
          function(object,value)
          {
            object@fixedMu = value
            return(object)
          })

# ======================================================================================================
#' Get the fixed variance.
#'
#' @name getFixedOmega
#' @param object An object from the class \linkS4class{ModelParameter}.
#' @return A boolean giving the fixed omega.
# ======================================================================================================

setGeneric(
  "getFixedOmega",
  function(object) {
    standardGeneric("getFixedOmega")
  })

setMethod("getFixedOmega",
          signature("ModelParameter"),
          function(object)
          {
            return(object@fixedOmega)
          })

# ======================================================================================================
#' Set the omega as fixed of not.
#'
#' @name setFixedOmega
#' @param object An object from the class \linkS4class{ModelParameter}.
#' @param value A Boolean fixed or not.
#' @return The model parameter with the omega updated as fixed or not.
# ======================================================================================================

setGeneric(
  "setFixedOmega",
  function(object,value) {
    standardGeneric("setFixedOmega")
  })

setMethod("setFixedOmega",
          signature("ModelParameter"),
          function(object,value)
          {
            object@fixedOmega = value
            return(object)
          })

# ======================================================================================================
# getMu
# ======================================================================================================

setMethod("getMu",
          "ModelParameter",
          function(object)
          {
            distribution = getDistribution( object )
            parameters = getParameters( distribution )
            mu = parameters$mu
            return(mu)
          })

# ======================================================================================================
# setMu
# ======================================================================================================

setMethod("setMu",
          "ModelParameter",
          function(object,value)
          {
            distribution = getDistribution( object )
            distribution = setMu( distribution, value)
            object = setDistribution( object, distribution )
            return( object )
          })

# ======================================================================================================
# getOmega
# ======================================================================================================

setMethod("getOmega",
          "ModelParameter",
          function(object)
          {
            distribution = getDistribution( object )
            parameters = getParameters( distribution )
            omega = parameters$omega
            return(omega)
          })

# ======================================================================================================
# setOmega
# ======================================================================================================

setMethod("setOmega",
          "ModelParameter",
          function(object,value)
          {
            distribution = getDistribution( object )
            distribution = setOmega( distribution, value )
            object = setDistribution( object, distribution )
            return( object )
          })

###########################################################################################
# End class ModelParameter
###########################################################################################









