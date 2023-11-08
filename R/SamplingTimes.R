#' Class "SamplingTimes"
#'
#' @description The class "SamplingTimes" implements the sampling times.
#'
#' @name SamplingTimes-class
#' @aliases SamplingTimes
#' @docType class
#' @include GenericMethods.R
#' @export
#'
#' @section Objects from the class \code{SamplingTimes}:
#' Objects form the class \code{SamplingTimes} can be created by calls of the form \code{SamplingTimes(...)} where
#' (...) are the parameters for the \code{SamplingTimes} objects.
#'
#' @section Slots for \code{SamplingTimes} objects:
#'  \describe{
#'    \item{\code{outcome}:}{A string giving the outcome.}
#'    \item{\code{samplings}:}{A vector giving the sampling times.}
#'  }

SamplingTimes = setClass(Class="SamplingTimes",
                         representation=representation
                         (
                           outcome = "character",
                           samplings ="vector"
                         ))
# Initialize method
setMethod(
  f="initialize",
  signature="SamplingTimes",
  definition= function (.Object, outcome, samplings )
  {
    if(!missing(outcome))
    {
      .Object@outcome = outcome
    }
    if(!missing(samplings))
    {
      .Object@samplings = samplings
    }
    validObject(.Object)
    return (.Object )
  }
)

# ======================================================================================================
# getOutcome
# ======================================================================================================

setMethod("getOutcome",
          "SamplingTimes",
          function(object)
          {
            return(object@outcome)
          }
)


# ======================================================================================================
# setOutcome
# ======================================================================================================

setMethod("setOutcome",
          "SamplingTimes",
          function(object, outcome)
          {
            object@outcome = outcome
            return(object)
          }
)

# ======================================================================================================
# getSamplings
# ======================================================================================================

setMethod("getSamplings",
          "SamplingTimes",
          function(object)
          {
            return(object@samplings)
          }
)

# ======================================================================================================
#' Set the sampling times.
#'
#' @name setSamplings
#' @param object An object from the class \linkS4class{SamplingTimes}.
#' @param samplings A vector giving the sampling times.
#' @return The updated sampling times.
# ======================================================================================================

setGeneric("setSamplings",
           function(object, samplings)
           {
             standardGeneric("setSamplings")
           }
)

setMethod("setSamplings",
          "SamplingTimes",
          function(object, samplings)
          {
            object@samplings = samplings
            return(object)
          }
)

# ########################################################################################################################
# END Class SamplingTimes
# ########################################################################################################################




