#' Class "Distribution"
#'
#' @description The class defines all the required methods for a distribution object.
#'
#' @name Distribution-class
#' @aliases Distribution
#' @docType class
#' @include GenericMethods.R
#' @export
#'
#' @section Objects from the class:
#' Objects form the class \code{Distribution} can be created by calls of the form \code{Distribution(...)} where
#' (...) are the parameters for the \code{Distribution} objects.
#'
#' @section Slots for \code{Distribution} objects:
#'  \describe{
#'    \item{\code{parameters}:}{A list containing the distribution parameters.}
#'  }

Distribution = setClass("Distribution",
                        representation  =  representation(
                          parameters = "list"
                        ))

#' initialize
#' @param .Object .Object
#' @param parameters parameters
#' @return Distribution
#' @export
#'
setMethod(f="initialize",
          signature="Distribution",
          definition= function (.Object, parameters)
          {
            if(!missing(parameters))
            {
              .Object@parameters = parameters
            }
            validObject(.Object)
            return (.Object )
          }
)

# ======================================================================================================
# getParameters
# ======================================================================================================

#' @rdname getParameters
#' @export

setMethod("getParameters",
          "Distribution",
          function(object)
          {
            parameters = object@parameters
            return(parameters)
          })

# ======================================================================================================
# setParameters
# ======================================================================================================

#' @rdname setParameters
#' @export

setMethod("setParameters",
          "Distribution",
          function(object,parameters)
          {
            object@parameters = parameters
            return(object)
          })

# ======================================================================================================
# getMu
# ======================================================================================================

#' @rdname getMu
#' @export

setMethod("getMu",
          "Distribution",
          function(object)
          {
            parameters = getParameters(object)
            mu = parameters$mu
            return(mu)
          })

# ======================================================================================================
# setMu
# ======================================================================================================

#' @rdname setMu
#' @export

setMethod("setMu",
          "Distribution",
          function(object,value)
          {
            parameters = getParameters( object)
            parameters$mu = value
            object = setParameters( object, parameters )
            return( object )
          })

# ======================================================================================================
# getOmega
# ======================================================================================================

#' @rdname getOmega
#' @export

setMethod("getOmega",
          "Distribution",
          function(object)
          {
            parameters = getParameters(object)
            omega = parameters$omega
            return(omega)
          })

# ======================================================================================================
# setOmega
# ======================================================================================================

#' @rdname setOmega
#' @export

setMethod("setOmega",
          "Distribution",
          function(object,value)
          {
            parameters = getParameters( object)
            parameters$omega = value
            object = setParameters( object, parameters )

            return( object )
          })

#' Get the adjusted gradient.
#'
#' @title getAdjustedGradient
#' @param object An object \code{distribution} from the class \linkS4class{Distribution}.
#' @param outcomesGradient A list containing the evaluation of the outcome gradients.
#' @return A list giving the adjusted gradient.
#' @export

setGeneric("getAdjustedGradient",
           function( object, outcomesGradient )
           {
             standardGeneric("getAdjustedGradient")
           })

###########################################################################################
# End class Distribution
###########################################################################################







