#' Class "Normal"
#'
#' @description The class defines all the required methods for a Normal distribution object.
#' The class \code{Normal} inherits from the class \code{Distribution}.
#'
#' @name Normal-class
#' @aliases Normal
#' @docType class
#' @include Distribution.R
#' @export

Normal = setClass( Class = "Normal",
                     contains = "Distribution",
                     representation = representation())

setMethod(
  f = "initialize",
  signature = "Normal",
  definition = function( .Object, ... )
  {
    parameters = list(...)

    if(!missing(parameters))
    {
      .Object@parameters = list(mu = parameters$mu,
                                omega = parameters$omega)
    }

    validObject(.Object)
    return (.Object )
  })

# ======================================================================================================
# getAdjustedGradient
# ======================================================================================================

setMethod("getAdjustedGradient",
          "Normal",
          function( object, outcomesGradient )
          {

            adjustedGradient = getMu( object )

            return( adjustedGradient )
          }
)

##############################################################################
# END Class Normal
##############################################################################


