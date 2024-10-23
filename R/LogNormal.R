#' Class "LogNormal"
#'
#' @description The class defines all the required methods for a LogNormal distribution object.
#' The class \code{LogNormal} inherits from the class \code{Distribution}.
#'
#' @name LogNormal-class
#' @aliases LogNormal
#' @docType class
#' @include Distribution.R
#' @export

LogNormal = setClass( Class = "LogNormal",
                      contains = "Distribution",
                      representation = representation())

#' initialize
#' @param .Object .Object
#' @param ... args
#' @return LogNormal
#' @export

setMethod( f = "initialize",
           signature = "LogNormal",
           definition = function( .Object, ... )
           {
             parameters = list(...)

             if( !missing( parameters ) )
             {
               .Object@parameters = list( mu = parameters$mu,
                                          omega = parameters$omega)
             }

             validObject(.Object)
             return (.Object )
           })

# ======================================================================================================
# getAdjustedGradient
# ======================================================================================================

#' @rdname getAdjustedGradient
#' @export

setMethod("getAdjustedGradient",
          "LogNormal",
          function( object, outcomesGradient )
          {

            adjustedGradient = getMu( object ) * outcomesGradient

            return( adjustedGradient )
          }
)

###########################################################################################
# End class LogNormal
###########################################################################################



