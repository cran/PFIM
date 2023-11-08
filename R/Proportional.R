#' Class "Proportional"
#'
#' @description The Class "Proportional" defines the the residual error variance according
#' to the formula  g(sigma_inter, sigma_slope, c_error, f(x, theta)) =  sigma_slope*f(x,theta).
#'
#' @name Proportional-class
#' @aliases Proportional
#' @docType class
#' @include Combined1.R
#' @export
#'
#' @section Objects from the Class \linkS4class{Proportional}:
#' Objects are typically created by calls to \code{Proportional} and contain the following slots
#' that are inherited from the class \linkS4class{Combined1}:
#'
#' @section Slots for the \code{Proportional} objects:
#' \describe{
#' \item{\code{.Object}:}{An object of the Class \code{Proportional}}
#' \item{\code{sigma_inter}:}{A numeric value giving the sigma inter of the error model}
#' \item{\code{sigma_slope}:}{A numeric value giving the sigma slope of the error model}
#' }

Proportional = setClass(Class = "Proportional",
                        contains = "ModelError",
                        representation = representation
                        (
                          outcome = "character",
                          equation = "expression",
                          derivatives = "list",
                          sigmaInter = "numeric",
                          sigmaSlope = "numeric",
                          cError = "numeric"
                        ),
                        prototype = prototype( cError = 1,
                                               sigmaInter = 0,
                                               equation =  parse(text = "sigmaSlope")
                        ))

setMethod(
  f="initialize",
  signature="Proportional",
  definition= function (.Object, outcome, equation, derivatives, sigmaInter, sigmaSlope, cError )
  {
    if(!missing(outcome))
    {
      .Object@outcome = outcome
    }
    if(!missing(equation))
    {
      .Object@equation = equation
    }
    if(!missing(derivatives))
    {
      .Object@derivatives = derivatives
    }
    if(!missing(sigmaInter))
    {
      .Object@sigmaInter = sigmaInter
    }
    if(!missing(sigmaSlope))
    {
      .Object@sigmaSlope = sigmaSlope
    }
    if( !missing( cError ) )
    {
      .Object@cError = cError
    }

    validObject(.Object)
    return (.Object )
  }
)

##########################################################################################################
# End class Proportional
##########################################################################################################














